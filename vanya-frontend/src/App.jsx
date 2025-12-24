// src/App.jsx
import React, { useEffect, useRef, useState, useCallback, useMemo } from "react";
import "./App.css";
import {
  chatRun,
  getThread,
  listThreads,
  createThread as apiCreateThread,
  deleteThread as apiDeleteThread,
} from "./api";
import Sidebar from "./components/Sidebar";
import Chat from "./components/Chat";

/**
 * Helpers (frontend)
 */
const escapeHtml = (s) => {
  if (s == null) return "";
  return String(s)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
};

const formatText = (text) => {
  if (!text) return "";
  const safe = escapeHtml(text);
  return safe.replace(/\*\*(.*?)\*\*/g, "<b>$1</b>").replace(/\n/g, "<br/>");
};

const shortId = (id) => (id ? `${String(id).slice(0, 8)}…` : "");

export default function App() {
  // -----------------------------
  // UI state
  // -----------------------------
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState("");
  const [uiError, setUiError] = useState("");

  // Busy flags
  const [isSending, setIsSending] = useState(false);
  const [isThreadsLoading, setIsThreadsLoading] = useState(false);
  const sidebarBusy = isSending || isThreadsLoading;

  // -----------------------------
  // Session + Thread state
  // -----------------------------
  const [sessionId, setSessionId] = useState(() => {
    try {
      return localStorage.getItem("vanya_session_id") || null;
    } catch {
      return null;
    }
  });

  const [threadId, setThreadId] = useState(() => {
    try {
      return localStorage.getItem("vanya_thread_id") || null;
    } catch {
      return null;
    }
  });

  const [threads, setThreads] = useState([]);
  const [isSidebarOpen, setIsSidebarOpen] = useState(true);

  const chatEndRef = useRef(null);

  const scrollToBottom = () => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(scrollToBottom, [messages]);

  const setWelcome = useCallback(() => {
    setMessages([
      {
        role: "bot",
        content:
          "Hola, soy **Vanya**, tu Agente de QA inteligente. ¿En qué puedo ayudarte hoy con tus pruebas?",
        meta: { mode: "welcome" },
      },
    ]);
  }, []);

  // -----------------------------
  // Threads helpers
  // -----------------------------
  const normalizeThreads = useCallback((list) => {
    const arr = Array.isArray(list) ? list : [];
    const cleaned = arr
      .map((t) => ({
        ...t,
        id: t?.id || t?.thread_id || null,
      }))
      .filter((t) => !!t.id);

    cleaned.sort((a, b) => {
      const da = a?.updated_at ? new Date(a.updated_at).getTime() : 0;
      const db = b?.updated_at ? new Date(b.updated_at).getTime() : 0;
      return db - da;
    });

    return cleaned;
  }, []);

  const refreshThreads = useCallback(async () => {
    setIsThreadsLoading(true);
    setUiError("");
    try {
      const list = await listThreads();
      const normalized = normalizeThreads(list || []);
      setThreads(normalized);
      return normalized;
    } catch (e) {
      setUiError(
        `No pude cargar el historial (/threads). Detalle: ${String(e?.message || e)}`
      );
      setThreads([]);
      return [];
    } finally {
      setIsThreadsLoading(false);
    }
  }, [normalizeThreads]);

  // -----------------------------
  // Map backend thread -> UI messages
  // -----------------------------
  const mapBackendMessagesToUI = useCallback((items) => {
    if (!Array.isArray(items) || items.length === 0) return null;

    const ui = [];
    for (const m of items) {
      const roleRaw = String(m?.role || m?.speaker || "").toLowerCase();
      if (roleRaw === "system") continue;

      const role =
        roleRaw === "assistant" || roleRaw === "bot" ? "bot" : "user";

      const content =
        typeof m?.content === "string"
          ? m.content
          : typeof m?.text === "string"
          ? m.text
          : m?.message && typeof m.message === "string"
          ? m.message
          : "";

      if (!content) continue;

      ui.push({
        role,
        content,
        meta: m?.meta || {},
      });
    }

    return ui.length ? ui : null;
  }, []);

  // -----------------------------
  // Load one thread
  // -----------------------------
  const loadThread = useCallback(
    async (id, { refreshSidebar = false } = {}) => {
      const tid = String(id || "").trim();
      if (!tid) return;

      setUiError("");
      setThreadId(tid);
      try {
        localStorage.setItem("vanya_thread_id", tid);
      } catch {}

      setIsThreadsLoading(true);
      try {
        const data = await getThread(tid);
        // Esperado: { id, title, messages: [...] } o parecido
        const backendMsgs =
          data?.messages || data?.items || data?.history || data?.thread?.messages;

        const ui = mapBackendMessagesToUI(backendMsgs);

        if (ui) setMessages(ui);
        else setWelcome();

        if (refreshSidebar) await refreshThreads().catch(() => {});
      } catch (e) {
        setUiError(
          `No pude cargar el chat (/threads/${shortId(tid)}). Detalle: ${String(
            e?.message || e
          )}`
        );
        setWelcome();
      } finally {
        setIsThreadsLoading(false);
      }
    },
    [mapBackendMessagesToUI, refreshThreads, setWelcome]
  );

  // -----------------------------
  // Create thread
  // -----------------------------
  const handleNew = useCallback(async () => {
    if (sidebarBusy) return;
    setUiError("");
    setIsThreadsLoading(true);
    try {
      const data = await apiCreateThread();
      const id = data?.id || data?.thread_id;
      if (!id) throw new Error("Backend no devolvió id/thread_id al crear thread.");

      setThreadId(id);
      try {
        localStorage.setItem("vanya_thread_id", id);
      } catch {}

      setWelcome();
      await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`No pude crear chat. Detalle: ${String(e?.message || e)}`);
    } finally {
      setIsThreadsLoading(false);
    }
  }, [refreshThreads, setWelcome, sidebarBusy]);

  // -----------------------------
  // Delete thread
  // -----------------------------
  const handleDelete = useCallback(
    async (id) => {
      if (sidebarBusy) return;
      const tid = String(id || "").trim();
      if (!tid) return;

      const ok = window.confirm("¿Borrar este chat? Esto no se puede deshacer.");
      if (!ok) return;

      setUiError("");
      setIsThreadsLoading(true);
      try {
        await apiDeleteThread(tid);
        await refreshThreads();

        if (String(tid) === String(threadId)) {
          setThreadId(null);
          try {
            localStorage.removeItem("vanya_thread_id");
          } catch {}
          setWelcome();
        }
      } catch (e) {
        setUiError(`No pude borrar el chat. Detalle: ${String(e?.message || e)}`);
      } finally {
        setIsThreadsLoading(false);
      }
    },
    [refreshThreads, setWelcome, sidebarBusy, threadId]
  );

  // -----------------------------
  // Select thread (from sidebar)
  // -----------------------------
  const handleSelect = useCallback(
    async (id) => {
      if (sidebarBusy) return;
      await loadThread(id, { refreshSidebar: false });
    },
    [loadThread, sidebarBusy]
  );

  // -----------------------------
  // Send chat
  // -----------------------------
  const handleSend = useCallback(async () => {
    const prompt = (input || "").trim();
    if (!prompt || isSending) return;

    setUiError("");
    setIsSending(true);

    // UI optimistic
    setMessages((prev) => [...prev, { role: "user", content: prompt }]);
    setInput("");

    try {
      const resp = await chatRun(prompt, threadId || null, { session_id: sessionId });

      // Resp esperada: { answer, thread_id, session_id, ... }
      const newThreadId = resp?.thread_id || resp?.threadId || threadId || null;
      const newSessionId = resp?.session_id || resp?.sessionId || sessionId || null;

      if (newThreadId && String(newThreadId) !== String(threadId)) {
        setThreadId(newThreadId);
        try {
          localStorage.setItem("vanya_thread_id", newThreadId);
        } catch {}
      }

      if (newSessionId && String(newSessionId) !== String(sessionId)) {
        setSessionId(newSessionId);
        try {
          localStorage.setItem("vanya_session_id", newSessionId);
        } catch {}
      }

      const answer =
        typeof resp?.answer === "string"
          ? resp.answer
          : typeof resp?.text === "string"
          ? resp.text
          : "";

      if (answer) {
        setMessages((prev) => [...prev, { role: "bot", content: answer, meta: resp }]);
      } else {
        // Si no viene "answer" pero sí viene estructura, al menos mostramos algo
        setMessages((prev) => [
          ...prev,
          { role: "bot", content: "Listo. (Respuesta sin texto, revisa el payload)", meta: resp },
        ]);
      }

      // refresca sidebar (título/updated_at)
      await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`Error al enviar. Detalle: ${String(e?.message || e)}`);
      setMessages((prev) => [
        ...prev,
        { role: "bot", content: "❗ Hubo un error enviando tu mensaje. Reintenta.", meta: { mode: "error" } },
      ]);
    } finally {
      setIsSending(false);
    }
  }, [input, isSending, refreshThreads, sessionId, threadId]);

  // -----------------------------
  // Initial load
  // -----------------------------
  useEffect(() => {
    refreshThreads().catch(() => {});
  }, [refreshThreads]);

  useEffect(() => {
    // si hay thread guardado, lo cargamos
    if (threadId) {
      loadThread(threadId).catch(() => setWelcome());
    } else {
      setWelcome();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // -----------------------------
  // Layout
  // -----------------------------
  return (
    <div style={{ display: "flex", height: "100vh", width: "100%" }}>
      {/* Sidebar */}
      <div
        style={{
          width: isSidebarOpen ? 320 : 0,
          overflow: "hidden",
          transition: "width 200ms ease",
          borderRight: isSidebarOpen ? "1px solid rgba(255,255,255,0.08)" : "none",
        }}
      >
        {isSidebarOpen && (
          <Sidebar
            threads={threads}
            activeId={threadId}
            onNew={handleNew}
            onSelect={handleSelect}
            onDelete={handleDelete}
            isLoading={sidebarBusy}
          />
        )}
      </div>

      {/* Main */}
      <div style={{ flex: 1, display: "flex", flexDirection: "column", minWidth: 0 }}>
        {/* Top bar */}
        <div
          style={{
            height: 60,
            padding: "10px 16px",
            borderBottom: "1px solid rgba(255,255,255,0.08)",
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            gap: 10,
          }}
        >
          <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
            <button
              onClick={() => setIsSidebarOpen((s) => !s)}
              style={{
                width: 36,
                height: 36,
                borderRadius: 10,
                border: "1px solid rgba(255,255,255,0.12)",
                background: "rgba(0,0,0,0.25)",
                color: "white",
                cursor: "pointer",
              }}
              title={isSidebarOpen ? "Ocultar historial" : "Mostrar historial"}
            >
              {isSidebarOpen ? "⟪" : "⟫"}
            </button>

            <div style={{ fontWeight: 800, fontSize: 18 }}>
              Vanya{" "}
              <span style={{ fontWeight: 400, opacity: 0.7, fontSize: 12 }}>
                | QA Intelligence Agent {threadId ? `— thread: ${shortId(threadId)}` : ""}
              </span>
            </div>
          </div>

          {uiError ? (
            <div
              style={{
                padding: "6px 10px",
                borderRadius: 10,
                background: "rgba(255,0,0,0.10)",
                border: "1px solid rgba(255,0,0,0.25)",
                fontSize: 12,
                maxWidth: 520,
                overflow: "hidden",
                textOverflow: "ellipsis",
                whiteSpace: "nowrap",
              }}
              title={uiError}
            >
              ❗ {uiError}
            </div>
          ) : null}
        </div>

        {/* Chat */}
        <div style={{ flex: 1, overflow: "auto" }}>
          {/* IMPORTANTÍSIMO: key={threadId} para que al seleccionar otro chat sí cambie */}
          <Chat
            key={threadId || "no-thread"}
            messages={messages}
            input={input}
            setInput={setInput}
            handleSend={handleSend}
            isLoading={isSending}
            sessionId={sessionId}
            threadId={threadId}
            formatText={formatText}
            chatEndRef={chatEndRef}
          />
          <div ref={chatEndRef} />
        </div>
      </div>
    </div>
  );
}