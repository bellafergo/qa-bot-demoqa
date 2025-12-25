// src/App.jsx
import React, {
  useEffect,
  useRef,
  useState,
  useCallback,
  useMemo,
} from "react";
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

// ---- API guardrails (evita "X is not a function")
const safeFn = (fn, name) => {
  if (typeof fn !== "function") {
    throw new Error(`API function "${name}" no es una función`);
  }
  return fn;
};

const safeListThreads = safeFn(listThreads, "listThreads");
const safeGetThread = safeFn(getThread, "getThread");
const safeChatRun = safeFn(chatRun, "chatRun");
const safeCreateThread = safeFn(apiCreateThread, "createThread");
const safeDeleteThread = safeFn(apiDeleteThread, "deleteThread");

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
  if (typeof text !== "string") return "";
  const safe = escapeHtml(text);
  return safe.replace(/\*\*(.*?)\*\*/g, "<b>$1</b>").replace(/\n/g, "<br/>");
};

const shortId = (id) => (id ? `${String(id).slice(0, 8)}…` : "");

/**
 * ErrorBoundary (evita “pantalla negra”)
 */
class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, err: null };
  }
  static getDerivedStateFromError(error) {
    return { hasError: true, err: error };
  }
  componentDidCatch(error, info) {
    // log útil
    // eslint-disable-next-line no-console
    console.error("UI crashed:", error, info);
  }
  render() {
    if (this.state.hasError) {
      const msg = String(this.state.err?.message || this.state.err || "Error");
      return (
        <div style={{ padding: 18, color: "white" }}>
          <div
            style={{
              padding: "10px 12px",
              borderRadius: 12,
              background: "rgba(255,0,0,0.10)",
              border: "1px solid rgba(255,0,0,0.25)",
              maxWidth: 900,
            }}
          >
            <div style={{ fontWeight: 900, marginBottom: 6 }}>❗ UI Crashed</div>
            <div style={{ opacity: 0.9, whiteSpace: "pre-wrap" }}>{msg}</div>
            <div style={{ marginTop: 10, fontSize: 12, opacity: 0.75 }}>
              Abre DevTools → Console para ver el stack. Esto evita que se quede en negro.
            </div>
          </div>
        </div>
      );
    }
    return this.props.children;
  }
}

export default function App() {
  // -----------------------------
  // UI state
  // -----------------------------
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState("");
  const [uiError, setUiError] = useState("");

  // Captura errores globales (unhandledrejection/JS runtime)
  const [fatalUiError, setFatalUiError] = useState("");

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

  const scrollToBottom = useCallback(() => {
    // si el componente está montado
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, []);

  useEffect(() => {
    scrollToBottom();
  }, [messages, scrollToBottom]);

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
      const list = await safeListThreads();
      const normalized = normalizeThreads(list || []);
      setThreads(normalized);
      return normalized;
    } catch (e) {
      setUiError(
        `No pude cargar el historial (/threads). Detalle: ${String(
          e?.message || e
        )}`
      );
      setThreads([]);
      return [];
    } finally {
      setIsThreadsLoading(false);
    }
  }, [normalizeThreads]);

  // -----------------------------
  // Map backend thread -> UI messages
  // (IMPORTANTE: no descartes mensajes que no tengan content pero sí meta.runner)
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

      // meta (tu backend ya lo manda como "meta" o "meta_json")
      const meta = m?.meta ?? m?.meta_json ?? {};

      // Si no hay content pero sí hay runner/doc en meta, lo mantenemos para que Chat renderice evidencia
      const hasRenderableMeta =
        !!meta?.runner || !!meta?.docArtifacts || !!meta?.artifacts || !!meta?.mode;

      if (!content && !hasRenderableMeta) continue;

      ui.push({ role, content, meta });
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
        const data = await safeGetThread(tid);

        const backendMsgs =
          data?.messages ||
          data?.items ||
          data?.history ||
          data?.thread?.messages;

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
      const data = await safeCreateThread();
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
        await safeDeleteThread(tid);
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
    setMessages((prev) => [...prev, { role: "user", content: prompt, meta: {} }]);
    setInput("");

    try {
      const resp = await safeChatRun(prompt, threadId || null, {
        session_id: sessionId,
      });

      const newThreadId = resp?.thread_id || resp?.threadId || threadId || null;
      const newSessionId =
        resp?.session_id || resp?.sessionId || sessionId || null;

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

      setMessages((prev) => [
        ...prev,
        {
          role: "bot",
          content: answer || "Listo. (Respuesta sin texto, revisa el payload)",
          meta: resp || {},
        },
      ]);

      await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`Error al enviar. Detalle: ${String(e?.message || e)}`);
      setMessages((prev) => [
        ...prev,
        {
          role: "bot",
          content: "❗ Hubo un error enviando tu mensaje. Reintenta.",
          meta: { mode: "error" },
        },
      ]);
    } finally {
      setIsSending(false);
    }
  }, [input, isSending, refreshThreads, sessionId, threadId]);

  // -----------------------------
  // Global error listeners (evita “pantalla negra”)
  // -----------------------------
  useEffect(() => {
    const onErr = (event) => {
      const msg =
        event?.error?.stack ||
        event?.error?.message ||
        event?.message ||
        String(event || "Unknown error");
      setFatalUiError(msg);
      // eslint-disable-next-line no-console
      console.error("window.onerror:", event);
    };
    const onRej = (event) => {
      const reason = event?.reason;
      const msg =
        reason?.stack || reason?.message || String(reason || "Unhandled rejection");
      setFatalUiError(msg);
      // eslint-disable-next-line no-console
      console.error("unhandledrejection:", event);
    };
    window.addEventListener("error", onErr);
    window.addEventListener("unhandledrejection", onRej);
    return () => {
      window.removeEventListener("error", onErr);
      window.removeEventListener("unhandledrejection", onRej);
    };
  }, []);

  // -----------------------------
  // Initial load (estable)
  // -----------------------------
  useEffect(() => {
    (async () => {
      await refreshThreads().catch(() => {});
      if (threadId) {
        await loadThread(threadId).catch(() => setWelcome());
      } else {
        setWelcome();
      }
    })();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // -----------------------------
  // UI memo: error badge
  // -----------------------------
  const errorBadge = useMemo(() => {
    const msg = fatalUiError || uiError;
    if (!msg) return null;
    return (
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
          color: "white",
        }}
        title={msg}
      >
        ❗ {msg}
      </div>
    );
  }, [fatalUiError, uiError]);

  // -----------------------------
  // Layout
  // -----------------------------
  return (
    <ErrorBoundary>
      <div style={{ display: "flex", height: "100vh", width: "100%" }}>
        {/* Sidebar */}
        <div
          style={{
            width: isSidebarOpen ? 320 : 0,
            overflow: "hidden",
            transition: "width 200ms ease",
            borderRight: isSidebarOpen
              ? "1px solid rgba(255,255,255,0.08)"
              : "none",
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
        <div
          style={{
            flex: 1,
            display: "flex",
            flexDirection: "column",
            minWidth: 0,
          }}
        >
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

              <div style={{ fontWeight: 800, fontSize: 18, color: "white" }}>
                Vanya{" "}
                <span style={{ fontWeight: 400, opacity: 0.7, fontSize: 12 }}>
                  | QA Intelligence Agent{" "}
                  {threadId ? `— thread: ${shortId(threadId)}` : ""}
                </span>
              </div>
            </div>

            {errorBadge}
          </div>

          {/* Chat */}
          <div style={{ flex: 1, overflow: "auto" }}>
            <Chat
              key={threadId || "no-thread"}
              messages={Array.isArray(messages) ? messages : []}
              input={input}
              setInput={typeof setInput === "function" ? setInput : () => {}}
              handleSend={typeof handleSend === "function" ? handleSend : () => {}}
              isLoading={!!isSending}
              sessionId={sessionId}
              threadId={threadId}
              formatText={typeof formatText === "function" ? formatText : (x) => x}
              chatEndRef={chatEndRef}
            />
            {/* NOTA: NO duplicamos chatEndRef aquí porque Chat ya lo usa */}
          </div>
        </div>
      </div>
    </ErrorBoundary>
  );
}