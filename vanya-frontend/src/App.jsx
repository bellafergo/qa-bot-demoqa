import React, { useEffect, useRef, useState } from "react";
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

function App() {
  // -----------------------------
  // Chat UI state
  // -----------------------------
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState("");
  const [isLoading, setIsLoading] = useState(false); // sending
  const [isThreadsLoading, setIsThreadsLoading] = useState(false); // sidebar load/list
  const [uiError, setUiError] = useState("");

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

  const setWelcome = () => {
    setMessages([
      {
        role: "bot",
        content:
          "Hola, soy **Vanya**, tu Agente de QA inteligente. ¿En qué puedo ayudarte hoy con tus pruebas?",
        meta: { mode: "welcome" },
      },
    ]);
  };

  // -----------------------------
  // Threads
  // -----------------------------
  const normalizeThreads = (list) => {
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
  };

  const refreshThreads = async () => {
    setIsThreadsLoading(true);
    try {
      const list = await listThreads();
      const normalized = normalizeThreads(list || []);
      setThreads(normalized);
      return normalized;
    } catch (e) {
      setUiError(
        `No pude cargar el historial (/threads). Detalle: ${String(
          e?.message || e
        )}`
      );
      return [];
    } finally {
      setIsThreadsLoading(false);
    }
  };

  const mapBackendMessagesToUI = (items) => {
    if (!Array.isArray(items) || items.length === 0) return null;

    const ui = [];
    for (const m of items) {
      const role = String(m?.role || "").toLowerCase();
      if (role === "system") continue;

      if (role === "assistant") {
        ui.push({
          role: "bot",
          content: String(m?.content || ""),
          meta: m?.meta || {},
          runner: m?.runner || null,
          docArtifacts: m?.doc_artifacts || null,
        });
      } else {
        ui.push({
          role: "user",
          content: String(m?.content || ""),
        });
      }
    }
    return ui.length ? ui : null;
  };

  const loadThread = async (id, opts = { refreshSidebar: true }) => {
    if (!id) throw new Error("threadId inválido");

    setIsThreadsLoading(true);
    setUiError("");

    try {
      const data = await getThread(id);

      const items = data?.messages || data?.thread?.messages || [];
      const uiMessages = mapBackendMessagesToUI(items);

      setThreadId(id);
      try {
        localStorage.setItem("vanya_thread_id", id);
      } catch {}

      if (!uiMessages || uiMessages.length === 0) setWelcome();
      else setMessages(uiMessages);

      if (opts?.refreshSidebar) await refreshThreads();
      return data;
    } finally {
      setIsThreadsLoading(false);
    }
  };

  const createThread = async () => {
    setIsThreadsLoading(true);
    setUiError("");

    try {
      const data = await apiCreateThread();
      const id = data?.id || data?.thread_id;
      if (!id) throw new Error("No recibí thread_id");

      setThreadId(id);
      try {
        localStorage.setItem("vanya_thread_id", id);
      } catch {}

      setWelcome();

      // refresca sidebar
      await refreshThreads();
      return id;
    } catch (e) {
      setUiError(
        `No pude crear un chat nuevo (POST /threads). Detalle: ${String(
          e?.message || e
        )}`
      );
      throw e;
    } finally {
      setIsThreadsLoading(false);
    }
  };

  // Init: cargar sidebar + cargar thread guardado si existe
  useEffect(() => {
    refreshThreads();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  useEffect(() => {
    if (threadId) {
      loadThread(threadId, { refreshSidebar: false }).catch(() => setWelcome());
    } else {
      setWelcome();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // -----------------------------
  // Main send (POST /chat_run)
  // -----------------------------
  const addMessage = (msg) => setMessages((prev) => [...prev, msg]);

  const addBotMessage = (content, meta = {}, runner = null, docArtifacts = null) =>
    addMessage({ role: "bot", content, meta, runner, docArtifacts });

  const ensureThreadId = async () => {
    if (threadId) return threadId;
    return await createThread();
  };

  const handleSend = async () => {
    const text = input.trim();
    if (!text || isLoading) return;

    setUiError("");

    addMessage({
      id: crypto?.randomUUID?.() || `optimistic-${Date.now()}`,
      role: "user",
      content: text,
    });

    setInput("");
    setIsLoading(true);

    try {
      const activeThreadId = await ensureThreadId();

      const data = await chatRun(text, activeThreadId, { session_id: sessionId });

      if (data?.session_id) {
        setSessionId(data.session_id);
        try {
          localStorage.setItem("vanya_session_id", data.session_id);
        } catch {}
      }

      const mode = String(data?.mode || "").toLowerCase();

      if (mode === "execute") {
        // ✅ Importante: NO hacemos loadThread aquí, porque backend aún no persiste runner/screenshot
        // y al recargar perderíamos la evidencia en UI.
        addBotMessage(
          `✅ Ejecuté la prueba${data.scope ? ` (${data.scope})` : ""}.`,
          { mode: "execute" },
          data.run_result || null,
          null
        );
      } else if (mode === "doc") {
        addBotMessage(
          data.answer || "Generé artefactos QA.",
          { mode: "doc" },
          null,
          data.doc_artifacts || null
        );
      } else if (mode === "need_info") {
        addBotMessage(
          data.answer || "Me falta información para ejecutar.",
          { mode: "need_info" }
        );
      } else if (mode === "advise" || mode === "info" || mode === "plan") {
        addBotMessage(data.answer || "Ok. ¿Qué quieres validar?", { mode: "advise" });
      } else {
        addBotMessage(data.answer || "Ok.", { mode });
      }

      // ✅ Solo refrescamos sidebar para traer el título/updated_at nuevos
      await refreshThreads().catch(() => {});
    } catch (error) {
      console.error("Error en la petición:", error);
      addBotMessage(
        "❌ **Error de conexión con Vanya.**\n\n" +
          "El servidor no respondió correctamente.\n\n" +
          `Detalle: ${String(error?.message || error)}`,
        { mode: "error" }
      );
      setUiError(String(error?.message || error));
    } finally {
      setIsLoading(false);
    }
  };

  // -----------------------------
  // Sidebar actions
  // -----------------------------
  const handleNew = async () => {
    try {
      await createThread();
    } catch {}
  };

  const handleSelect = async (id) => {
    try {
      await loadThread(id, { refreshSidebar: false });
    } catch (e) {
      setUiError(String(e?.message || e));
    }
  };

  const handleDelete = async (id) => {
    const ok = window.confirm(`¿Eliminar este chat (${shortId(id)})?`);
    if (!ok) return;

    setUiError("");

    // UI optimista
    setThreads((prev) => prev.filter((t) => String(t.id) !== String(id)));

    try {
      await apiDeleteThread(id);

      if (String(threadId) === String(id)) {
        try {
          localStorage.removeItem("vanya_thread_id");
        } catch {}
        setThreadId(null);
        setWelcome();
        await createThread();
      }

      await refreshThreads();
    } catch (e) {
      setUiError(`No se pudo eliminar. Detalle: ${String(e?.message || e)}`);
      await refreshThreads().catch(() => {});
    }
  };

  const sidebarBusy = isLoading || isThreadsLoading;

  // -----------------------------
  // Renderers para Chat.jsx
  // -----------------------------
  const prettyStatus = (s) => {
    const v = String(s || "").toLowerCase();
    if (v === "passed" || v === "pass") return "PASSED";
    if (v === "failed" || v === "fail") return "FAIL";
    return (s || "").toString();
  };

  const renderRunnerReport = (runner) => {
    if (!runner) return null;

    const status = prettyStatus(runner.status);
    const isPass = status === "PASSED";
    const evidence = runner.evidence_id ? `EV-${String(runner.evidence_id).replace(/^EV-/, "")}` : null;

    return (
      <div
        style={{
          marginTop: 10,
          padding: 12,
          borderRadius: 14,
          background: "rgba(255,255,255,0.04)",
          border: "1px solid rgba(255,255,255,0.10)",
        }}
      >
        <div style={{ fontWeight: 800, marginBottom: 6 }}>
          {isPass ? "✅ Prueba ejecutada" : "❌ Prueba ejecutada"}: {status}
          {evidence ? (
            <span style={{ fontWeight: 400, opacity: 0.75 }}> (evidence: {evidence})</span>
          ) : null}
        </div>

        {runner.error ? (
          <div style={{ marginTop: 6, opacity: 0.9 }}>
            <b>Detalle:</b> {String(runner.error)}
          </div>
        ) : null}

        {/* ✅ Screenshot */}
        {runner.screenshot_b64 ? (
          <div style={{ marginTop: 10 }}>
            <div style={{ fontWeight: 700, marginBottom: 6, opacity: 0.9 }}>
              🖼️ Evidencia
            </div>
            <img
              src={`data:image/png;base64,${runner.screenshot_b64}`}
              alt="Evidencia"
              style={{
                width: "100%",
                maxWidth: 900,
                borderRadius: 12,
                border: "1px solid rgba(255,255,255,0.12)",
                display: "block",
              }}
              onClick={() => {
                const newTab = window.open();
                if (!newTab) return;
                newTab.document.write(
                  `<img src="data:image/png;base64,${runner.screenshot_b64}" style="width:100%">`
                );
              }}
              title="Click para ampliar"
            />
            <div style={{ fontSize: 11, opacity: 0.6, marginTop: 6 }}>
              Click para ampliar
            </div>
          </div>
        ) : (
          <div style={{ marginTop: 10, fontSize: 12, opacity: 0.7 }}>
            (Sin screenshot. Si lo necesitas, aumenta timeout o ajusta el runner para capturar siempre.)
          </div>
        )}

        {/* Steps (si vienen) */}
        {Array.isArray(runner.steps) && runner.steps.length ? (
          <details style={{ marginTop: 10 }}>
            <summary style={{ cursor: "pointer", opacity: 0.9 }}>
              Ver pasos ({runner.steps.length})
            </summary>
            <div style={{ marginTop: 8, fontSize: 12, opacity: 0.9 }}>
              {runner.steps.map((st, idx) => (
                <div key={idx} style={{ marginTop: 6 }}>
                  <b>
                    {st?.status === "pass" ? "✅" : "❌"} #{st?.i ?? idx + 1} {st?.action}
                  </b>
                  {st?.error ? (
                    <div style={{ opacity: 0.85 }}>Error: {String(st.error)}</div>
                  ) : null}
                </div>
              ))}
            </div>
          </details>
        ) : null}
      </div>
    );
  };

  const renderDocArtifacts = (docArtifacts) => {
    if (!docArtifacts) return null;

    // si es array -> lista simple
    if (Array.isArray(docArtifacts)) {
      return (
        <div style={{ marginTop: 10 }}>
          <div style={{ fontWeight: 800, marginBottom: 6 }}>📎 Artefactos</div>
          <ul style={{ margin: 0, paddingLeft: 18, opacity: 0.95 }}>
            {docArtifacts.map((a, i) => (
              <li key={i}>{typeof a === "string" ? a : JSON.stringify(a)}</li>
            ))}
          </ul>
        </div>
      );
    }

    // si es objeto -> JSON pretty
    return (
      <div style={{ marginTop: 10 }}>
        <div style={{ fontWeight: 800, marginBottom: 6 }}>📎 Artefactos</div>
        <pre
          style={{
            margin: 0,
            padding: 12,
            borderRadius: 12,
            background: "rgba(0,0,0,0.25)",
            border: "1px solid rgba(255,255,255,0.10)",
            overflow: "auto",
            fontSize: 12,
          }}
        >
          {JSON.stringify(docArtifacts, null, 2)}
        </pre>
      </div>
    );
  };

  // -----------------------------
  // Layout
  // -----------------------------
  return (
    <div style={{ display: "flex", height: "100vh", width: "100%" }}>
      <div
        style={{
          width: isSidebarOpen ? 320 : 0,
          overflow: "hidden",
          transition: "width 200ms ease",
          borderRight: isSidebarOpen ? "1px solid rgba(255,255,255,0.08)" : "none",
        }}
      >
        <Sidebar
          threads={threads}
          activeId={threadId}
          onNew={handleNew}
          onSelect={handleSelect}
          onDelete={handleDelete}
          isLoading={sidebarBusy}
        />
      </div>

      <div style={{ flex: 1, display: "flex", flexDirection: "column" }}>
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
                | QA Intelligence Agent{" "}
                {threadId ? `— thread: ${shortId(threadId)}` : ""}
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

        <div style={{ flex: 1, overflow: "auto" }}>
          <Chat
            messages={messages}
            input={input}
            setInput={setInput}
            handleSend={handleSend}
            isLoading={isLoading}
            sessionId={sessionId}
            threadId={threadId}
            renderRunnerReport={renderRunnerReport}
            renderDocArtifacts={renderDocArtifacts}
            formatText={formatText}
            chatEndRef={chatEndRef}
            prettyMode={(m) => m}
            prettyStatus={prettyStatus}
          />
          <div ref={chatEndRef} />
        </div>
      </div>
    </div>
  );
}

export default App;