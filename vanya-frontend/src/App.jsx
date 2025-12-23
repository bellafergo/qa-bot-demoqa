import React, { useEffect, useMemo, useRef, useState } from "react";
import "./App.css";
import { chatRun, getThread, listThreads, createThread as apiCreateThread } from "./api";
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

const prettyStatus = (st) => {
  const s = (st || "").toLowerCase();
  if (s === "passed" || s === "pass" || s === "ok") return "✅ PASSED";
  if (s === "fail" || s === "failed" || s === "error") return "❌ FAILED";
  return st || "—";
};

const prettyMode = (mode) => {
  const m = (mode || "").toLowerCase();
  if (m === "execute") return "EXECUTE";
  if (m === "doc") return "DOC";
  if (m === "advise") return "ADVISE";
  if (m === "need_info") return "NEED INFO";
  if (m === "error") return "ERROR";
  return (mode || "").toUpperCase();
};

function App() {
  // -----------------------------
  // Chat UI state
  // -----------------------------
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState("");
  const [isLoading, setIsLoading] = useState(false);

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
  const refreshThreads = async () => {
    const list = await listThreads(); // api.js ya normaliza a array
    setThreads(list || []);
    return list || [];
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
    const data = await getThread(id);

    const items = data?.messages || data?.thread?.messages || [];
    const uiMessages = mapBackendMessagesToUI(items);

    setThreadId(id);
    try {
      localStorage.setItem("vanya_thread_id", id);
    } catch {}

    if (!uiMessages || uiMessages.length === 0) setWelcome();
    else setMessages(uiMessages);

    if (opts?.refreshSidebar) await refreshThreads().catch(() => {});
    return data;
  };

  const createThread = async () => {
  const data = await apiCreateThread();
  const id = data?.id || data?.thread_id;
  if (!id) throw new Error("No recibí thread_id");

  setThreadId(id);
  try {
    localStorage.setItem("vanya_thread_id", id);
  } catch {}

  // NO borres nada aquí
  // El chat nuevo se cargará vacío desde backend
  await refreshThreads();

  // carga explícitamente el nuevo thread (vacío)
  await loadThread(id, { refreshSidebar: false });

  return id;
};

  // Init: cargar sidebar + cargar thread guardado si existe
  useEffect(() => {
    refreshThreads().catch(() => {});
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
  // Render helpers (runner / doc)
  // -----------------------------
  const renderRunnerReport = (runner) => {
    if (!runner) return null;

    const status = runner.status || runner.state || runner.result;
    const error = runner.error;
    const steps = Array.isArray(runner.steps) ? runner.steps : [];
    const logs = Array.isArray(runner.logs) ? runner.logs : [];
    const duration = runner.duration_ms;
    const evidenceId = runner.evidence_id;

    return (
      <div className="runner-report" style={{ marginTop: 12 }}>
        <div style={{ fontWeight: 700, marginBottom: 6 }}>
          Resultado: {prettyStatus(status)}
          {typeof duration === "number" && (
            <span style={{ fontWeight: 400, marginLeft: 8, opacity: 0.8 }}>
              ({duration} ms)
            </span>
          )}
        </div>

        {evidenceId && (
          <div style={{ fontSize: 12, opacity: 0.85, marginBottom: 10 }}>
            <b>Evidencia ID:</b> {evidenceId}
          </div>
        )}

        {error && (
          <div
            style={{
              padding: 10,
              borderRadius: 10,
              background: "rgba(255, 0, 0, 0.08)",
              border: "1px solid rgba(255, 0, 0, 0.25)",
              marginBottom: 10,
            }}
          >
            <b>Error:</b> {String(error)}
          </div>
        )}

        {steps.length > 0 && (
          <div style={{ marginTop: 10 }}>
            <div style={{ fontWeight: 700, marginBottom: 6 }}>Pasos ejecutados:</div>

            <div
              style={{
                overflowX: "auto",
                borderRadius: 10,
                border: "1px solid rgba(255,255,255,0.12)",
              }}
            >
              <table
                style={{
                  width: "100%",
                  borderCollapse: "collapse",
                  fontSize: 12,
                }}
              >
                <thead>
                  <tr style={{ background: "rgba(255,255,255,0.06)" }}>
                    <th style={{ textAlign: "left", padding: 8 }}>#</th>
                    <th style={{ textAlign: "left", padding: 8 }}>Acción</th>
                    <th style={{ textAlign: "left", padding: 8 }}>Selector/Text/URL</th>
                    <th style={{ textAlign: "left", padding: 8 }}>Status</th>
                    <th style={{ textAlign: "left", padding: 8 }}>ms</th>
                  </tr>
                </thead>
                <tbody>
                  {steps.map((s, idx) => {
                    const st = String(s.status || "").toLowerCase();
                    const ok = st.includes("pass") || st === "ok";
                    const target = s.url || s.selector || s.text || "—";
                    return (
                      <tr
                        key={idx}
                        style={{ borderTop: "1px solid rgba(255,255,255,0.08)" }}
                      >
                        <td style={{ padding: 8, opacity: 0.9 }}>
                          {s.i ?? s.step ?? idx + 1}
                        </td>
                        <td style={{ padding: 8 }}>{s.action || "—"}</td>
                        <td style={{ padding: 8, opacity: 0.9 }}>{target}</td>
                        <td style={{ padding: 8 }}>
                          {ok ? "✅" : "❌"} {s.status || "—"}
                        </td>
                        <td style={{ padding: 8, opacity: 0.9 }}>
                          {s.duration_ms ?? "—"}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {logs.length > 0 && (
          <div style={{ marginTop: 12 }}>
            <div style={{ fontWeight: 700, marginBottom: 6 }}>Logs:</div>
            <pre
              style={{
                whiteSpace: "pre-wrap",
                wordBreak: "break-word",
                padding: 10,
                borderRadius: 10,
                background: "rgba(0,0,0,0.25)",
                border: "1px solid rgba(255,255,255,0.12)",
                fontSize: 12,
                maxHeight: 220,
                overflow: "auto",
              }}
            >
              {logs.join("\n")}
            </pre>
          </div>
        )}
      </div>
    );
  };

  const copyToClipboard = async (text) => {
    try {
      await navigator.clipboard.writeText(text);
      return true;
    } catch {
      return false;
    }
  };

  const renderDocArtifacts = (doc) => {
    if (!doc) return null;

    const scripts = doc?.automation_scripts || {};
    const files = Array.isArray(scripts.files) ? scripts.files : [];
    const framework = scripts.framework || "playwright-python";
    const structure = scripts.structure || "page-object";

    if (!files.length) return null;

    return (
      <div style={{ marginTop: 12 }}>
        <details>
          <summary style={{ cursor: "pointer", fontWeight: 700 }}>
            📦 Scripts ({framework}, {structure}) — {files.length} archivo(s)
          </summary>

          <div style={{ marginTop: 10 }}>
            {files.map((f, idx) => {
              const path = f.path || `file_${idx}.txt`;
              const content = f.content || "";
              return (
                <div
                  key={idx}
                  style={{
                    marginBottom: 12,
                    padding: 10,
                    borderRadius: 10,
                    border: "1px solid rgba(255,255,255,0.12)",
                    background: "rgba(0,0,0,0.18)",
                  }}
                >
                  <div
                    style={{
                      display: "flex",
                      alignItems: "center",
                      justifyContent: "space-between",
                      gap: 10,
                      marginBottom: 8,
                    }}
                  >
                    <div style={{ fontWeight: 700, fontSize: 13 }}>{path}</div>
                    <button
                      style={{ padding: "6px 10px", fontSize: 12, cursor: "pointer" }}
                      onClick={async () => {
                        const ok = await copyToClipboard(content);
                        alert(ok ? `Copiado: ${path}` : "No se pudo copiar.");
                      }}
                    >
                      Copiar
                    </button>
                  </div>

                  <pre
                    style={{
                      whiteSpace: "pre-wrap",
                      wordBreak: "break-word",
                      padding: 10,
                      borderRadius: 10,
                      background: "rgba(0,0,0,0.25)",
                      border: "1px solid rgba(255,255,255,0.12)",
                      fontSize: 12,
                      maxHeight: 260,
                      overflow: "auto",
                    }}
                  >
                    {content}
                  </pre>
                </div>
              );
            })}
          </div>
        </details>
      </div>
    );
  };

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
        addBotMessage(
          `✅ Ejecuté la prueba${data.scope ? ` (${data.scope})` : ""}. Aquí tienes los resultados:`,
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
        addBotMessage(data.answer || "Me falta información para ejecutar.", { mode }, null, null);
      } else if (mode === "advise" || mode === "info" || mode === "plan") {
        addBotMessage(data.answer || "Ok. ¿Qué quieres validar?", { mode: "advise" });
      } else {
        addBotMessage(data.answer || "Ok.", { mode });
      }

      // refresca mensajes desde backend (historial real)
      await loadThread(activeThreadId, { refreshSidebar: false }).catch(() => {});
      await refreshThreads().catch(() => {});
    } catch (error) {
      console.error("Error en la petición:", error);
      addBotMessage(
        "❌ **Error de conexión con Vanya.**\n\n" +
          "El servidor no respondió correctamente. " +
          "Si estás en Render, revisa que el deploy esté Live y vuelve a intentar.\n\n" +
          `Detalle: ${String(error?.message || error)}`,
        { mode: "error" }
      );
    } finally {
      setIsLoading(false);
    }
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
        {isSidebarOpen && (
          <Sidebar
            threads={threads}
            activeId={threadId}
            isLoading={isLoading}
            onNew={async () => {
            await createThread();
             }}
            onSelect={async (id) => {
             await loadThread(id, { refreshSidebar: true });
            }}
          />
        )}
      </div>

      <div style={{ flex: 1, minWidth: 0, display: "flex", flexDirection: "column" }}>
        <div
          style={{
            display: "flex",
            alignItems: "center",
            gap: 10,
            padding: "10px 12px",
            borderBottom: "1px solid rgba(255,255,255,0.08)",
          }}
        >
          <button
            onClick={() => setIsSidebarOpen((v) => !v)}
            style={{ padding: "6px 10px", cursor: "pointer" }}
            title={isSidebarOpen ? "Ocultar historial" : "Mostrar historial"}
          >
            {isSidebarOpen ? "⟨⟨" : "⟩⟩"}
          </button>

          <div style={{ fontWeight: 700, opacity: 0.9 }}>
            Vanya QA Bot{" "}
            <span style={{ fontWeight: 400, opacity: 0.7 }}>
              {threadId ? `• Thread: ${String(threadId).slice(0, 8)}…` : ""}
            </span>
          </div>
        </div>

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
          prettyMode={prettyMode}
          prettyStatus={prettyStatus}
        />
      </div>
    </div>
  );
}

export default App;