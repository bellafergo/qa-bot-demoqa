import React, { useEffect, useMemo, useRef, useState } from "react";
import "./App.css";
import { chatRun, getThread, listThreads } from "./api";
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

  // ✅ API por env (Vite) y fallback
  // Mantiene compatibilidad con tu VITE_API_BASE actual y permite VITE_API_BASE_URL
  const API_BASE = useMemo(() => {
    const fromEnv =
      (import.meta?.env?.VITE_API_BASE_URL || "").trim() ||
      (import.meta?.env?.VITE_API_BASE || "").trim();
    return fromEnv || "https://qa-bot-demoqa.onrender.com";
  }, []);

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

  // Init welcome
  useEffect(() => {
    // si ya hay thread seleccionado, lo cargamos; si falla, ponemos welcome
    if (threadId) {
      loadThread(threadId).catch(() => setWelcome());
    } else {
      setWelcome();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // -----------------------------
  // Networking helpers
  // -----------------------------
  const fetchJson = async (path, options = {}) => {
    const url = `${API_BASE}${path}`;
    const resp = await fetch(url, {
      headers: { "Content-Type": "application/json", ...(options.headers || {}) },
      ...options,
    });

    const data = await resp.json().catch(() => ({}));
    if (!resp.ok) {
      const detail = data?.detail ? `\n\nDetalle: ${data.detail}` : "";
      throw new Error(`Error server: ${resp.status}${detail}`);
    }
    return data;
  };

  // -----------------------------
  // Threads: list/create/load
  // -----------------------------
  const refreshThreads = async () => {
    // GET /threads
    // Esperado: [{ id, title, updated_at }]
    const data = await fetchJson("/threads", { method: "GET" });
    const list = Array.isArray(data) ? data : data?.threads || [];
    setThreads(list);
    return list;
  };

  const const createThread = async () => {
    // POST /threads
    // Esperado: { id, title }
    const data = await fetchJson("/threads", {
      method: "POST",
      body: JSON.stringify({}), // ✅ evita problemas si backend no espera body
    });

    const id = data?.id || data?.thread_id;
    if (!id) throw new Error("No recibí thread_id del backend.");

    setThreadId(id);
    try {
      localStorage.setItem("vanya_thread_id", id);
    } catch {}

    // UI: welcome inmediato (optimista)
    setWelcome();

    // refresca sidebar
    await refreshThreads().catch(() => {});

    // ✅ opcional: carga el thread (por si tu backend luego agrega mensaje system inicial)
    await loadThread(id).catch(() => {});
    return id;
  };

  const mapBackendMessagesToUI = (items) => {
  // backend: [{ role: "user"|"assistant"|"system", content, created_at, meta? }]
  // ui: { role: "user"|"bot", content, meta, runner, docArtifacts }

  if (!Array.isArray(items) || items.length === 0) {
    return null; // 👈 una sola regla: vacío = null
  }

  const ui = [];

  for (const m of items) {
    const role = String(m?.role || "").toLowerCase();

    // ignoramos system
    if (role === "system") continue;

    if (role === "assistant") {
      ui.push({
        role: "bot",
        content: String(m?.content || ""), // 👈 siempre string
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
  // GET /threads/{id}
  // Esperado: { id, title, messages: [...] }

  const data = await getThread(id); // ✅ usa api.js (retry + error detail)

  const items = data?.messages || data?.thread?.messages || [];
  const uiMessages = mapBackendMessagesToUI(items);

  setThreadId(id);
  try {
    localStorage.setItem("vanya_thread_id", id);
  } catch {}

  // ✅ Manejo correcto de vacío: welcome si no hay mensajes o si el mapper regresa null/[]
  const isEmpty =
    !uiMessages ||
    (Array.isArray(uiMessages) && uiMessages.length === 0);

  if (isEmpty) setWelcome();
  else setMessages(uiMessages);

  // ✅ Evita doble refresh (handleSend ya refresca)
  if (opts?.refreshSidebar) {
    await refreshThreads().catch(() => {});
  }

  return data;
};

  // Refresh threads on mount (no bloquea)
  useEffect(() => {
    refreshThreads().catch(() => {});
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // -----------------------------
  // UI components
  // -----------------------------
  const ModeBadge = ({ mode }) => {
    const m = (mode || "").toLowerCase();
    const bg =
      m === "execute"
        ? "rgba(0, 180, 120, 0.18)"
        : m === "doc"
        ? "rgba(80, 140, 255, 0.18)"
        : m === "need_info"
        ? "rgba(255, 180, 0, 0.18)"
        : m === "error"
        ? "rgba(255, 0, 0, 0.18)"
        : "rgba(255,255,255,0.10)";

    const bd =
      m === "execute"
        ? "rgba(0, 180, 120, 0.35)"
        : m === "doc"
        ? "rgba(80, 140, 255, 0.35)"
        : m === "need_info"
        ? "rgba(255, 180, 0, 0.35)"
        : m === "error"
        ? "rgba(255, 0, 0, 0.35)"
        : "rgba(255,255,255,0.18)";

    return (
      <span
        style={{
          display: "inline-flex",
          alignItems: "center",
          gap: 6,
          padding: "4px 8px",
          borderRadius: 999,
          border: `1px solid ${bd}`,
          background: bg,
          fontSize: 11,
          fontWeight: 700,
          letterSpacing: 0.3,
          marginLeft: 8,
          opacity: 0.95,
        }}
        title={`Modo: ${mode || "—"}`}
      >
        {prettyMode(mode)}
      </span>
    );
  };


  // Runner report UI
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
            <div style={{ fontWeight: 700, marginBottom: 6 }}>
              Pasos ejecutados:
            </div>

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
                    <th style={{ textAlign: "left", padding: 8 }}>
                      Selector/Text/URL
                    </th>
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
                        style={{
                          borderTop: "1px solid rgba(255,255,255,0.08)",
                        }}
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

  // DOC UI
  const copyToClipboard = async (text) => {
    try {
      await navigator.clipboard.writeText(text);
      return true;
    } catch {
      return false;
    }
  };

  const renderNeedInfoHint = (answer) => {
    if (!answer) return null;
    return (
      <div
        style={{
          marginTop: 12,
          padding: 10,
          borderRadius: 10,
          border: "1px solid rgba(255,180,0,0.35)",
          background: "rgba(255,180,0,0.10)",
          fontSize: 12,
          lineHeight: 1.4,
        }}
      >
        <b>Faltan datos para ejecutar:</b>
        <ul style={{ margin: "8px 0 0 18px" }}>
          <li>URL (o di “la misma”)</li>
          <li>Qué validar (botón/campo/texto esperado)</li>
          <li>Credenciales (si aplica)</li>
        </ul>
        <div style={{ marginTop: 8, opacity: 0.9 }}>{answer}</div>
      </div>
    );
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
                      style={{
                        padding: "6px 10px",
                        fontSize: 12,
                        cursor: "pointer",
                      }}
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

  // Message helpers
  const addMessage = (msg) => setMessages((prev) => [...prev, msg]);

  const addBotMessage = (content, meta = {}, runner = null, docArtifacts = null) =>
    addMessage({ role: "bot", content, meta, runner, docArtifacts });

  // -----------------------------
  // Main send (POST /chat_run)
  // -----------------------------
  const ensureThreadId = async () => {
    if (threadId) return threadId;
    // si no hay thread, crea uno
    const id = await createThread();
    return id;
  };

  const handleSend = async () => {
  const text = input.trim();
  if (!text || isLoading) return;

  // optimistic UI
  const optimisticId = crypto?.randomUUID?.() || `optimistic-${Date.now()}`;
  addMessage({ id: optimisticId, role: "user", content: text });

  setInput("");
  setIsLoading(true);

  try {
    const activeThreadId = await ensureThreadId();

    // ⬇️ usa tu api.js (mejor errors + retry)
    const data = await chatRun(text, activeThreadId, { session_id: sessionId });

    // Persist session_id
    if (data?.session_id) {
      setSessionId(data.session_id);
      try {
        localStorage.setItem("vanya_session_id", data.session_id);
      } catch {}
    }

    const mode = String(data?.mode || "").toLowerCase();

    // Render bot response
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
      addBotMessage(
        data.answer || "Me falta información para ejecutar.",
        { mode: "need_info" },
        null,
        null
      );
    } else if (mode === "advise" || mode === "info" || mode === "plan") {
      addBotMessage(data.answer || "Ok. ¿Qué quieres validar?", { mode: "advise" });
    } else {
      addBotMessage(data.answer || "Ok.", { mode });
    }

    // ✅ CLAVE: recargar desde BD para que UI = backend
    // Tip anti-duplicados:
    // 1) carga el thread
    // 2) tu loadThread debe "reemplazar" la lista de mensajes (no append)
    await loadThread(activeThreadId).catch(async () => {
      // fallback si loadThread falla: pega directo al endpoint
      try {
        const t = await getThread(activeThreadId);
        // si tienes una función para setear mensajes desde thread, úsala aquí.
        // ejemplo: setMessages(t.messages || []);
      } catch {}
    });

    // refrescar sidebar
    await refreshThreads().catch(async () => {
      try {
        await listThreads();
      } catch {}
    });
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
    <Sidebar
      threads={threads}
      activeId={threadId}
      isLoading={isLoading}
      onNew={async () => {
        const t = await createThread();
        await loadThread(t.id);
      }}
      onSelect={async (id) => {
        await loadThread(id);
      }}
    />

    <Chat
      messages={messages}
      input={input}
      setInput={setInput}
      handleSend={handleSend}
      isLoading={isLoading}
    />
  </div>
);

export default App;