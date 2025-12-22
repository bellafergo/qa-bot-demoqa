import React, { useState, useEffect, useMemo, useRef } from "react";
import "./App.css";

function App() {
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [sessionId, setSessionId] = useState(() => {
    try {
      return localStorage.getItem("vanya_session_id") || null;
    } catch {
      return null;
    }
  });
  const chatEndRef = useRef(null);

  // ✅ Mejor: API por env (Vite) y fallback al tuyo
  const API_BASE = useMemo(() => {
    const fromEnv = (import.meta?.env?.VITE_API_BASE || "").trim();
    return fromEnv || "https://qa-bot-demoqa.onrender.com";
  }, []);

  const scrollToBottom = () => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    setMessages([
      {
        role: "bot",
        content:
          "Hola, soy **Vanya**, tu Agente de QA inteligente. ¿En qué puedo ayudarte hoy con tus pruebas?",
      },
    ]);
  }, []);

  useEffect(scrollToBottom, [messages]);

  // ------------------------------------------------------------
  // Formatting (sin librerías)
  // ------------------------------------------------------------
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
    // Escapamos primero para evitar inyecciones
    const safe = escapeHtml(text);
    // Luego aplicamos markdown simple
    return safe
      .replace(/\*\*(.*?)\*\*/g, "<b>$1</b>")
      .replace(/\n/g, "<br/>");
  };

  const prettyStatus = (st) => {
    const s = (st || "").toLowerCase();
    if (s === "passed" || s === "pass" || s === "ok") return "✅ PASSED";
    if (s === "fail" || s === "failed" || s === "error") return "❌ FAILED";
    return st || "—";
  };

  // ------------------------------------------------------------
  // Runner report UI
  // ------------------------------------------------------------
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

  // ------------------------------------------------------------
  // Message helpers
  // ------------------------------------------------------------
  const addMessage = (msg) => setMessages((prev) => [...prev, msg]);

  const addBotMessage = (content, runner = null) =>
    addMessage({ role: "bot", content, runner });

  // ------------------------------------------------------------
  // Main send
  // ------------------------------------------------------------
  const handleSend = async () => {
    const text = input.trim();
    if (!text || isLoading) return;

    addMessage({ role: "user", content: text });
    setInput("");
    setIsLoading(true);

    try {
      const resp = await fetch(`${API_BASE}/chat_run`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          prompt: text,
          headless: true,
          session_id: sessionId,
        }),
      });

      const data = await resp.json().catch(() => ({}));

      // Guarda session_id (para "la misma URL")
      if (data.session_id) {
        setSessionId(data.session_id);
        try {
          localStorage.setItem("vanya_session_id", data.session_id);
        } catch {}
      }

      if (!resp.ok) {
        const detail = data?.detail ? `\n\nDetalle: ${data.detail}` : "";
        throw new Error(`Error server: ${resp.status}${detail}`);
      }

      const mode = (data.mode || "").toLowerCase();

      // ✅ MODO EJECUCIÓN
      if (mode === "execute") {
        const runner = data.run_result || null;
        const scope = data.scope ? ` (${data.scope})` : "";
        addBotMessage(
          `✅ Ejecuté la prueba${scope}. Aquí tienes los resultados:`,
          runner
        );
        return;
      }

      // ✅ MODO ASESOR / PEDIR INFO
      if (mode === "advise" || mode === "need_info" || mode === "info" || mode === "plan") {
        addBotMessage(data.answer || "Ok. ¿Qué quieres validar?");
        return;
      }

      // Fallback
      addBotMessage(data.answer || "Ok.");
    } catch (error) {
      console.error("Error en la petición:", error);
      addBotMessage(
        "❌ **Error de conexión con Vanya.**\n\n" +
          "El servidor no respondió correctamente. " +
          "Si estás en Render, revisa que el deploy esté Live y vuelve a intentar.\n\n" +
          `Detalle: ${String(error?.message || error)}`
      );
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="vanya-wrap">
      <header className="vanya-header">
        <div className="logo-dot"></div>
        <h1>
          Vanya <small>| QA Intelligence Agent</small>
        </h1>
      </header>

      <main className="chat-area">
        {messages.map((msg, i) => (
          <div key={i} className={`message-row ${msg.role}`}>
            <div className="message-label">{msg.role === "user" ? "Tú" : "Vanya"}</div>
            <div className="bubble">
              <div
                className="text-content"
                dangerouslySetInnerHTML={{ __html: formatText(msg.content) }}
              />

              {/* Reporte completo */}
              {msg.runner && renderRunnerReport(msg.runner)}

              {/* Evidencia */}
              {msg.runner?.screenshot_b64 && (
                <div className="evidence-container">
                  <p className="ev-title">🖼️ Evidencia de ejecución:</p>
                  <img
                    src={`data:image/png;base64,${msg.runner.screenshot_b64}`}
                    alt="Evidencia"
                    className="evidence-img"
                    onClick={() => {
                      const newTab = window.open();
                      if (!newTab) return;
                      newTab.document.write(
                        `<img src="data:image/png;base64,${msg.runner.screenshot_b64}" style="width:100%">`
                      );
                    }}
                  />
                  <p style={{ fontSize: "10px", marginTop: "5px", opacity: 0.5 }}>
                    Click para ampliar
                  </p>
                </div>
              )}
            </div>
          </div>
        ))}

        {isLoading && (
          <div className="message-row bot">
            <div className="message-label">Vanya</div>
            <div className="bubble loading">Vanya está procesando tu solicitud...</div>
          </div>
        )}
        <div ref={chatEndRef} />
      </main>

      <footer className="input-area">
        <textarea
          value={input}
          onChange={(e) => setInput(e.target.value)}
          placeholder="Escribe una orden de QA o pregunta algo..."
          disabled={isLoading}
          onKeyDown={(e) => {
            if (e.key === "Enter" && !e.shiftKey) {
              e.preventDefault();
              handleSend();
            }
          }}
        />
        <button onClick={handleSend} disabled={isLoading || !input.trim()}>
          {isLoading ? "..." : "Enviar"}
        </button>
      </footer>
    </div>
  );
}

export default App;