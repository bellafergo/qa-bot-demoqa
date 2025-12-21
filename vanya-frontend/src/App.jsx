import React, { useState, useEffect, useRef } from 'react';
import './App.css';

function App() {
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const chatEndRef = useRef(null);

  const API_BASE = "https://qa-bot-demoqa.onrender.com";

  const scrollToBottom = () => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    setMessages([{
      role: 'bot',
      content: 'Hola, soy **Vanya**, tu Agente de QA inteligente. ¿En qué puedo ayudarte hoy con tus pruebas?'
    }]);
  }, []);

  useEffect(scrollToBottom, [messages]);

  const formatText = (text) => {
    if (!text) return "";
    return text
      .replace(/\*\*(.*?)\*\*/g, '<b>$1</b>')
      .replace(/\n/g, '<br/>');
  };

  const prettyStatus = (st) => {
    const s = (st || "").toLowerCase();
    if (s === "passed" || s === "pass" || s === "ok") return "✅ PASSED";
    if (s === "fail" || s === "failed" || s === "error") return "❌ FAILED";
    return st || "—";
  };

  const renderRunnerReport = (runner) => {
    if (!runner) return null;

    const status = runner.status || runner.state || runner.result;
    const error = runner.error;
    const steps = Array.isArray(runner.steps) ? runner.steps : [];
    const logs = Array.isArray(runner.logs) ? runner.logs : [];
    const duration = runner.duration_ms;

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

        {error && (
          <div style={{
            padding: 10,
            borderRadius: 10,
            background: "rgba(255, 0, 0, 0.08)",
            border: "1px solid rgba(255, 0, 0, 0.25)",
            marginBottom: 10
          }}>
            <b>Error:</b> {String(error)}
          </div>
        )}

        {steps.length > 0 && (
          <div style={{ marginTop: 10 }}>
            <div style={{ fontWeight: 700, marginBottom: 6 }}>Pasos ejecutados:</div>

            <div style={{
              overflowX: "auto",
              borderRadius: 10,
              border: "1px solid rgba(255,255,255,0.12)"
            }}>
              <table style={{
                width: "100%",
                borderCollapse: "collapse",
                fontSize: 12
              }}>
                <thead>
                  <tr style={{ background: "rgba(255,255,255,0.06)" }}>
                    <th style={{ textAlign: "left", padding: 8 }}>#</th>
                    <th style={{ textAlign: "left", padding: 8 }}>Acción</th>
                    <th style={{ textAlign: "left", padding: 8 }}>Selector/URL</th>
                    <th style={{ textAlign: "left", padding: 8 }}>Status</th>
                    <th style={{ textAlign: "left", padding: 8 }}>ms</th>
                  </tr>
                </thead>
                <tbody>
                  {steps.map((s, idx) => (
                    <tr key={idx} style={{ borderTop: "1px solid rgba(255,255,255,0.08)" }}>
                      <td style={{ padding: 8, opacity: 0.9 }}>{s.i ?? s.step ?? (idx + 1)}</td>
                      <td style={{ padding: 8 }}>{s.action || "—"}</td>
                      <td style={{ padding: 8, opacity: 0.9 }}>
                        {s.url || s.selector || "—"}
                      </td>
                      <td style={{ padding: 8 }}>
                        {(s.status || "").toLowerCase().includes("pass") ? "✅" : "❌"} {s.status || "—"}
                      </td>
                      <td style={{ padding: 8, opacity: 0.9 }}>{s.duration_ms ?? "—"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {logs.length > 0 && (
          <div style={{ marginTop: 12 }}>
            <div style={{ fontWeight: 700, marginBottom: 6 }}>Logs:</div>
            <pre style={{
              whiteSpace: "pre-wrap",
              wordBreak: "break-word",
              padding: 10,
              borderRadius: 10,
              background: "rgba(0,0,0,0.25)",
              border: "1px solid rgba(255,255,255,0.12)",
              fontSize: 12,
              maxHeight: 220,
              overflow: "auto"
            }}>
              {logs.join("\n")}
            </pre>
          </div>
        )}
      </div>
    );
  };

  const handleSend = async () => {
    if (!input.trim() || isLoading) return;

    const userMsg = { role: 'user', content: input };
    setMessages(prev => [...prev, userMsg]);

    const currentInput = input;
    setInput('');
    setIsLoading(true);

    try {
      const resp = await fetch(`${API_BASE}/chat_run`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          prompt: currentInput,
          headless: true,
          // opcional pero recomendado:
          // base_url: "https://example.com"
        })
      });

      if (!resp.ok) throw new Error(`Error server: ${resp.status}`);

      const data = await resp.json();

if (data.mode === "info" || data.mode === "plan") {
  setMessages(prev => [...prev, {
    role: 'bot',
    content: data.answer || "Listo.",
    runner: null
  }]);
} else {
  setMessages(prev => [...prev, {
    role: 'bot',
    content: "He procesado tu solicitud. Aquí tienes los resultados de la ejecución:",
    runner: data.run_result
  }]);
}

      setMessages(prev => [...prev, {
        role: 'bot',
        content: `He procesado tu solicitud. **${status}**`,
        runner
      }]);

    } catch (error) {
      console.error("Error en la petición:", error);
      setMessages(prev => [...prev, {
        role: 'bot',
        content: "❌ **Error de conexión con Vanya.**\n\nEl servidor en Render no respondió correctamente. Revisa el deploy y vuelve a intentar."
      }]);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="vanya-wrap">
      <header className="vanya-header">
        <div className="logo-dot"></div>
        <h1>Vanya <small>| QA Intelligence Agent</small></h1>
      </header>

      <main className="chat-area">
        {messages.map((msg, i) => (
          <div key={i} className={`message-row ${msg.role}`}>
            <div className="message-label">{msg.role === 'user' ? 'Tú' : 'Vanya'}</div>
            <div className="bubble">
              <div
                className="text-content"
                dangerouslySetInnerHTML={{ __html: formatText(msg.content) }}
              />

              {/* ✅ Reporte completo (PASSED/FAILED + pasos + logs + error) */}
              {msg.runner && renderRunnerReport(msg.runner)}

              {/* ✅ Evidencia */}
              {msg.runner?.screenshot_b64 && (
                <div className="evidence-container">
                  <p className="ev-title">🖼️ Evidencia de ejecución:</p>
                  <img
                    src={`data:image/png;base64,${msg.runner.screenshot_b64}`}
                    alt="Evidencia"
                    className="evidence-img"
                    onClick={() => {
                      const newTab = window.open();
                      newTab.document.write(
                        `<img src="data:image/png;base64,${msg.runner.screenshot_b64}" style="width:100%">`
                      );
                    }}
                  />
                  <p style={{ fontSize: '10px', marginTop: '5px', opacity: 0.5 }}>
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
            if (e.key === 'Enter' && !e.shiftKey) {
              e.preventDefault();
              handleSend();
            }
          }}
        />
        <button onClick={handleSend} disabled={isLoading || !input.trim()}>
          {isLoading ? '...' : 'Enviar'}
        </button>
      </footer>
    </div>
  );
}

export default App;