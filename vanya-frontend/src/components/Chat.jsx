// src/components/Chat.jsx
import React from "react";

function ModeBadge({ mode }) {
  const m = String(mode || "").toLowerCase();

  const label =
    m === "execute"
      ? "EXECUTE"
      : m === "doc"
      ? "DOC"
      : m === "need_info"
      ? "NEED INFO"
      : m === "advise"
      ? "ADVISE"
      : m === "error"
      ? "ERROR"
      : m === "welcome"
      ? "WELCOME"
      : (mode || "").toUpperCase() || "INFO";

  const style = {
    fontSize: 10,
    padding: "2px 8px",
    borderRadius: 999,
    border: "1px solid rgba(255,255,255,0.18)",
    opacity: 0.85,
    userSelect: "none",
  };

  return <span style={style}>{label}</span>;
}

export default function Chat({
  messages = [],
  input,
  setInput,
  handleSend,
  isLoading,
  sessionId,
  threadId,
  chatEndRef,
  formatText = (t) => t,
  renderNeedInfoHint, // opcional
  renderRunnerReport, // opcional
  renderDocArtifacts, // opcional
}) {
  return (
    <div className="vanya-wrap" style={{ flex: 1, minWidth: 0 }}>
      <header className="vanya-header">
        <div className="logo-dot"></div>
        <h1 style={{ display: "flex", alignItems: "center", gap: 10 }}>
          Vanya <small>| QA Intelligence Agent</small>
          {threadId && (
            <span style={{ fontSize: 11, opacity: 0.55 }}>
              thread: {String(threadId).slice(0, 8)}…
            </span>
          )}
        </h1>
      </header>

      <main className="chat-area">
        {messages.map((msg, i) => (
          <div key={i} className={`message-row ${msg.role}`}>
            <div className="message-label">
              {msg.role === "user" ? "Tú" : "Vanya"}
            </div>

            <div className="bubble">
              <div
                style={{
                  display: "flex",
                  alignItems: "center",
                  gap: 8,
                  marginBottom: 6,
                }}
              >
                {msg.role === "bot" && <ModeBadge mode={msg.meta?.mode} />}

                {msg.role === "bot" && sessionId && (
                  <span style={{ fontSize: 10, opacity: 0.45 }}>
                    session: {String(sessionId).slice(0, 8)}…
                  </span>
                )}
              </div>

              <div
                className="text-content"
                dangerouslySetInnerHTML={{ __html: formatText(msg.content) }}
              />

              {msg.meta?.mode === "need_info" &&
                typeof renderNeedInfoHint === "function" &&
                renderNeedInfoHint(msg.content)}

              {msg.runner && typeof renderRunnerReport === "function" && renderRunnerReport(msg.runner)}

              {msg.docArtifacts &&
                typeof renderDocArtifacts === "function" &&
                renderDocArtifacts(msg.docArtifacts)}

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