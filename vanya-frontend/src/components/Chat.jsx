import React from "react";

export default function Chat({
  messages = [],
  input,
  setInput,
  handleSend,
  isLoading,
  renderRunnerReport,
  renderDocArtifacts,
  formatText,
  chatEndRef,
}) {
  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
      {/* Messages */}
      <div style={{ flex: 1, overflowY: "auto", padding: 16 }}>
        {messages.map((m, i) => {
          // 🔑 CLAVE: soportar runner nuevo y runner histórico
          const runner = m.runner || m.meta?.runner || null;
          const docArtifacts = m.docArtifacts || m.meta?.docArtifacts || null;

          return (
            <div
              key={i}
              style={{
                marginBottom: 14,
                textAlign: m.role === "user" ? "right" : "left",
              }}
            >
              <div
                style={{
                  display: "inline-block",
                  maxWidth: "92%",
                  padding: 12,
                  borderRadius: 14,
                  background:
                    m.role === "user"
                      ? "linear-gradient(135deg,#4f6cf7,#3b82f6)"
                      : "rgba(255,255,255,0.06)",
                  border:
                    m.role === "user"
                      ? "none"
                      : "1px solid rgba(255,255,255,0.12)",
                  color: "white",
                }}
              >
                {/* TEXTO */}
                {m.content ? (
                  <div
                    dangerouslySetInnerHTML={{
                      __html: formatText(m.content),
                    }}
                  />
                ) : null}

                {/* ✅ RUNNER (nuevo + histórico, UNA SOLA VEZ) */}
                {runner ? renderRunnerReport(runner) : null}

                {/* 📄 Artefactos (si existen) */}
                {docArtifacts ? renderDocArtifacts(docArtifacts) : null}
              </div>
            </div>
          );
        })}

        <div ref={chatEndRef} />
      </div>

      {/* Input */}
      <div
        style={{
          display: "flex",
          gap: 10,
          padding: 14,
          borderTop: "1px solid rgba(255,255,255,0.08)",
        }}
      >
        <input
          value={input}
          onChange={(e) => setInput(e.target.value)}
          placeholder="Escribe una orden de QA o pregunta algo…"
          onKeyDown={(e) => e.key === "Enter" && handleSend()}
          style={{
            flex: 1,
            padding: 12,
            borderRadius: 12,
            border: "1px solid rgba(255,255,255,0.15)",
            background: "rgba(0,0,0,0.25)",
            color: "white",
          }}
        />
        <button
          onClick={handleSend}
          disabled={isLoading}
          style={{
            padding: "0 18px",
            borderRadius: 12,
            background: "#3b82f6",
            color: "white",
            border: "none",
            cursor: "pointer",
            opacity: isLoading ? 0.6 : 1,
          }}
        >
          {isLoading ? "…" : "Enviar"}
        </button>
      </div>
    </div>
  );
}