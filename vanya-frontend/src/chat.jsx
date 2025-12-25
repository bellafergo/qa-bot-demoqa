import React, { useMemo, useCallback } from "react";

export default function Chat(props) {
  const {
    messages = [],
    input = "",
    setInput = () => {},
    handleSend = () => {},
    isLoading = false,
    sessionId = null,
    threadId = null,
    formatText = (t) => (typeof t === "string" ? t : ""),
    chatEndRef = null,
  } = props || {};

  const safeMessages = useMemo(
    () => (Array.isArray(messages) ? messages : []),
    [messages]
  );

  const onEnter = useCallback(
    (e) => {
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault();
        handleSend();
      }
    },
    [handleSend]
  );

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    // ---- Evidence / Screenshot (varios nombres posibles)
    const screenshot =
      m?.meta?.runner?.screenshot_url ||
      m?.meta?.runner?.screenshot ||
      m?.meta?.runner?.evidence_url ||
      m?.meta?.evidence?.screenshot_url ||
      m?.meta?.screenshot_url ||
      null;

    const evidenceId =
      m?.meta?.runner?.evidence_id ||
      m?.meta?.runner?.evidenceId ||
      m?.meta?.evidence_id ||
      null;

    return (
      <div
        key={`${m?.meta?.id || idx}`}
        style={{
          display: "flex",
          justifyContent: role === "user" ? "flex-end" : "flex-start",
          marginBottom: 12,
          padding: "0 6px",
        }}
      >
        <div
          style={{
            maxWidth: 760,
            padding: "10px 12px",
            borderRadius: 14,
            background:
              role === "user"
                ? "rgba(120,160,255,0.18)"
                : "rgba(255,255,255,0.08)",
            border: "1px solid rgba(255,255,255,0.12)",
            color: "white",
            wordBreak: "break-word",
          }}
        >
          <div style={{ fontSize: 12, opacity: 0.75, marginBottom: 6 }}>
            {role === "user" ? "Tú" : "Vanya"}
            {evidenceId ? (
              <span style={{ marginLeft: 8, opacity: 0.8 }}>
                · evid: {String(evidenceId)}
              </span>
            ) : null}
          </div>

          {/* Texto seguro */}
          <div
            dangerouslySetInnerHTML={{ __html: html }}
            style={{ lineHeight: 1.35 }}
          />

          {/* Screenshot (si existe) */}
          {screenshot ? (
            <div style={{ marginTop: 10 }}>
              <a
                href={screenshot}
                target="_blank"
                rel="noreferrer"
                style={{
                  display: "inline-block",
                  marginBottom: 8,
                  fontSize: 12,
                  opacity: 0.85,
                  color: "white",
                  textDecoration: "underline",
                }}
              >
                Abrir evidencia
              </a>

              <img
                src={screenshot}
                alt="Evidencia de prueba"
                loading="lazy"
                style={{
                  maxWidth: "100%",
                  borderRadius: 10,
                  border: "1px solid rgba(255,255,255,0.15)",
                }}
                onError={(e) => {
                  // Si falla por 404/CORS, evitamos que rompa la UI
                  e.currentTarget.style.display = "none";
                }}
              />
            </div>
          ) : null}
        </div>
      </div>
    );
  };

  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      {/* Messages */}
      <div style={{ flex: 1, overflow: "auto", padding: 14 }}>
        {!threadId ? (
          <div style={{ color: "rgba(255,255,255,0.7)", padding: 12 }}>
            Selecciona un chat o crea uno nuevo.
          </div>
        ) : null}

        {safeMessages.map(renderMsg)}

        <div ref={chatEndRef || undefined} />
      </div>

      {/* Input */}
      <div
        style={{
          padding: 12,
          borderTop: "1px solid rgba(255,255,255,0.08)",
          display: "flex",
          gap: 10,
          alignItems: "center",
        }}
      >
        <textarea
          value={input}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={onEnter}
          placeholder="Escribe aquí… (Enter para enviar, Shift+Enter para salto)"
          rows={1}
          style={{
            flex: 1,
            resize: "none",
            padding: "10px 12px",
            borderRadius: 12,
            border: "1px solid rgba(255,255,255,0.14)",
            background: "rgba(0,0,0,0.25)",
            color: "white",
            outline: "none",
          }}
          disabled={isLoading}
        />

        <button
          onClick={handleSend}
          disabled={isLoading || !String(input || "").trim()}
          style={{
            padding: "10px 14px",
            borderRadius: 12,
            border: "1px solid rgba(255,255,255,0.14)",
            background: isLoading
              ? "rgba(255,255,255,0.08)"
              : "rgba(120,160,255,0.35)",
            color: "white",
            cursor: isLoading ? "not-allowed" : "pointer",
            fontWeight: 700,
          }}
          title={sessionId ? `session: ${sessionId}` : ""}
        >
          {isLoading ? "..." : "Enviar"}
        </button>
      </div>
    </div>
  );
}