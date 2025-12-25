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

  const safeMessages = useMemo(() => {
    return Array.isArray(messages) ? messages : [];
  }, [messages]);

  const onEnter = useCallback(
    (e) => {
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault();
        if (!isLoading) handleSend?.();
      }
    },
    [handleSend, isLoading]
  );

  const isImageUrl = (url) => /\.(png|jpe?g|webp)(\?.*)?$/i.test(String(url || ""));

  const extractEvidenceFromText = (content) => {
    const text = typeof content === "string" ? content : "";
    const m = text.match(/Evidence:\s*(https?:\/\/\S+)/i);
    return m ? m[1].trim() : null;
  };

  const pickEvidenceUrl = (m) => {
    // 1) Meta directo (nuevo backend)
    const direct =
      m?.meta?.evidence_url ||
      m?.meta?.evidenceUrl ||
      m?.meta_json?.evidence_url ||
      m?.metaJson?.evidence_url ||
      m?.evidence_url ||
      m?.evidenceUrl;

    if (typeof direct === "string" && direct.trim()) return direct.trim();

    // 2) Runner / Evidence objects (compatibilidad)
    const candidates = [
      // Lo que ya tenías
      m?.meta?.runner?.screenshot_url,
      m?.meta?.runner?.screenshot,
      m?.meta?.runner?.evidence_url,
      m?.meta?.runner?.evidenceUrl,
      m?.meta?.evidence?.screenshot_url,
      m?.meta?.evidence?.url,
      m?.meta?.screenshot_url,
      m?.meta?.screenshotUrl,
      m?.screenshot_url,
      // Nuevos posibles
      m?.meta?.runner?.cloudinary_url,
      m?.meta?.runner?.image_url,
    ];

    const hit = candidates.find((x) => typeof x === "string" && x.trim());
    if (hit) return hit.trim();

    // 3) Último fallback: parsear del texto "Evidence: https://..."
    const fromText = extractEvidenceFromText(m?.content);
    return fromText;
  };

  const pickEvidenceId = (m) => {
    const id =
      m?.meta?.runner?.evidence_id ||
      m?.meta?.runner?.evidenceId ||
      m?.meta?.evidence_id ||
      m?.meta?.evidenceId ||
      m?.evidence_id ||
      m?.evidenceId ||
      null;
    return id ? String(id) : null;
  };

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const evidenceUrl = pickEvidenceUrl(m);
    const evidenceId = pickEvidenceId(m);

    const key = String(m?.id || m?.meta?.id || `${role}-${idx}`);

    return (
      <div
        key={key}
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
                · evid: {evidenceId}
              </span>
            ) : null}
          </div>

          {/* Texto (ya viene formateado desde formatText) */}
          <div
            dangerouslySetInnerHTML={{ __html: html }}
            style={{ lineHeight: 1.35 }}
          />

          {/* Evidencia (SIEMPRE que exista) */}
          {evidenceUrl ? (
            <div style={{ marginTop: 10 }}>
              <a
                href={evidenceUrl}
                target="_blank"
                rel="noreferrer"
                style={{
                  display: "inline-block",
                  marginBottom: 8,
                  fontSize: 12,
                  opacity: 0.9,
                  color: "white",
                  textDecoration: "underline",
                }}
              >
                Abrir evidencia
              </a>

              {/* Render imagen solo si parece imagen */}
              {isImageUrl(evidenceUrl) ? (
                <img
                  src={evidenceUrl}
                  alt="Evidencia de prueba"
                  loading="lazy"
                  style={{
                    maxWidth: "100%",
                    borderRadius: 10,
                    border: "1px solid rgba(255,255,255,0.15)",
                    display: "block",
                  }}
                  onError={(e) => {
                    // Evita que una URL rota rompa el UI
                    e.currentTarget.style.display = "none";
                  }}
                />
              ) : (
                <div style={{ fontSize: 12, opacity: 0.85 }}>
                  (La evidencia no parece imagen. Abre el link.)
                </div>
              )}
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
          onChange={(e) => setInput?.(e.target.value)}
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
          onClick={() => handleSend?.()}
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