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

  const isImageUrl = (url) =>
    /\.(png|jpe?g|webp)(\?.*)?$/i.test(String(url || ""));

  const extractEvidenceFromText = (content) => {
    const text = typeof content === "string" ? content : "";
    const m = text.match(/Evidence:\s*(https?:\/\/\S+)/i);
    return m ? m[1].trim() : null;
  };

  const pickEvidenceUrl = (m) => {
    // Normaliza meta: puede venir en meta, meta_json o metaJson
    const meta = m?.meta || m?.meta_json || m?.metaJson || {};
    const runner = meta?.runner || {};
    const evidence = meta?.evidence || {};

    // 1) Directo: lo más confiable
    const directCandidates = [
      meta?.evidence_url,
      meta?.evidenceUrl,
      meta?.screenshot_url,
      meta?.screenshotUrl,
      m?.evidence_url,
      m?.evidenceUrl,
      m?.screenshot_url,
      m?.screenshotUrl,
    ];

    const directHit = directCandidates.find(
      (x) => typeof x === "string" && x.trim()
    );
    if (directHit) return directHit.trim();

    // 2) Runner / Evidence objects (compatibilidad)
    const runnerCandidates = [
      runner?.screenshot_url,
      runner?.screenshotUrl,
      runner?.evidence_url,
      runner?.evidenceUrl,
      runner?.cloudinary_url,
      runner?.image_url,

      // si el runner trae algo anidado (por ejemplo runner.result.evidence_url)
      runner?.result?.evidence_url,
      runner?.result?.evidenceUrl,
      runner?.result?.screenshot_url,
      runner?.result?.screenshotUrl,

      evidence?.screenshot_url,
      evidence?.url,
    ];

    const runnerHit = runnerCandidates.find(
      (x) => typeof x === "string" && x.trim()
    );
    if (runnerHit) return runnerHit.trim();

    // 3) Por si guardaste el runner al root del message (algunas variantes)
    const rootRunnerCandidates = [
      m?.runner?.evidence_url,
      m?.runner?.evidenceUrl,
      m?.runner?.screenshot_url,
      m?.runner?.screenshotUrl,
    ];
    const rootRunnerHit = rootRunnerCandidates.find(
      (x) => typeof x === "string" && x.trim()
    );
    if (rootRunnerHit) return rootRunnerHit.trim();

    // 4) Último fallback: parsear del texto "Evidence: https://..."
    return extractEvidenceFromText(m?.content);
  };

  const pickEvidenceId = (m) => {
    const meta = m?.meta || m?.meta_json || m?.metaJson || {};
    const runner = meta?.runner || {};

    const id =
      runner?.evidence_id ||
      runner?.evidenceId ||
      meta?.evidence_id ||
      meta?.evidenceId ||
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

    const key = String(m?.id || (m?.meta || m?.meta_json || m?.metaJson)?.id || `${role}-${idx}`);

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

          {/* Texto */}
          <div
            dangerouslySetInnerHTML={{ __html: html }}
            style={{ lineHeight: 1.35 }}
          />

          {/* Evidencia */}
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