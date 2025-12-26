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
        if (!isLoading) handleSend?.();
      }
    },
    [handleSend, isLoading]
  );

  // ---------- helpers ----------
  const safeJsonParse = (v) => {
    if (!v) return null;
    if (typeof v === "object") return v;
    if (typeof v !== "string") return null;
    try {
      return JSON.parse(v);
    } catch {
      return null;
    }
  };

  const getMeta = (m) => {
    const direct = m?.meta && typeof m.meta === "object" ? m.meta : null;
    if (direct) return direct;

    const parsed =
      safeJsonParse(m?.meta_json) ||
      safeJsonParse(m?.metaJson) ||
      safeJsonParse(m?.metaJSON);
    if (parsed && typeof parsed === "object") return parsed;

    if (m?.meta_json && typeof m.meta_json === "object") return m.meta_json;

    return {};
  };

  // OJO: ya NO extraemos "cualquier URL" (porque agarra saucedemo y lo pone como evidencia).
  // Solo aceptamos el formato explícito:
  // "Evidence: https://...."
  const extractEvidenceFromText = (content) => {
    const text = typeof content === "string" ? content : "";
    const m = text.match(/Evidence:\s*(https?:\/\/\S+)/i);
    return m && m[1] ? m[1].trim() : null;
  };

  const isImageUrl = (url) => {
    const u = String(url || "").toLowerCase();

    if (/\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(u)) return true;

    // Cloudinary normalmente sí es imagen aunque no tenga extensión
    if (u.includes("res.cloudinary.com")) return true;
    if (u.includes("cloudinary.com")) return true;
    if (u.includes("/image/upload")) return true;

    return false;
  };

  const pickEvidenceUrl = (m) => {
    const meta = getMeta(m);
    const runner = meta?.runner || {};

    // 1) meta directo
    const direct =
      meta?.evidence_url ||
      meta?.evidenceUrl ||
      meta?.screenshot_url ||
      meta?.screenshotUrl;

    if (typeof direct === "string" && direct.trim()) return direct.trim();

    // 2) runner.*
    const runnerHit =
      runner?.screenshot_url ||
      runner?.screenshot ||
      runner?.evidence_url ||
      runner?.evidenceUrl ||
      runner?.image_url ||
      runner?.cloudinary_url;

    if (typeof runnerHit === "string" && runnerHit.trim()) return runnerHit.trim();

    // 3) plano
    const flat =
      m?.evidence_url ||
      m?.evidenceUrl ||
      m?.screenshot_url ||
      m?.screenshotUrl;

    if (typeof flat === "string" && flat.trim()) return flat.trim();

    // 4) fallback desde el texto SOLO si viene "Evidence: ..."
    const fromText = extractEvidenceFromText(m?.content);
    if (typeof fromText === "string" && fromText.trim()) return fromText.trim();

    return null;
  };

  const pickEvidenceId = (m) => {
    const meta = getMeta(m);
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

  const shouldShowEvidence = (m) => {
    // SOLO mostrar evidencia en mensajes del bot
    if ((m?.role || "").trim() !== "assistant") return false;

    const meta = getMeta(m);
    // preferimos mostrar evidencia si el modo es execute (o si hay runner)
    const mode = (meta?.mode || "").toString().toLowerCase();
    if (mode === "execute") return true;
    if (meta?.runner && typeof meta.runner === "object") return true;

    // Si trae evidence_url explícito, también
    if (meta?.evidence_url || meta?.screenshot_url) return true;

    return false;
  };

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const meta = getMeta(m);
    const evidenceUrl = pickEvidenceUrl(m);
    const evidenceId = pickEvidenceId(m);

    const key = String(m?.id || meta?.id || `${role}-${idx}`);

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

          {/* Evidencia (solo bot/execute) */}
          {shouldShowEvidence(m) && evidenceUrl ? (
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
                  referrerPolicy="no-referrer"
                  style={{
                    maxWidth: "100%",
                    borderRadius: 10,
                    border: "1px solid rgba(255,255,255,0.15)",
                    display: "block",
                  }}
                  onError={(e) => {
                    // si falla la imagen, escondemos la img pero el link queda
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