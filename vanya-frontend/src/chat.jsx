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

  const safeMessages = useMemo(() => (Array.isArray(messages) ? messages : []), [messages]);

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
    // Puede venir como meta, meta_json (obj o string), metaJson, etc.
    const direct = m?.meta && typeof m.meta === "object" ? m.meta : null;
    if (direct) return direct;

    const parsed = safeJsonParse(m?.meta_json) || safeJsonParse(m?.metaJson);
    if (parsed && typeof parsed === "object") return parsed;

    // fallback: si backend manda meta_json ya como objeto pero no cae arriba
    if (m?.meta_json && typeof m.meta_json === "object") return m.meta_json;

    return {};
  };

  const extractEvidenceFromText = (content) => {
    const text = typeof content === "string" ? content : "";
    // 1) formato: Evidence: https://...
    let m = text.match(/Evidence:\s*(https?:\/\/\S+)/i);
    if (m && m[1]) return m[1].trim();

    // 2) cualquier URL suelta
    m = text.match(/(https?:\/\/[^\s)]+)\b/i);
    if (m && m[1]) return m[1].trim();

    return null;
  };


  const isImageUrl = (url) => {
    const u = String(url || "").toLowerCase();

    // Si tiene extensión típica, es imagen
    if (/\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(u)) return true;

    // Cloudinary casi siempre es imagen aunque no tenga extensión
    if (u.includes("res.cloudinary.com")) return true;
    if (u.includes("cloudinary.com")) return true;
    if (u.includes("/image/upload")) return true;

    return false;
  }

  const pickEvidenceUrl = (m) => {
    const meta = getMeta(m);

    // 1) meta directo (tu backend ya lo guarda)
    const direct =
      meta?.evidence_url ||
      meta?.evidenceUrl ||
      meta?.screenshot_url ||
      meta?.screenshotUrl;

    if (typeof direct === "string" && direct.trim()) return direct.trim();

    // 2) meta.runner.* (tu compat)
    const runner = meta?.runner || {};
    const runnerHit =
      runner?.screenshot_url ||
      runner?.screenshot ||
      runner?.evidence_url ||
      runner?.evidenceUrl ||
      runner?.image_url ||
      runner?.cloudinary_url;

    if (typeof runnerHit === "string" && runnerHit.trim()) return runnerHit.trim();

    // 3) message root (por si el API lo manda plano)
    const flat =
      m?.evidence_url ||
      m?.evidenceUrl ||
      m?.screenshot_url ||
      m?.screenshotUrl;

    if (typeof flat === "string" && flat.trim()) return flat.trim();

    // 4) fallback desde el texto (Evidence: https://...)
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

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const evidenceUrl = pickEvidenceUrl(m);
    const evidenceId = pickEvidenceId(m);

    const key = String(m?.id || getMeta(m)?.id || `${role}-${idx}`);

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
                  referrerPolicy="no-referrer"
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