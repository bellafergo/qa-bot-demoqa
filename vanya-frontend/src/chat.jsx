// vanya-frontend/src/chat.jsx
import React, { useMemo, useCallback, useState } from "react";

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

    const parsed = safeJsonParse(m?.meta_json) || safeJsonParse(m?.metaJson);
    if (parsed && typeof parsed === "object") return parsed;

    if (m?.meta_json && typeof m.meta_json === "object") return m.meta_json;

    return {};
  };

  const extractEvidenceFromText = (content) => {
    const text = typeof content === "string" ? content : "";
    let mm = text.match(/Evidence:\s*(https?:\/\/\S+)/i);
    if (mm && mm[1]) return mm[1].trim();

    mm = text.match(/(https?:\/\/[^\s)]+)\b/i);
    if (mm && mm[1]) return mm[1].trim();

    return null;
  };

  // Construye Data URL seguro
  const toDataUrl = (b64) => {
    const s = String(b64 || "").trim();
    if (!s) return null;
    if (s.startsWith("data:image/")) return s;
    return "data:image/png;base64," + s;
  };

  const pickRunner = (m) => {
    const meta = getMeta(m);
    const r1 = meta?.runner && typeof meta.runner === "object" ? meta.runner : null;
    const r2 = m?.runner && typeof m.runner === "object" ? m.runner : null;
    return r1 || r2 || {};
  };

  const pickEvidenceUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    // ✅ Prioridad 1: Data URL directo del backend (FIX principal)
    const dataUrl =
      runner?.screenshot_data_url ||
      runner?.screenshotDataUrl ||
      meta?.screenshot_data_url ||
      meta?.screenshotDataUrl ||
      meta?.runner?.screenshot_data_url ||
      meta?.runner?.screenshotDataUrl ||
      null;

    if (typeof dataUrl === "string" && dataUrl.trim()) return dataUrl.trim();

    // ✅ Prioridad 2: Base64 -> Data URL
    const b64 =
      runner?.screenshot_b64 ||
      runner?.screenshotB64 ||
      runner?.screenshotBase64 ||
      meta?.screenshot_b64 ||
      meta?.screenshotB64 ||
      meta?.screenshotBase64 ||
      meta?.runner?.screenshot_b64 ||
      meta?.runner?.screenshotB64 ||
      meta?.runner?.screenshotBase64 ||
      null;

    const b64AsDataUrl = toDataUrl(b64);
    if (b64AsDataUrl) return b64AsDataUrl;

    // ✅ Prioridad 3: URLs externas (Cloudinary/S3/etc)
    const candidates = [
      // meta directo
      meta?.secure_url,
      meta?.image_url,
      meta?.evidence_url,
      meta?.screenshot_url,
      meta?.evidenceUrl,
      meta?.screenshotUrl,

      // meta.runner.*
      runner?.secure_url,
      runner?.image_url,
      runner?.screenshot_url,
      runner?.evidence_url,
      runner?.screenshotUrl,
      runner?.evidenceUrl,

      // plano en el message root
      m?.secure_url,
      m?.image_url,
      m?.evidence_url,
      m?.screenshot_url,
      m?.evidenceUrl,
      m?.screenshotUrl,
    ].filter((x) => typeof x === "string" && x.trim());

    if (candidates.length) return candidates[0].trim();

    // fallback desde texto
    const fromText = extractEvidenceFromText(m?.content);
    if (typeof fromText === "string" && fromText.trim()) return fromText.trim();

    return null;
  };

  const pickReportUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    const url =
      meta?.report_url ||
      meta?.reportUrl ||
      runner?.report_url ||
      runner?.reportUrl ||
      m?.report_url ||
      m?.reportUrl ||
      null;

    if (typeof url !== "string") return null;
    const u = url.trim();
    return u ? u : null;
  };

  const pickEvidenceId = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);
    const id =
      runner?.evidence_id ||
      runner?.evidenceId ||
      meta?.evidence_id ||
      meta?.evidenceId ||
      meta?.runner?.evidence_id ||
      meta?.runner?.evidenceId ||
      m?.evidence_id ||
      m?.evidenceId ||
      null;
    return id ? String(id) : null;
  };

  const isExecuteMessage = (m) => {
    const meta = getMeta(m);
    const mode = String(meta?.mode || m?.mode || "").trim().toLowerCase();
    return mode === "execute" || mode === "execute_mode";
  };

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const meta = getMeta(m);
    const evidenceUrl = pickEvidenceUrl(m);
    const reportUrl = pickReportUrl(m);
    const evidenceId = pickEvidenceId(m);

    const isExecute = isExecuteMessage(m);

    // ✅ Evidencia solo si viene (y para bot/execute)
    const showEvidence = !!evidenceUrl && (isExecute || role === "bot");

    // ✅ Reporte solo cuando exista (y normalmente en execute)
    const showReport = !!reportUrl && (isExecute || role === "bot");

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

          {/* Evidencia */}
          {showEvidence ? <EvidenceBlock evidenceUrl={evidenceUrl} /> : null}

          {/* Reporte descargable */}
          {showReport ? <ReportLink reportUrl={reportUrl} /> : null}
        </div>
      </div>
    );
  };

  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div style={{ flex: 1, overflow: "auto", padding: 14 }}>
        {!threadId ? (
          <div style={{ color: "rgba(255,255,255,0.7)", padding: 12 }}>
            Selecciona un chat o crea uno nuevo.
          </div>
        ) : null}

        {safeMessages.map(renderMsg)}
        <div ref={chatEndRef || undefined} />
      </div>

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

// ---------- Evidence component ----------
function EvidenceBlock({ evidenceUrl }) {
  const [failed, setFailed] = useState(false);

  const url = String(evidenceUrl || "").trim();
  const lower = url.toLowerCase();

  const looksImage = (() => {
    if (!url) return false;
    if (lower.startsWith("data:image/")) return true;
    if (/\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(lower)) return true;
    if (lower.includes("res.cloudinary.com") || lower.includes("/image/upload")) return true;
    return false;
  })();

  // Cache-buster SOLO para URLs http(s); para data:image NO
  const imgSrc = (() => {
    if (!url) return "";
    if (lower.startsWith("data:image/")) return url;
    return url.includes("?") ? `${url}&cb=${Date.now()}` : `${url}?cb=${Date.now()}`;
  })();

  return (
    <div style={{ marginTop: 10 }}>
      {!lower.startsWith("data:image/") ? (
        <a
          href={url}
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
      ) : (
        <div style={{ fontSize: 12, opacity: 0.8, marginBottom: 8 }}>
          Evidencia (captura)
        </div>
      )}

      {looksImage && !failed ? (
        <img
          src={imgSrc}
          alt="Evidencia de prueba"
          loading="lazy"
          referrerPolicy="no-referrer"
          style={{
            maxWidth: "100%",
            borderRadius: 10,
            border: "1px solid rgba(255,255,255,0.15)",
            display: "block",
          }}
          onError={() => setFailed(true)}
        />
      ) : (
        <div style={{ fontSize: 12, opacity: 0.85 }}>
          {failed ? "⚠️ No se pudo cargar la imagen inline." : "(La evidencia no parece imagen.)"}
        </div>
      )}
    </div>
  );
}

// ---------- Report link ----------
function ReportLink({ reportUrl }) {
  const url = String(reportUrl || "").trim();
  if (!url) return null;

  return (
    <div style={{ marginTop: 10 }}>
      <a
        href={url}
        target="_blank"
        rel="noreferrer"
        style={{
          display: "inline-block",
          fontSize: 12,
          opacity: 0.95,
          color: "white",
          textDecoration: "underline",
        }}
      >
        📄 Descargar reporte
      </a>
    </div>
  );
}