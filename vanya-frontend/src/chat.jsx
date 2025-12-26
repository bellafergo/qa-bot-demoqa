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

  const extractUrlFromText = (content) => {
    const text = typeof content === "string" ? content : "";
    let m = text.match(/Report(?:e)?\s*:\s*(https?:\/\/\S+)/i);
    if (m && m[1]) return m[1].trim();

    m = text.match(/(https?:\/\/[^\s)]+)\b/i);
    if (m && m[1]) return m[1].trim();

    return null;
  };

  const toDataUrl = (b64) => {
    const s = String(b64 || "").trim();
    if (!s) return null;
    if (s.startsWith("data:image/")) return s;
    return "data:image/png;base64," + s;
  };

  const isProbablyPdfUrl = (url) => {
    const u = String(url || "").toLowerCase();
    if (!u) return false;
    if (u.endsWith(".pdf")) return true;
    // Cloudinary RAW suele verse así:
    if (u.includes("res.cloudinary.com") && u.includes("/raw/upload")) return true;
    return false;
  };

  const pickRunner = (m) => {
    const meta = getMeta(m);
    const r1 =
      meta?.runner && typeof meta.runner === "object" ? meta.runner : null;
    const r2 = m?.runner && typeof m.runner === "object" ? m.runner : null;
    return r1 || r2 || {};
  };

  const pickEvidenceUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    // ✅ Prioridad 1: Data URL directo del backend
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
      runner?.screenshot_base64 ||
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
      meta?.secure_url,
      meta?.image_url,
      meta?.evidence_url,
      meta?.screenshot_url,
      meta?.evidenceUrl,
      meta?.screenshotUrl,

      runner?.secure_url,
      runner?.image_url,
      runner?.screenshot_url,
      runner?.evidence_url,
      runner?.screenshotUrl,
      runner?.evidenceUrl,

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

  const pickReportUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    const candidates = [
      // top-level response guardada en message
      m?.report_url,
      m?.reportUrl,

      // meta directo
      meta?.report_url,
      meta?.reportUrl,

      // meta.runner / runner
      runner?.report_url,
      runner?.reportUrl,
      meta?.runner?.report_url,
      meta?.runner?.reportUrl,
    ].filter((x) => typeof x === "string" && x.trim());

    if (candidates.length) return candidates[0].trim();

    // fallback texto si parece pdf
    const fromText = extractUrlFromText(m?.content);
    if (fromText && isProbablyPdfUrl(fromText)) return fromText.trim();

    return null;
  };

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const evidenceUrl = pickEvidenceUrl(m);
    const evidenceId = pickEvidenceId(m);
    const reportUrl = pickReportUrl(m);

    const isExecute = isExecuteMessage(m);

    // ✅ evidencia: si hay URL, muéstrala (sobre todo en execute)
    const showEvidence = !!evidenceUrl && (isExecute || role === "bot");
    // ✅ reporte: SOLO si es execute (para no ensuciar mensajes normales)
    const showReport = !!reportUrl && isExecute;

    const meta = getMeta(m);
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

          {/* Reporte PDF */}
          {showReport ? <ReportBlock reportUrl={reportUrl} /> : null}
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
  if (!url) return null;

  const lower = url.toLowerCase();

  const looksImage = (() => {
    if (lower.startsWith("data:image/")) return true;
    if (/\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(lower)) return true;
    if (lower.includes("res.cloudinary.com") || lower.includes("/image/upload"))
      return true;
    return false;
  })();

  // Cache-buster SOLO para URLs http(s); para data:image NO
  const imgSrc = (() => {
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

// ---------- Report component ----------
function ReportBlock({ reportUrl }) {
  const url = String(reportUrl || "").trim();
  if (!url) return null;

  return (
    <div
      style={{
        marginTop: 12,
        padding: "10px 12px",
        borderRadius: 12,
        border: "1px solid rgba(255,255,255,0.14)",
        background: "rgba(0,0,0,0.18)",
      }}
    >
      <div style={{ fontSize: 12, opacity: 0.85, marginBottom: 8 }}>
        📄 Reporte PDF
      </div>

      <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
        <a
          href={url}
          target="_blank"
          rel="noreferrer"
          style={{
            padding: "8px 10px",
            borderRadius: 10,
            border: "1px solid rgba(255,255,255,0.16)",
            background: "rgba(120,160,255,0.25)",
            color: "white",
            textDecoration: "none",
            fontWeight: 700,
            fontSize: 13,
          }}
        >
          Abrir reporte
        </a>

        {/* Nota: download cross-domain a veces no descarga “bonito”, pero sirve en varios casos */}
        <a
          href={url}
          download
          style={{
            padding: "8px 10px",
            borderRadius: 10,
            border: "1px solid rgba(255,255,255,0.16)",
            background: "rgba(255,255,255,0.08)",
            color: "white",
            textDecoration: "none",
            fontWeight: 700,
            fontSize: 13,
          }}
        >
          Descargar
        </a>
      </div>

      <div style={{ marginTop: 8, fontSize: 11, opacity: 0.65 }}>
        {url}
      </div>
    </div>
  );
}