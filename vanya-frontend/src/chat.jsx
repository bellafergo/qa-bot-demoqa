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

  // -------------------- helpers --------------------
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
    if (m?.meta && typeof m.meta === "object") return m.meta;
    const parsed = safeJsonParse(m?.meta_json) || safeJsonParse(m?.metaJson);
    if (parsed && typeof parsed === "object") return parsed;
    if (m?.meta_json && typeof m.meta_json === "object") return m.meta_json;
    return {};
  };

  const extractUrlFromText = (content) => {
    const text = typeof content === "string" ? content : "";

    // Report/Reporte:
    let m = text.match(/Report(?:e)?\s*:\s*(https?:\/\/\S+)/i);
    if (m && m[1]) return m[1].trim();

    // Evidence/Evidencia:
    m = text.match(/Evidenc(?:e|ia)\s*:\s*(https?:\/\/\S+)/i);
    if (m && m[1]) return m[1].trim();

    // cualquier url
    m = text.match(/(https?:\/\/[^\s)]+)\b/i);
    if (m && m[1]) return m[1].trim();

    return null;
  };

  const isProbablyImageUrl = (url) => {
    const u = String(url || "").toLowerCase();
    if (u.startsWith("data:image/")) return true;
    if (/\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(u)) return true;
    if (u.includes("res.cloudinary.com") && u.includes("/image/upload")) return true;
    return false;
  };

  const isProbablyPdfUrl = (url) => {
    const u = String(url || "").toLowerCase();
    if (u.endsWith(".pdf")) return true;
    // cloudinary raw
    if (u.includes("res.cloudinary.com") && u.includes("/raw/upload")) return true;
    return false;
  };

  const isExecuteMessage = (m) => {
    const meta = getMeta(m);
    const mode = String(meta?.mode || m?.mode || "").trim().toLowerCase();
    return mode === "execute" || mode === "execute_mode";
  };

  const pickRunner = (m) => {
    const meta = getMeta(m);
    return meta?.runner || m?.runner || {};
  };

  const pickEvidenceId = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);
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

  const pickEvidenceUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    const candidates = [
      // meta directo
      meta?.secure_url,
      meta?.image_url,
      meta?.evidence_url,
      meta?.screenshot_url,
      meta?.screenshot_data_url,
      meta?.evidenceUrl,
      meta?.screenshotUrl,
      meta?.screenshotDataUrl,

      // runner
      runner?.secure_url,
      runner?.image_url,
      runner?.evidence_url,
      runner?.screenshot_url,
      runner?.screenshot_data_url,
      runner?.evidenceUrl,
      runner?.screenshotUrl,
      runner?.screenshotDataUrl,

      // plano
      m?.secure_url,
      m?.image_url,
      m?.evidence_url,
      m?.screenshot_url,
      m?.screenshot_data_url,
    ].filter((x) => typeof x === "string" && x.trim());

    if (candidates.length) return candidates[0].trim();

    // fallback texto (si viene Evidence: https://...)
    const fromText = extractUrlFromText(m?.content);
    if (fromText && isProbablyImageUrl(fromText)) return fromText.trim();

    return null;
  };

  const pickReportUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    const candidates = [
      meta?.report_url,
      meta?.reportUrl,
      runner?.report_url,
      runner?.reportUrl,
      m?.report_url,
      m?.reportUrl,
    ].filter((x) => typeof x === "string" && x.trim());

    if (candidates.length) return candidates[0].trim();

    // fallback si alguien lo pega al texto
    const fromText = extractUrlFromText(m?.content);
    if (fromText && isProbablyPdfUrl(fromText)) return fromText.trim();

    return null;
  };

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const meta = getMeta(m);
    const runner = pickRunner(m);

    const evidenceId = pickEvidenceId(m);
    const evidenceUrl = pickEvidenceUrl(m);
    const reportUrl = pickReportUrl(m);

    const isExecute = isExecuteMessage(m);

    // muestra artefactos si es execute o si vienen (robustez)
    const showEvidence = !!evidenceUrl && (isExecute || role === "bot");
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

          <div
            dangerouslySetInnerHTML={{ __html: html }}
            style={{ lineHeight: 1.35 }}
          />

          {showEvidence ? <EvidenceBlock evidenceUrl={evidenceUrl} /> : null}
          {showReport ? <ReportLink reportUrl={reportUrl} /> : null}

          {/* debug opcional (si quieres verlo rápido) */}
          {meta?.debug ? (
            <pre style={{ fontSize: 11, opacity: 0.7, marginTop: 10 }}>
              {JSON.stringify({ meta, runner }, null, 2)}
            </pre>
          ) : null}
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

  const looksImage =
    lower.startsWith("data:image/") ||
    /\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(lower) ||
    (lower.includes("res.cloudinary.com") && lower.includes("/image/upload"));

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
