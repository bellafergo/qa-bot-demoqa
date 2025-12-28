// vanya-frontend/src/chat.jsx
import React, { useMemo, useCallback, useState } from "react";
import DocArtifactTabs from "./components/DocArtifactTabs";

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

  const pickDocJson = (m) => {
    if (!m) return null;

    // 1) top-level (backend puede enviarlo así)
    if (m.doc_json && typeof m.doc_json === "object") return m.doc_json;

    // 2) dentro de meta
    const meta = getMeta(m);
    if (meta?.doc_json && typeof meta.doc_json === "object") return meta.doc_json;

    // 3) por si algún día viene como string JSON
    const parsed = safeJsonParse(meta?.doc_json);
    if (parsed && typeof parsed === "object") return parsed;

    return null;
  };

  const extractUrlFromText = (content) => {
    const text = typeof content === "string" ? content : "";

    // Report: / Reporte:
    let m = text.match(/Report(?:e)?\s*:\s*(https?:\/\/\S+)/i);
    if (m && m[1]) return m[1].trim();

    // Evidence:
    m = text.match(/Evidence\s*:\s*(https?:\/\/\S+)/i);
    if (m && m[1]) return m[1].trim();

    // cualquier URL
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

  const isProbablyPdfUrl = (u) => {
    const s = String(u || "").toLowerCase().trim();
    if (!s) return false;
    if (s.includes("res.cloudinary.com") && s.includes("/raw/upload")) return true;
    if (s.endsWith(".pdf") || s.includes(".pdf?")) return true;
    return false;
  };

  const pickRunner = (m) => {
    const meta = getMeta(m);
    const r1 = meta?.runner && typeof meta.runner === "object" ? meta.runner : null;
    const r2 = m?.runner && typeof m.runner === "object" ? m.runner : null;
    return r1 || r2 || {};
  };

  const isExecuteMessage = (m) => {
    const meta = getMeta(m);
    const mode = String(m?.mode || meta?.mode || meta?.runner?.mode || "")
      .trim()
      .toLowerCase();
    return mode.includes("execute");
  };

  const pickEvidenceId = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    const candidates = [
      runner?.evidence_id,
      runner?.evidenceId,
      meta?.evidence_id,
      meta?.evidenceId,
      meta?.runner?.evidence_id,
      meta?.runner?.evidenceId,
      m?.evidence_id,
      m?.evidenceId,
    ].filter((x) => typeof x === "string" && x.trim());

    return candidates.length ? candidates[0].trim() : null;
  };

  const pickEvidenceUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    // ✅ Prioridad 1: screenshot_data_url directo
    const dataUrl =
      runner?.screenshot_data_url ||
      runner?.screenshotDataUrl ||
      meta?.screenshot_data_url ||
      meta?.screenshotDataUrl ||
      meta?.runner?.screenshot_data_url ||
      meta?.runner?.screenshotDataUrl ||
      null;

    if (typeof dataUrl === "string" && dataUrl.trim()) return dataUrl.trim();

    // ✅ Prioridad 2: base64 -> data url
    const b64 =
      runner?.screenshot_b64 ||
      runner?.screenshotB64 ||
      meta?.screenshot_b64 ||
      meta?.screenshotB64 ||
      meta?.runner?.screenshot_b64 ||
      meta?.runner?.screenshotB64 ||
      null;

    const asData = toDataUrl(b64);
    if (typeof asData === "string" && asData.trim()) return asData.trim();

    // ✅ Prioridad 3: URLs típicas
    const candidates = [
      runner?.evidence_url,
      runner?.evidenceUrl,
      runner?.screenshot_url,
      runner?.screenshotUrl,

      meta?.evidence_url,
      meta?.evidenceUrl,
      meta?.screenshot_url,
      meta?.screenshotUrl,

      meta?.runner?.evidence_url,
      meta?.runner?.evidenceUrl,
      meta?.runner?.screenshot_url,
      meta?.runner?.screenshotUrl,

      m?.evidence_url,
      m?.evidenceUrl,
      m?.screenshot_url,
      m?.screenshotUrl,
    ].filter((x) => typeof x === "string" && x.trim());

    if (candidates.length) return candidates[0].trim();

    // ✅ fallback: intenta sacarlo del texto
    const fromText = extractUrlFromText(m?.content);
    if (fromText) return fromText.trim();

    return null;
  };

  const pickReportUrl = (m) => {
    const meta = getMeta(m);
    const runner = pickRunner(m);

    const candidates = [
      m?.report_url,
      m?.reportUrl,

      meta?.report_url,
      meta?.reportUrl,

      runner?.report_url,
      runner?.reportUrl,

      meta?.runner?.report_url,
      meta?.runner?.reportUrl,
    ].filter((x) => typeof x === "string" && x.trim());

    if (candidates.length) return candidates[0].trim();

    // fallback desde texto
    const fromText = extractUrlFromText(m?.content);
    if (fromText && isProbablyPdfUrl(fromText)) return fromText.trim();

    return null;
  };

  const pickExecStatus = (m) => {
    // Detecta PASS/FAIL robusto (por runner/meta/content)
    const meta = getMeta(m);
    const runner = pickRunner(m);
    const txt = String(m?.content || "").toLowerCase();

    // 1) campos estructurados (si existen)
    const ok = runner?.ok ?? runner?.success ?? meta?.runner?.ok ?? meta?.runner?.success;
    if (ok === true) return "PASSED";
    if (ok === false) return "FAILED";

    const status = String(runner?.status || meta?.runner?.status || "").toLowerCase().trim();
    if (status.includes("pass")) return "PASSED";
    if (status.includes("fail")) return "FAILED";

    // 2) fallback por texto
    if (txt.includes("(fail)") || txt.includes(" fail") || txt.includes("failed")) return "FAILED";
    if (txt.includes("(pass)") || txt.includes(" pass") || txt.includes("passed")) return "PASSED";

    return null;
  };

  const statusColor = (status) =>
    status === "FAILED"
      ? "#ff4d4f"
      : status === "PASSED"
      ? "#52c41a"
      : "rgba(255,255,255,0.75)";

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const evidenceUrl = pickEvidenceUrl(m);
    const evidenceId = pickEvidenceId(m);
    const reportUrl = pickReportUrl(m);
    const docJson = pickDocJson(m);

    const isExecute = isExecuteMessage(m);

    const showEvidence = !!evidenceUrl;
    const showReport   = !!reportUrl;

    const meta = getMeta(m);
    const key = String(m?.id || meta?.id || `${role}-${idx}`);

    const showDocTabs = role === "bot" && !!docJson;

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
          {/* Header */}
          <div
            style={{
              fontSize: 12,
              marginBottom: 6,
              display: "flex",
              gap: 8,
              alignItems: "center",
              flexWrap: "wrap",
            }}
          >
            <span style={{ opacity: 0.75 }}>{role === "user" ? "Tú" : "Vanya"}</span>

            {execStatus ? (
              <span
                style={{
                  padding: "2px 8px",
                  borderRadius: 999,
                  fontWeight: 900,
                  fontSize: 11,
                  color: statusColor(execStatus),
                  border: `1px solid ${statusColor(execStatus)}`,
                  textTransform: "uppercase",
                }}
              >
                {execStatus}
              </span>
            ) : null}

            {evidenceId ? <span style={{ opacity: 0.7 }}>· evid: {evidenceId}</span> : null}
          </div>

          {/* Texto (si hay doc_json, el texto sirve como summary; se mantiene) */}
          {content ? (
            <div
              dangerouslySetInnerHTML={{ __html: html }}
              style={{ lineHeight: 1.35 }}
            />
          ) : null}

          {/* DOC Tabs (Executive / QA) */}
          {showDocTabs ? <DocArtifactTabs doc={docJson} /> : null}

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
  const looksImage =
    lower.startsWith("data:image/") ||
    /\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(lower) ||
    lower.includes("res.cloudinary.com") ||
    lower.includes("/image/upload");

  // Cache-buster SOLO para http(s); para data:image NO
  const imgSrc = (() => {
    if (!url) return "";
    if (lower.startsWith("data:image/")) return url;
    return url.includes("?") ? `${url}&cb=${Date.now()}` : `${url}?cb=${Date.now()}`;
  })();

  return (
    <div style={{ marginTop: 10 }}>
      <div style={{ fontSize: 12, opacity: 0.85, marginBottom: 6 }}>
        Evidencia (captura)
      </div>

      {looksImage && !failed ? (
        <img
          src={imgSrc}
          alt="evidence"
          style={{
            width: "100%",
            maxWidth: 720,
            borderRadius: 12,
            border: "1px solid rgba(255,255,255,0.10)",
            background: "rgba(0,0,0,0.25)",
          }}
          onError={() => setFailed(true)}
        />
      ) : (
        <div style={{ fontSize: 12, opacity: 0.85 }}>
          <a
            href={url}
            target="_blank"
            rel="noreferrer"
            style={{ color: "rgba(160,200,255,0.95)" }}
          >
            Abrir evidencia
          </a>
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
    <div style={{ marginTop: 10 }}>
      <div style={{ fontSize: 12, opacity: 0.85, marginBottom: 6 }}>
        Reporte (PDF)
      </div>

      <div style={{ display: "flex", gap: 10, alignItems: "center", flexWrap: "wrap" }}>
        <a
          href={url}
          target="_blank"
          rel="noreferrer"
          style={{
            display: "inline-block",
            padding: "8px 10px",
            borderRadius: 10,
            border: "1px solid rgba(255,255,255,0.14)",
            background: "rgba(0,0,0,0.25)",
            color: "rgba(160,200,255,0.95)",
            textDecoration: "none",
            fontWeight: 700,
            fontSize: 12,
          }}
        >
          Abrir reporte
        </a>

        <span style={{ fontSize: 12, opacity: 0.75, wordBreak: "break-all" }}>
          {url}
        </span>
      </div>
    </div>
  );
}