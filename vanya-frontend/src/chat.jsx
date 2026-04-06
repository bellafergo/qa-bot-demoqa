// vanya-frontend/src/chat.jsx
import React, { useMemo, useCallback, useState, useEffect } from "react";
import DocArtifactTabs from "./components/DocArtifactTabs";
import { useLang } from "./i18n/LangContext";
import { createTestFromRun, apiErrorMessage } from "./api";

const getRunnerContract = (m) => {
  const meta = getMeta(m);
  const runner = meta?.runner;
  if (runner && typeof runner === "object") return runner;
  return null;
};

/**
 * ✅ PRODUCT READY CHAT
 * - Meta parsing robusto (meta/meta_json/metaJson)
 * - Runner picking + compat aliases
 * - Evidence picking: data-url, b64->data-url, urls
 * - Report picking: url o fallback desde texto si PDF
 * - Exec status real: PASSED / FAILED / PASSED (negativo) / FAILED (debió fallar)
 * - EvidenceBlock: evita blanco si URL inválida, cache-buster solo http(s)
 * - RunDebugBlock: details, copy json
 */

const BUILD_TAG = "product-chat-2025-12-29-01";

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

const hasValidHttpUrl = (u) => {
  const s = String(u || "").trim();
  return s.startsWith("http://") || s.startsWith("https://");
};

const hasValidEvidence = (u) => {
  const s = String(u || "").trim();
  if (!s) return false;
  if (s.startsWith("data:image/")) return true;
  return hasValidHttpUrl(s);
};

const pickRunner = (m) => {
  const meta = getMeta(m);
  const r1 = meta?.runner && typeof meta.runner === "object" ? meta.runner : null;
  const r2 = m?.runner && typeof m.runner === "object" ? m.runner : null;
  const r = r1 || r2 || null;
  return r && typeof r === "object" ? r : null;
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
  const runner = pickRunner(m) || meta?.runner || {};

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
  const runner = pickRunner(m) || meta?.runner || {};

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

/**
 * ✅ Exec status "producto"
 */
const pickExecBadge = (m) => {
  const meta = getMeta(m);
  const runner = pickRunner(m) || meta?.runner || null;
  const txt = String(m?.content || "").toLowerCase();

  if (!runner || typeof runner !== "object") {
    // fallback por texto
    if (txt.includes("(fail)") || txt.includes(" failed") || txt.includes("ejecutado (fail)")) {
      return { label: "FAILED", kind: "bad" };
    }
    if (txt.includes("(pass)") || txt.includes(" passed") || txt.includes("ejecutado (pass)")) {
      return { label: "PASSED", kind: "ok" };
    }
    return null;
  }

  const ok = runner?.ok ?? runner?.success ?? meta?.runner?.ok ?? meta?.runner?.success;
  const statusRaw = String(runner?.status || meta?.runner?.status || "").toLowerCase().trim();
  const expected = String(runner?.expected || "").toLowerCase().trim() || "pass";
  const outcome = String(runner?.outcome || "").toLowerCase().trim() || (ok === false ? "fail" : "pass");

  if (statusRaw.includes("passed")) {
    if (expected === "fail" && outcome === "fail") return { label: "PASSED (negativo)", kind: "ok" };
    return { label: "PASSED", kind: "ok" };
  }
  if (statusRaw.includes("failed")) {
    if (expected === "fail" && outcome === "pass") return { label: "FAILED (debió fallar)", kind: "bad" };
    return { label: "FAILED", kind: "bad" };
  }
  if (statusRaw.includes("error")) return { label: "ERROR", kind: "bad" };

  if (ok === true) {
    if (expected === "fail") return { label: "FAILED (debió fallar)", kind: "bad" };
    return { label: "PASSED", kind: "ok" };
  }
  if (ok === false) {
    if (expected === "fail") return { label: "PASSED (negativo)", kind: "ok" };
    return { label: "FAILED", kind: "bad" };
  }

  if (txt.includes("(fail)") || txt.includes(" failed") || txt.includes("ejecutado (fail)")) {
    if (expected === "fail") return { label: "PASSED (negativo)", kind: "ok" };
    return { label: "FAILED", kind: "bad" };
  }
  if (txt.includes("(pass)") || txt.includes(" passed") || txt.includes("ejecutado (pass)")) {
    if (expected === "fail") return { label: "FAILED (debió fallar)", kind: "bad" };
    return { label: "PASSED", kind: "ok" };
  }

  return null;
};

const badgeColor = (kind) =>
  kind === "bad" ? "#b91c1c" : kind === "ok" ? "#15803d" : "#64748b";

export default function Chat(props) {
  const { t } = useLang();
  const {
    messages = [],
    input = "",
    setInput = () => {},
    handleSend = () => {},
    isLoading = false,
    sessionId = null,
    threadId = null,
    formatText = (fmt) => (typeof fmt === "string" ? fmt : ""),
    chatEndRef = null,
    projectId = "default",
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

  const renderMsg = (m, idx) => {
    const role = m?.role === "user" ? "user" : "bot";
    const isBot = role === "bot";
    const content = typeof m?.content === "string" ? m.content : "";
    const html = formatText(content);

    const meta = getMeta(m);
    const key = String(m?.id || meta?.id || `${role}-${idx}`);

    // Runner + meta normalizados
    const runner = isBot ? pickRunner(m) : null;

    // URLs / data ya normalizados
    const evidenceUrl = isBot ? pickEvidenceUrl(m) : null;
    const reportUrl = isBot ? pickReportUrl(m) : null;
    const evidenceId = isBot ? pickEvidenceId(m) : null;
    const docJson = isBot ? pickDocJson(m) : null;

    const badge = isBot ? pickExecBadge(m) : null;
    const showRunDebug = isBot && runner && (Array.isArray(runner.steps) || Array.isArray(runner.logs));

    const showEvidence = isBot && hasValidEvidence(evidenceUrl);
    const showReport = isBot && hasValidHttpUrl(reportUrl);
    const showDocTabs = isBot && !!docJson;

    const reason =
      (runner && typeof runner.reason === "string" && runner.reason.trim()) ||
      (meta?.runner && typeof meta.runner.reason === "string" && meta.runner.reason.trim()) ||
      "";

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
            padding: "12px 16px",
            borderRadius: 14,
            background: role === "user" ? "var(--bubble-user-bg)" : "var(--bubble-bot-bg)",
            border: `1px solid ${role === "user" ? "var(--bubble-user-border)" : "var(--bubble-bot-border)"}`,
            color: "var(--bubble-text)",
            wordBreak: "break-word",
            boxShadow: "var(--shadow-1)",
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
            <span style={{ color: "var(--bubble-meta)", fontWeight: 500, fontSize: 11, textTransform: "uppercase", letterSpacing: "0.05em" }}>{isBot ? t("chat.bubble.bot") : t("chat.bubble.user")}</span>

            {badge ? (
              <span
                style={{
                  padding: "2px 8px",
                  borderRadius: 999,
                  fontWeight: 500,
                  fontSize: 11,
                  color: badgeColor(badge.kind),
                  border: `1px solid ${badgeColor(badge.kind)}`,
                  textTransform: "uppercase",
                }}
                title={reason || ""}
              >
                {badge.label}
              </span>
            ) : null}

            {evidenceId ? <span style={{ color: "var(--bubble-meta)", fontSize: 11 }}>· evid: {evidenceId}</span> : null}
          </div>

          {/* Reason corto si existe */}
          {isBot && reason ? (
            <div style={{ fontSize: 12, color: "var(--bubble-meta)", marginBottom: content ? 8 : 0 }}>{reason}</div>
          ) : null}

          {/* Texto */}
          {content ? (
            <div dangerouslySetInnerHTML={{ __html: html }} style={{ lineHeight: 1.35 }} />
          ) : null}

          {/* DOC Tabs */}
          {showDocTabs ? <DocArtifactTabs doc={docJson} /> : null}

          {/* Evidencia */}
          {showEvidence ? <EvidenceBlock evidenceUrl={evidenceUrl} /> : null}

          {/* Reporte */}
          {showReport ? <ReportBlock reportUrl={reportUrl} /> : null}

          {/* Debug */}
          {showRunDebug ? <RunDebugBlock runner={runner} projectId={projectId} /> : null}
        </div>
      </div>
    );
  };

  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column", background: "var(--chat-bg)" }}>
      <div style={{ fontSize: 10, color: "var(--text-4)", padding: "6px 20px", borderBottom: "1px solid var(--border-light)" }}>build: {BUILD_TAG}</div>

      <div style={{ flex: 1, overflow: "auto", padding: "20px 24px" }}>
        <div style={{ maxWidth: 800, margin: "0 auto" }}>
        {!threadId ? (
          <div style={{ color: "var(--text-3)", padding: "24px 0", textAlign: "center", fontSize: 14 }}>
            {t("chat.no_thread")}
          </div>
        ) : null}

        {safeMessages.map(renderMsg)}
        <div ref={chatEndRef || undefined} />
        </div>
      </div>

      <div
        style={{
          padding: "12px 24px 16px",
          borderTop: "1px solid var(--border)",
          background: "var(--surface)",
        }}
      >
        <div style={{ maxWidth: 800, margin: "0 auto", display: "flex", gap: 10, alignItems: "flex-end" }}>
          <textarea
            value={input}
            onChange={(e) => setInput?.(e.target.value)}
            onKeyDown={onEnter}
            placeholder={t("chat.input.placeholder")}
            rows={1}
            style={{
              flex: 1,
              resize: "none",
              padding: "12px 16px",
              borderRadius: 12,
              border: "1px solid var(--chat-input-border)",
              background: "var(--chat-input-bg)",
              color: "var(--text-1)",
              outline: "none",
              fontFamily: "inherit",
              fontSize: 14,
              lineHeight: 1.5,
              boxShadow: "var(--shadow-1)",
              transition: "border-color 0.15s, box-shadow 0.15s",
            }}
            onFocus={e => { e.target.style.borderColor = "var(--border-focus)"; e.target.style.boxShadow = "none"; e.target.style.outline = "2px solid var(--accent-border)"; e.target.style.outlineOffset = "1px"; }}
            onBlur={e => { e.target.style.borderColor = "var(--chat-input-border)"; e.target.style.boxShadow = "var(--shadow-1)"; e.target.style.outline = "none"; }}
            disabled={isLoading}
          />

          <button
            onClick={() => handleSend?.()}
            disabled={isLoading || !String(input || "").trim()}
            style={{
              padding: "10px 18px",
              borderRadius: 10,
              border: "none",
              background: isLoading || !String(input || "").trim() ? "var(--surface-3)" : "var(--accent)",
              color: isLoading || !String(input || "").trim() ? "var(--text-3)" : "#ffffff",
              cursor: isLoading || !String(input || "").trim() ? "not-allowed" : "pointer",
              fontWeight: 500,
              fontSize: 13,
              fontFamily: "inherit",
              flexShrink: 0,
              transition: "background 0.15s",
              boxShadow: isLoading || !String(input || "").trim() ? "none" : "var(--shadow-1)",
            }}
            title={sessionId ? `session: ${sessionId}` : ""}
          >
            {isLoading ? "…" : t("chat.send")}
          </button>
        </div>
      </div>
    </div>
  );
}

// ---------- Evidence component ----------
function EvidenceBlock({ evidenceUrl }) {
  const { t } = useLang();
  const [failed, setFailed] = useState(false);

  const url = String(evidenceUrl || "").trim();
  if (!url) return null;

  const lower = url.toLowerCase();

  const looksImage =
    lower.startsWith("data:image/") ||
    /\.(png|jpe?g|webp|gif)(\?.*)?$/i.test(lower) ||
    lower.includes("res.cloudinary.com") ||
    lower.includes("/image/upload");

  const imgSrc = (() => {
    if (!url) return "";
    if (lower.startsWith("data:image/")) return url;
    if (!hasValidHttpUrl(url)) return url;
    return url.includes("?") ? `${url}&cb=${Date.now()}` : `${url}?cb=${Date.now()}`;
  })();

  const canRenderImg = looksImage && !failed;

  return (
    <div style={{ marginTop: 10 }}>
      <div style={{ fontSize: 11, color: "var(--bubble-meta)", fontWeight: 400, textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 8 }}>{t("chat.screenshot")}</div>

      {canRenderImg ? (
        <img
          src={imgSrc}
          alt="evidence"
          style={{
            width: "100%",
            maxWidth: 720,
            borderRadius: 10,
            border: "1px solid var(--border)",
            boxShadow: "var(--shadow-2)",
          }}
          loading="lazy"
          onError={() => setFailed(true)}
        />
      ) : (
        <div style={{ fontSize: 13 }}>
          <a
            href={hasValidHttpUrl(url) ? url : undefined}
            target="_blank"
            rel="noreferrer"
            style={{ color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}
            onClick={(e) => { if (!hasValidHttpUrl(url)) e.preventDefault(); }}
            title={url}
          >
            {t("chat.evidence_open")}
          </a>
          {!hasValidHttpUrl(url) ? (
            <div style={{ marginTop: 6, color: "var(--text-3)", wordBreak: "break-all", fontSize: 12 }}>{url}</div>
          ) : null}
        </div>
      )}
    </div>
  );
}

function RunDebugBlock({ runner, projectId = "default" }) {
  const { t } = useLang();
  const [saveBusy, setSaveBusy] = useState(false);
  const [saveOk, setSaveOk] = useState(false);

  useEffect(() => {
    if (!saveOk) return;
    const id = setTimeout(() => setSaveOk(false), 5000);
    return () => clearTimeout(id);
  }, [saveOk]);

  if (!runner || typeof runner !== "object") return null;

  const steps = Array.isArray(runner.steps) ? runner.steps : [];
  const logs = Array.isArray(runner.logs) ? runner.logs : [];
  const runIdForCatalog = runner.evidence_id || runner.run_id;

  const saveToCatalog = async () => {
    if (!runIdForCatalog) return;
    const defaultName = `Chat ${String(runIdForCatalog).slice(0, 12)}`;
    const name = window.prompt(t("catalog.from_run.prompt_name"), defaultName);
    if (name == null) return;
    const trimmed = String(name).trim();
    if (!trimmed) {
      window.alert(t("catalog.from_run.name_required"));
      return;
    }
    setSaveBusy(true);
    setSaveOk(false);
    try {
      await createTestFromRun({
        run_id: runIdForCatalog,
        name: trimmed,
        project_id: String(projectId || "default").trim() || "default",
      });
      setSaveOk(true);
    } catch (e) {
      window.alert(apiErrorMessage(e));
    } finally {
      setSaveBusy(false);
    }
  };

  const copyJson = async () => {
    try {
      await navigator.clipboard.writeText(JSON.stringify(runner, null, 2));
      alert("✅ Run JSON copiado");
    } catch {
      alert("No se pudo copiar (permisos del navegador).");
    }
  };

  return (
    <div style={{ marginTop: 12 }}>
      <details style={{ border: "1px solid var(--border)", borderRadius: 10, overflow: "hidden" }}>
        <summary style={{ cursor: "pointer", fontWeight: 500, fontSize: 12, padding: "8px 12px", background: "var(--surface-2)", color: "var(--text-3)", userSelect: "none" }}>
          Run details
        </summary>

        <div style={{ padding: "10px 12px" }}>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 10, alignItems: "center" }}>
            <button
              onClick={copyJson}
              style={{
                padding: "5px 10px",
                borderRadius: 6,
                border: "1px solid var(--border)",
                background: "var(--surface)",
                color: "var(--text-2)",
                cursor: "pointer",
                fontWeight: 500,
                fontSize: 12,
                fontFamily: "inherit",
              }}
            >
              Copy JSON
            </button>

            {runIdForCatalog ? (
              <button
                type="button"
                onClick={saveToCatalog}
                disabled={saveBusy}
                style={{
                  padding: "5px 10px",
                  borderRadius: 6,
                  border: "1px solid var(--accent-border)",
                  background: "var(--accent-light)",
                  color: "var(--accent)",
                  cursor: saveBusy ? "wait" : "pointer",
                  fontWeight: 600,
                  fontSize: 12,
                  fontFamily: "inherit",
                }}
              >
                {saveBusy ? t("catalog.from_run.saving") : t("catalog.from_run.save_btn")}
              </button>
            ) : null}
            {saveOk ? (
              <span style={{ fontSize: 12, color: "var(--green-text)", fontWeight: 600 }}>
                {t("catalog.from_run.success")}
              </span>
            ) : null}

            {runner?.evidence_id && <span style={{ fontSize: 11, color: "var(--text-3)", fontFamily: "monospace" }}>id: {runner.evidence_id}</span>}
            {runner?.status     && <span style={{ fontSize: 11, color: "var(--text-3)" }}>status: {String(runner.status)}</span>}
            {runner?.expected   && <span style={{ fontSize: 11, color: "var(--text-3)" }}>expected: {String(runner.expected)}</span>}
            {runner?.outcome    && <span style={{ fontSize: 11, color: "var(--text-3)" }}>outcome: {String(runner.outcome)}</span>}
          </div>

          {steps.length ? (
            <div style={{ marginBottom: 10 }}>
              <div style={{ fontWeight: 500, fontSize: 11, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em", marginBottom: 6 }}>{t("chat.run.steps")}</div>
              <div style={{ overflowX: "auto", border: "1px solid var(--border)", borderRadius: 6 }}>
                <table className="data-table" style={{ fontSize: 12 }}>
                  <thead>
                    <tr>
                      <th>#</th>
                      <th>{t("chat.run.col.action")}</th>
                      <th>{t("chat.run.col.selector")}</th>
                      <th>{t("chat.run.col.status")}</th>
                      <th>{t("chat.run.col.error")}</th>
                    </tr>
                  </thead>
                  <tbody>
                    {steps.map((s, i) => (
                      <tr key={i}>
                        <td style={{ color: "var(--text-3)" }}>{s.index ?? i + 1}</td>
                        <td><code style={{ fontSize: 11 }}>{String(s.action || "")}</code></td>
                        <td style={{ color: "var(--text-2)", maxWidth: 220, wordBreak: "break-all" }}>
                          {s.url ? `url: ${s.url}` : s.selector ? `sel: ${s.selector}` : "—"}
                        </td>
                        <td style={{ color: String(s.status || "").toLowerCase().includes("pass") ? "var(--green)" : String(s.status || "").toLowerCase().includes("fail") ? "var(--red)" : "var(--text-3)", fontWeight: 500 }}>{String(s.status || "—")}</td>
                        <td style={{ color: "var(--red-text)", fontSize: 11 }}>{s.error ? String(s.error) : ""}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          ) : null}

          {logs.length ? (
            <div>
              <div style={{ fontWeight: 500, fontSize: 11, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em", marginBottom: 6 }}>{t("chat.run.logs")}</div>
              <pre className="code-block" style={{ fontSize: 11, maxHeight: 200, overflow: "auto", margin: 0 }}>
                {logs.join("\n")}
              </pre>
            </div>
          ) : null}
        </div>
      </details>
    </div>
  );
}

// ---------- Report component ----------
function ReportBlock({ reportUrl }) {
  const { t } = useLang();
  const url = String(reportUrl || "").trim();
  if (!url) return null;

  const safe = hasValidHttpUrl(url);

  return (
    <div style={{ marginTop: 10 }}>
      <div style={{ fontSize: 12, opacity: 0.85, marginBottom: 6 }}>{t("chat.report.title")}</div>

      <div style={{ display: "flex", gap: 10, alignItems: "center", flexWrap: "wrap" }}>
        <a
          href={safe ? url : undefined}
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
            fontWeight: 500,
            fontSize: 12,
          }}
          onClick={(e) => {
            if (!safe) e.preventDefault();
          }}
        >
          {t("chat.report.open")}
        </a>

        <span style={{ fontSize: 12, opacity: 0.75, wordBreak: "break-all" }}>{url}</span>
      </div>
    </div>
  );
}
