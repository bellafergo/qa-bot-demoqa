// src/components/InitializeProjectPanel.jsx
import React, { useState } from "react";
import { Link } from "react-router-dom";
import { initializeProject, apiErrorMessage } from "../api";
import { useLang } from "../i18n/LangContext";
import InitStepTimeline from "./InitStepTimeline.jsx";
import { INIT_CHECKLIST_KEYS } from "../utils/initStepUtils";

export default function InitializeProjectPanel({
  projectId,
  projectName,
  compact = false,
  onInitialized,
  onDone,
}) {
  const { t } = useLang();
  const [busy, setBusy] = useState(false);
  const [result, setResult] = useState(null);
  const [error, setError] = useState("");

  const handleDone = onInitialized || onDone;

  if (!projectId) return null;

  async function handleInit() {
    if (busy) return;
    setBusy(true);
    setError("");
    setResult(null);
    try {
      const res = await initializeProject(projectId, { run_smoke: true, refresh_knowledge: true });
      setResult(res);
      handleDone?.(res);
    } catch (e) {
      setError(apiErrorMessage(e) || t("init.error"));
    } finally {
      setBusy(false);
    }
  }

  const failed = (result?.steps || []).filter((s) => s.status === "failed");

  return (
    <div
      className="card init-recommended-panel"
      style={{
        padding: compact ? "16px 18px" : "20px 24px",
        marginBottom: compact ? 0 : 20,
        borderColor: "var(--accent-border, var(--border))",
        background: "linear-gradient(180deg, var(--surface) 0%, var(--bg) 100%)",
        boxShadow: "0 1px 0 var(--border)",
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 16, flexWrap: "wrap" }}>
        <div style={{ flex: "1 1 280px", minWidth: 0 }}>
          <div
            style={{
              display: "inline-flex",
              alignItems: "center",
              gap: 8,
              marginBottom: 8,
            }}
          >
            <span
              aria-hidden
              style={{
                fontSize: 10,
                fontWeight: 700,
                letterSpacing: "0.08em",
                textTransform: "uppercase",
                color: "var(--accent)",
                background: "var(--accent-light, rgba(99,102,241,0.12))",
                padding: "3px 8px",
                borderRadius: 4,
              }}
            >
              {t("init.recommended_badge")}
            </span>
          </div>
          <div style={{ fontSize: compact ? 15 : 17, fontWeight: 700, color: "var(--text-1)", letterSpacing: "-0.01em" }}>
            {t("init.recommended_title")}
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", marginTop: 8, marginBottom: 0, maxWidth: 560, lineHeight: 1.55 }}>
            {t("init.recommended_subtitle")}
          </p>
          {projectName ? (
            <p style={{ fontSize: 11, color: "var(--text-4)", marginTop: 6, marginBottom: 0 }}>
              {t("init.scope", { name: projectName })}
            </p>
          ) : null}
        </div>
        <button
          type="button"
          className="btn btn-primary"
          style={{ flexShrink: 0, minWidth: compact ? undefined : 160 }}
          onClick={handleInit}
          disabled={busy}
        >
          {busy ? t("init.working") : t("init.cta")}
        </button>
      </div>

      <ul
        style={{
          margin: "16px 0 0",
          padding: 0,
          listStyle: "none",
          display: "grid",
          gridTemplateColumns: "repeat(auto-fit, minmax(200px, 1fr))",
          gap: 8,
        }}
      >
        {INIT_CHECKLIST_KEYS.map((key) => (
          <li
            key={key}
            style={{
              display: "flex",
              alignItems: "flex-start",
              gap: 8,
              fontSize: 12,
              color: "var(--text-2)",
              lineHeight: 1.45,
              padding: "8px 10px",
              borderRadius: 6,
              background: "var(--surface)",
              border: "1px solid var(--border)",
            }}
          >
            <span aria-hidden style={{ color: busy ? "var(--accent)" : "var(--green)", fontWeight: 700, marginTop: 1 }}>
              {busy ? "◌" : "✓"}
            </span>
            <span>{t(key)}</span>
          </li>
        ))}
      </ul>

      {error ? (
        <div className="alert alert-error" style={{ marginTop: 14, fontSize: 12 }}>{error}</div>
      ) : null}

      {busy && !result ? (
        <div style={{ marginTop: 14, fontSize: 12, color: "var(--text-3)" }}>{t("init.progress_hint")}</div>
      ) : null}

      {result ? (
        <div style={{ marginTop: 14, fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
          <div style={{ marginBottom: 8 }}>
            <span className={result.ok ? "badge badge-green" : "badge badge-orange"}>
              {result.ok ? t("init.status_ok") : t("init.status_partial")}
            </span>
            {" "}
            {result.message}
          </div>
          <InitStepTimeline result={result} />
          {failed.length > 0 ? (
            <p style={{ marginTop: 10, color: "var(--orange-text)" }}>{t("init.partial_hint")}</p>
          ) : null}
          {result.job_id ? (
            <p style={{ marginTop: 8 }}>
              <Link to="/batch" style={{ color: "var(--accent)" }}>{t("init.view_batch")}</Link>
            </p>
          ) : null}
        </div>
      ) : null}
    </div>
  );
}
