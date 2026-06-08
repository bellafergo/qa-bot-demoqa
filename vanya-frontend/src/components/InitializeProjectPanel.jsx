// src/components/InitializeProjectPanel.jsx
import React, { useState } from "react";
import { Link } from "react-router-dom";
import { initializeProject, apiErrorMessage } from "../api";
import { useLang } from "../i18n/LangContext";

export default function InitializeProjectPanel({
  projectId,
  projectName,
  compact = false,
  onDone,
}) {
  const { t } = useLang();
  const [busy, setBusy] = useState(false);
  const [result, setResult] = useState(null);
  const [error, setError] = useState("");

  if (!projectId) return null;

  async function handleInit() {
    if (busy) return;
    setBusy(true);
    setError("");
    setResult(null);
    try {
      const res = await initializeProject(projectId, { run_smoke: true, refresh_knowledge: true });
      setResult(res);
      onDone?.(res);
    } catch (e) {
      setError(apiErrorMessage(e) || t("init.error"));
    } finally {
      setBusy(false);
    }
  }

  const failed = (result?.steps || []).filter((s) => s.status === "failed");

  return (
    <div
      className="card"
      style={{
        padding: compact ? "14px 18px" : "18px 22px",
        marginBottom: compact ? 0 : 20,
        borderColor: "var(--accent-border, var(--border))",
        background: "var(--surface)",
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 16, flexWrap: "wrap" }}>
        <div>
          <div style={{ fontSize: compact ? 13 : 15, fontWeight: 600, color: "var(--text-1)" }}>
            {t("init.title")}
          </div>
          <p style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6, marginBottom: 0, maxWidth: 520, lineHeight: 1.5 }}>
            {t("init.subtitle", { name: projectName || projectId })}
          </p>
        </div>
        <button type="button" className="btn btn-primary btn-sm" onClick={handleInit} disabled={busy}>
          {busy ? t("init.working") : t("init.cta")}
        </button>
      </div>

      {error ? (
        <div className="alert alert-error" style={{ marginTop: 12, fontSize: 12 }}>{error}</div>
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
          <ul style={{ margin: 0, paddingLeft: 18 }}>
            {(result.steps || []).map((s) => (
              <li key={s.step}>
                <strong>{s.step}</strong>: {s.status} — {s.message}
              </li>
            ))}
          </ul>
          {failed.length > 0 ? (
            <p style={{ marginTop: 8, color: "var(--orange-text)" }}>{t("init.partial_hint")}</p>
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
