import React from "react";
import {
  fmtTs,
  severityBadge,
  reproducedLabel,
} from "../../utils/incidentInvestigatorHelpers.js";

export default function InvestigationResult({ run, t, titleKey = "incident.result.title" }) {
  if (!run) return null;
  const screenshot = run.screenshot_url || (run.screenshot_b64 ? `data:image/png;base64,${run.screenshot_b64}` : null);

  return (
    <div className="card" style={{ padding: "20px 24px", marginTop: 20 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 12, flexWrap: "wrap", marginBottom: 16 }}>
        <div>
          <div className="section-title" style={{ margin: 0 }}>{t(titleKey)}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>
            {fmtTs(run.created_at)} · {run.id?.slice(0, 8)}…
          </div>
        </div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
          <span className={severityBadge(run.severity)}>{run.severity}</span>
          <span className="badge badge-gray">{reproducedLabel(run.reproduced, t)}</span>
          <span className="badge badge-gray">{run.suspected_area || "unknown"}</span>
        </div>
      </div>

      {run.error_message ? (
        <div className="alert alert-error" style={{ marginBottom: 16, fontSize: 13 }}>{run.error_message}</div>
      ) : null}

      <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 8 }}>
        {run.diagnosis_summary || t("incident.result.no_diagnosis")}
      </div>

      {run.symptom_observed ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
          <strong>{t("incident.result.symptom")}:</strong> {run.symptom_observed}
        </div>
      ) : null}

      {run.probable_cause ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
          <strong>{t("incident.result.cause")}:</strong> {run.probable_cause}
        </div>
      ) : null}

      {run.suspected_endpoint ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, fontFamily: "monospace", wordBreak: "break-all" }}>
          {t("incident.result.endpoint")}: {run.suspected_endpoint}
        </div>
      ) : null}

      {run.target_url ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, wordBreak: "break-all" }}>
          {t("incident.form.target_url")}: {run.target_url}
        </div>
      ) : null}

      {screenshot ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8, textTransform: "uppercase", letterSpacing: "0.06em" }}>
            {t("incident.result.screenshot")}
          </div>
          <img
            src={screenshot}
            alt="investigation screenshot"
            style={{ maxWidth: "100%", borderRadius: 8, border: "1px solid var(--border)" }}
          />
        </div>
      ) : null}

      {run.recommendations?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.recommendations")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {run.recommendations.map((r, i) => <li key={i}>{r}</li>)}
          </ul>
        </div>
      ) : null}

      {run.reproduction_steps?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.repro_steps")}</div>
          <ol style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {run.reproduction_steps.map((s, i) => <li key={i}>{s}</li>)}
          </ol>
        </div>
      ) : null}

      {run.steps_executed?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.steps")}</div>
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {run.steps_executed.map((s, i) => (
              <span key={i} className="badge badge-gray" style={{ fontFamily: "monospace", fontSize: 10 }}>{s}</span>
            ))}
          </div>
        </div>
      ) : null}

      {(run.console_errors?.length > 0 || run.network_errors?.length > 0 || run.http_errors?.length > 0) ? (
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))", gap: 16 }}>
          {run.console_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.console")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.console_errors.map((e, i) => `${i + 1}. ${e.text || JSON.stringify(e)}`).join("\n")}
              </pre>
            </div>
          ) : null}
          {run.http_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.http")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.http_errors.map((e, i) => `${i + 1}. ${e.status} ${e.method || "GET"} ${e.url}`).join("\n")}
              </pre>
            </div>
          ) : null}
          {run.network_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.network")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.network_errors.map((e, i) => `${i + 1}. ${e.url} — ${e.failure || e.error || ""}`).join("\n")}
              </pre>
            </div>
          ) : null}
        </div>
      ) : null}
    </div>
  );
}
