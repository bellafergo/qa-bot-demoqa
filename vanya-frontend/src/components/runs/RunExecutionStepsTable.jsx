import React, { Fragment } from "react";
import { bestStepTarget, stepStatusColor } from "../../utils/runHelpers.js";
import ApiStepEvidencePanel from "./ApiStepEvidencePanel.jsx";

export default function RunExecutionStepsTable({ steps, healedIndexSet = new Set(), t, inline = false }) {
  if (!steps?.length) return null;
  const healed = healedIndexSet instanceof Set ? healedIndexSet : new Set();
  const table = (
    <div style={{ overflowX: "auto" }}>
      <table className="data-table">
        <thead><tr>
          <th style={{ width: 44 }}>{t("runs.detail.col.num")}</th>
          <th>{t("runs.detail.col.action")}</th>
          <th>{t("runs.detail.col.target_url")}</th>
          <th>{t("runs.detail.col.status")}</th>
          <th>{t("runs.detail.col.duration")}</th>
        </tr></thead>
        <tbody>
          {steps.map((step, i) => {
            const idx0 = typeof step.index === "number" ? step.index : i;
            const isHealed = healed.has(idx0);
            return (
              <Fragment key={`step-row-${i}`}>
                <tr
                  style={{
                    background: isHealed ? "var(--orange-bg)" : undefined,
                    boxShadow: isHealed ? "inset 3px 0 0 var(--orange-border)" : undefined,
                  }}
                >
                  <td style={{ color: "var(--text-3)", fontWeight: 600 }}>{step.index ?? i + 1}</td>
                  <td>
                    <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
                      <code style={{ fontSize: 12 }}>{step.action || "—"}</code>
                      {step.evidence && (
                        <span className="badge badge-gray" style={{ fontSize: 9 }}>
                          API
                        </span>
                      )}
                      {isHealed && (
                        <span className="badge badge-orange" style={{ fontSize: 9, textTransform: "uppercase", letterSpacing: "0.04em" }}>
                          {t("runs.detail.step_healed_badge")}
                        </span>
                      )}
                    </div>
                  </td>
                  <td style={{ maxWidth: 320, wordBreak: "break-all", fontSize: 12, color: "var(--text-2)" }}>{bestStepTarget(step)}</td>
                  <td><span style={{ fontSize: 11, fontWeight: 500, color: stepStatusColor(step.status), textTransform: "uppercase", letterSpacing: "0.03em" }}>{step.status || "—"}</span></td>
                  <td style={{ color: "var(--text-3)", fontSize: 12, whiteSpace: "nowrap" }}>{step.duration_ms ? `${step.duration_ms}ms` : "—"}</td>
                </tr>
                {step.evidence && (
                  <tr className="api-step-evidence-row">
                    <td colSpan={5} style={{ padding: 0, verticalAlign: "top" }}>
                      <ApiStepEvidencePanel evidence={step.evidence} t={t} />
                    </td>
                  </tr>
                )}
              </Fragment>
            );
          })}
        </tbody>
      </table>
    </div>
  );

  if (inline) {
    return (
      <div style={{ marginTop: 12, borderTop: "1px solid var(--border)", paddingTop: 12 }}>
        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 10 }}>
          {t("runs.api_evidence.title")}
        </div>
        {table}
      </div>
    );
  }

  return (
    <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
      <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
        <div className="section-title" style={{ margin: 0 }}>
          {t("runs.detail.exec_steps")} — {steps.length} {t("runs.detail.total_label")}
        </div>
      </div>
      {table}
    </div>
  );
}
