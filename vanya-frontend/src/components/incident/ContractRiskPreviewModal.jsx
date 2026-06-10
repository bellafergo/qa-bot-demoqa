import React from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { CONTRACT_RISK_ASSESSMENT_I18N_KEYS } from "../../utils/contractRiskAssessmentViewUtils.js";
import ContractRiskFactorCard from "./ContractRiskFactorCard.jsx";

export default function ContractRiskPreviewModal({ open, payload, onClose }) {
  const { t } = useLang();
  if (!open || !payload) return null;

  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex: 10050,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
      aria-labelledby="contract-risk-preview-title"
    >
      <div className="card" style={{ width: "min(720px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="contract-risk-preview-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {payload.title}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>
            {t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.previewSubtitle)}
          </div>
        </div>
        <div style={{ padding: "16px 20px", maxHeight: "70vh", overflowY: "auto" }}>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
            <span className="badge badge-red">
              {payload.labels.riskScore}: {payload.riskScore}
            </span>
            <span className="badge badge-red">
              {payload.labels.riskLevel}: {payload.riskLevel}
            </span>
            <span className="badge badge-blue">
              {payload.labels.confidence}: {payload.confidence}
            </span>
          </div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {payload.labels.businessImpact}
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, margin: "0 0 12px" }}>
            {payload.summary}
          </p>
          {payload.changes?.length ? (
            <>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                {payload.labels.changeDetails}
              </div>
              <ul style={{ margin: "0 0 12px", paddingLeft: 18, color: "var(--text-2)", fontSize: 13 }}>
                {payload.changes.map((change) => (
                  <li key={change.change_id}>{change.description}</li>
                ))}
              </ul>
            </>
          ) : null}
          {payload.affectedJourneys?.length ? (
            <div style={{ marginBottom: 8, fontSize: 13, color: "var(--text-2)" }}>
              <strong>{payload.labels.affectedJourneys}:</strong> {payload.affectedJourneys.join(", ")}
            </div>
          ) : null}
          {payload.affectedModules?.length ? (
            <div style={{ marginBottom: 8, fontSize: 13, color: "var(--text-2)" }}>
              <strong>{payload.labels.affectedModules}:</strong> {payload.affectedModules.join(", ")}
            </div>
          ) : null}
          {payload.affectedTests?.length ? (
            <div style={{ marginBottom: 12, fontSize: 13, color: "var(--text-2)" }}>
              <strong>{payload.labels.affectedTests}:</strong> {payload.affectedTests.join(", ")}
            </div>
          ) : null}
          {payload.factors?.length ? (
            <>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                {payload.labels.riskFactors}
              </div>
              <ul style={{ margin: "0 0 12px", padding: 0, listStyle: "none" }}>
                {payload.factors.map((factor) => (
                  <ContractRiskFactorCard
                    key={factor.factor_id}
                    factor={factor}
                    severityLabel={payload.labels.riskLevel}
                  />
                ))}
              </ul>
            </>
          ) : null}
          <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
            {payload.readOnlyNote}
          </p>
        </div>
        <div style={{ padding: "12px 20px", borderTop: "1px solid var(--border)", textAlign: "right" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onClose}>
            {t("common.close")}
          </button>
        </div>
      </div>
    </div>
  );
}
