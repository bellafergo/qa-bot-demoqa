import React from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { API_CONTRACT_INTELLIGENCE_I18N_KEYS } from "../../utils/apiContractIntelligenceViewUtils.js";

export default function ContractChangePreviewModal({ open, payload, onClose }) {
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
      aria-labelledby="contract-change-preview-title"
    >
      <div className="card" style={{ width: "min(720px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="contract-change-preview-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {payload.title}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>
            {t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.previewSubtitle)}
          </div>
        </div>
        <div style={{ padding: "16px 20px" }}>
          <div style={{ display: "grid", gap: 12, gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))" }}>
            <div>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.oldContract)}
              </div>
              <pre
                style={{
                  margin: 0,
                  padding: "10px 12px",
                  borderRadius: 8,
                  background: "var(--bg-2)",
                  fontSize: 12,
                  lineHeight: 1.55,
                  color: "var(--text-2)",
                  whiteSpace: "pre-wrap",
                  wordBreak: "break-word",
                }}
              >
                {payload.oldSnippet}
              </pre>
            </div>
            <div>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.newContract)}
              </div>
              <pre
                style={{
                  margin: 0,
                  padding: "10px 12px",
                  borderRadius: 8,
                  background: "var(--bg-2)",
                  fontSize: 12,
                  lineHeight: 1.55,
                  color: "var(--text-2)",
                  whiteSpace: "pre-wrap",
                  wordBreak: "break-word",
                }}
              >
                {payload.newSnippet}
              </pre>
            </div>
          </div>
          {payload.fields?.map((field) => (
            <div key={field.label} style={{ marginTop: 12 }}>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {field.label}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)" }}>{field.value}</div>
            </div>
          ))}
          <div style={{ marginTop: 12 }}>
            <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
              {t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.deploymentImpact)}
            </div>
            <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55 }}>{payload.deploymentImpact}</div>
          </div>
          <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "14px 0 0", fontStyle: "italic" }}>
            {payload.readOnlyNote}
          </p>
        </div>
        <div style={{ padding: "12px 20px", borderTop: "1px solid var(--border)", display: "flex", justifyContent: "flex-end" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onClose}>
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
