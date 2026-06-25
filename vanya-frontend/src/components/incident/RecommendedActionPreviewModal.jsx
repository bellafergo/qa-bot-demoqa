import React from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { RECOMMENDED_ACTIONS_I18N_KEYS } from "../../utils/incidentRecommendedActionsViewUtils.js";

export default function RecommendedActionPreviewModal({ open, payload, onClose }) {
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
      aria-labelledby="recommended-action-preview-title"
    >
      <div className="card" style={{ width: "min(560px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="recommended-action-preview-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {payload.title}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>
            {t(RECOMMENDED_ACTIONS_I18N_KEYS.previewSubtitle)}
          </div>
        </div>
        <div style={{ padding: "16px 20px" }}>
          {payload.fields.map((field) => (
            <div key={field.label} style={{ marginBottom: 12 }}>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {field.label}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, whiteSpace: "pre-wrap" }}>
                {field.value}
              </div>
            </div>
          ))}

        </div>
        <div style={{ padding: "14px 20px", display: "flex", justifyContent: "flex-end", borderTop: "1px solid var(--border)" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onClose}>
            {t("common.cancel")}
          </button>
        </div>
      </div>
    </div>
  );
}
