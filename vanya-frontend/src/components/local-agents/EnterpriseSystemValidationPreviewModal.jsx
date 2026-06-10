import React from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { ENTERPRISE_SYSTEM_I18N_KEYS } from "../../utils/enterpriseSystemViewUtils.js";

export default function EnterpriseSystemValidationPreviewModal({ open, payload, onClose }) {
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
      aria-labelledby="enterprise-system-validation-preview-title"
    >
      <div className="card" style={{ width: "min(640px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="enterprise-system-validation-preview-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {payload.title}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>{payload.readOnlyNote}</div>
        </div>
        <div style={{ padding: "16px 20px" }}>
          <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 10 }}>
            <strong>{t(ENTERPRISE_SYSTEM_I18N_KEYS.title)}:</strong> {payload.systemName}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 10 }}>
            <strong>{t(ENTERPRISE_SYSTEM_I18N_KEYS.modules)}:</strong> {payload.moduleName}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 10 }}>
            <strong>{t(ENTERPRISE_SYSTEM_I18N_KEYS.validationType)}:</strong> {payload.validationType}
          </div>
          {payload.approvalRequired ? (
            <div className="alert alert-warning" style={{ fontSize: 13, marginBottom: 12 }}>
              {payload.approvalRequiredLabel}
            </div>
          ) : null}
        </div>
        <div
          style={{
            padding: "12px 20px",
            borderTop: "1px solid var(--border)",
            display: "flex",
            gap: 8,
            justifyContent: "flex-end",
            flexWrap: "wrap",
          }}
        >
          <button type="button" className="btn btn-ghost btn-sm" onClick={onClose}>
            Close
          </button>
          <button type="button" className="btn btn-secondary btn-sm" disabled title={payload.executeDisabledNote}>
            {payload.executeLabel}
          </button>
        </div>
      </div>
    </div>
  );
}
