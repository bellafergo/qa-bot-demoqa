import React from "react";
import { useLang } from "../../i18n/LangContext.jsx";

export default function CorrelationDrilldownModal({
  open,
  title,
  fields = [],
  unavailableMessage,
  onClose,
}) {
  const { t } = useLang();
  if (!open) return null;

  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex: 10040,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
      aria-labelledby="correlation-drilldown-modal-title"
    >
      <div className="card" style={{ width: "min(560px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="correlation-drilldown-modal-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {title}
          </div>
          {unavailableMessage ? (
            <div style={{ fontSize: 13, color: "var(--orange-text, var(--text-2))", lineHeight: 1.55, marginTop: 8 }}>
              {unavailableMessage}
            </div>
          ) : null}
        </div>
        <div style={{ padding: "16px 20px" }}>
          {fields.map((field) => (
            <div key={field.labelKey} style={{ marginBottom: 12 }}>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {t(field.labelKey)}
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
