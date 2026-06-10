import React from "react";

export default function ApiValidationRequestCard({ validation, labels, onPreview }) {
  if (!validation) return null;

  return (
    <li
      style={{
        marginBottom: 8,
        padding: "10px 12px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 12,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 6 }}>
        <strong style={{ color: "var(--text-1)" }}>{validation.validationTypeLabel}</strong>
        <span className={validation.statusBadgeClass}>{validation.status}</span>
        {validation.requires_user_approval ? (
          <span className="badge badge-orange">{labels?.approvalRequiredLabel}</span>
        ) : null}
      </div>
      <div style={{ color: "var(--text-2)", marginBottom: 8 }}>
        {validation.connectorName} · {validation.endpointLabel}
      </div>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
        <button type="button" className="btn btn-secondary btn-sm" onClick={() => onPreview?.(validation)}>
          {labels?.previewValidationLabel}
        </button>
        <button type="button" className="btn btn-ghost btn-sm" disabled title={labels?.executeDisabledNote}>
          {labels?.executeValidationLabel}
        </button>
      </div>
    </li>
  );
}
