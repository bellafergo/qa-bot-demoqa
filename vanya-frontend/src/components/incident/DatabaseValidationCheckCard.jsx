import React, { useCallback, useState } from "react";
import DatabaseValidationPreviewModal from "./DatabaseValidationPreviewModal.jsx";
import DatabaseValidationExecuteModal from "./DatabaseValidationExecuteModal.jsx";

export default function DatabaseValidationCheckCard({
  check,
  labels,
  executePayload,
  onSimulateApprove,
  onExecute,
  executeBusy,
}) {
  const [previewOpen, setPreviewOpen] = useState(false);
  const [executeOpen, setExecuteOpen] = useState(false);

  const openPreview = useCallback(() => setPreviewOpen(true), []);
  const closePreview = useCallback(() => setPreviewOpen(false), []);
  const openExecute = useCallback(() => setExecuteOpen(true), []);
  const closeExecute = useCallback(() => setExecuteOpen(false), []);

  return (
    <li
      style={{
        marginBottom: 12,
        padding: "14px 16px",
        background: "var(--bg-2)",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <span className="badge badge-red">{labels.approvalRequiredLabel}</span>
        <span className={check.safetySafe ? "badge badge-gray" : "badge badge-red"}>
          {labels.readOnlySafetyLabel}: {check.safetyText}
        </span>
        <span className="badge badge-blue">
          {labels.databaseTypeLabel}: {check.database_type}
        </span>
        <span className="badge badge-orange">
          {check.enabled ? labels.enabledLabel : labels.disabledLabel}
        </span>
      </div>
      <strong style={{ color: "var(--text-1)", display: "block", marginBottom: 6 }}>
        {check.name}
      </strong>
      <div style={{ color: "var(--text-2)", marginBottom: 8, fontSize: 12 }}>
        {check.description}
      </div>
      <div style={{ color: "var(--text-3)", marginBottom: 12, fontSize: 12 }}>
        {labels.expectedResultLabel}: {check.expectedResultText}
      </div>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
        <button type="button" className="btn btn-secondary btn-sm" onClick={openPreview}>
          {labels.previewLabel}
        </button>
        {executePayload ? (
          <button type="button" className="btn btn-primary btn-sm" onClick={openExecute} disabled={!check.hasConnector}>
            {labels.executeLabel}
          </button>
        ) : null}
      </div>
      <DatabaseValidationPreviewModal
        open={previewOpen}
        payload={check.previewPayload}
        onClose={closePreview}
      />
      <DatabaseValidationExecuteModal
        open={executeOpen}
        payload={executePayload}
        onClose={closeExecute}
        onSimulateApprove={onSimulateApprove}
        onExecute={onExecute}
        busy={executeBusy}
      />
    </li>
  );
}
