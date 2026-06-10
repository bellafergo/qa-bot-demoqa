import React, { useCallback, useState } from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import ApprovalWorkflowPreviewModal from "./ApprovalWorkflowPreviewModal.jsx";

export default function ApprovalRequestCard({ request, labels }) {
  const [previewOpen, setPreviewOpen] = useState(false);

  const openPreview = useCallback(() => setPreviewOpen(true), []);
  const closePreview = useCallback(() => setPreviewOpen(false), []);

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
        <span className={request.statusBadgeClass}>
          {labels.statusLabel}: {request.statusLabel}
        </span>
        <span className="badge badge-gray">
          {labels.approvalTypeLabel}: {request.approval_type}
        </span>
      </div>
      <strong style={{ color: "var(--text-1)", display: "block", marginBottom: 6 }}>
        {request.title}
      </strong>
      <div style={{ color: "var(--text-2)", marginBottom: request.drilldownItem ? 10 : 12, fontSize: 12 }}>
        {request.description}
      </div>
      {request.drilldownItem ? (
        <div style={{ marginBottom: 12 }}>
          <EvidenceCorrelationDrilldownCell item={request.drilldownItem} />
        </div>
      ) : null}
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
        <button type="button" className="btn btn-secondary btn-sm" onClick={openPreview}>
          {labels.approveLabel}
        </button>
        <button type="button" className="btn btn-secondary btn-sm" onClick={openPreview}>
          {labels.rejectLabel}
        </button>
      </div>
      <ApprovalWorkflowPreviewModal open={previewOpen} payload={request.previewPayload} onClose={closePreview} />
    </li>
  );
}
