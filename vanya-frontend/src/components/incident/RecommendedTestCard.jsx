import React, { useCallback, useState } from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import RecommendedTestPreviewModal from "./RecommendedTestPreviewModal.jsx";

export default function RecommendedTestCard({ recommendation, labels }) {
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
        <span className="badge badge-red">{labels.approvalRequiredLabel}</span>
        <span className="badge badge-blue">
          {labels.priorityLabel}: {recommendation.priority}
        </span>
        <span className="badge badge-orange">
          {labels.confidenceLabel}: {recommendation.confidenceText}
        </span>
        <span className="badge badge-gray">
          {labels.riskReductionLabel}: {recommendation.riskReductionText}
        </span>
      </div>
      <strong style={{ color: "var(--text-1)", display: "block", marginBottom: 6 }}>
        {recommendation.test_name}
      </strong>
      <div style={{ color: "var(--text-2)", marginBottom: recommendation.drilldownItem ? 10 : 12, fontSize: 12 }}>
        {recommendation.reason}
      </div>
      {recommendation.drilldownItem ? (
        <div style={{ marginBottom: 12 }}>
          <EvidenceCorrelationDrilldownCell item={recommendation.drilldownItem} />
        </div>
      ) : null}
      <button type="button" className="btn btn-secondary btn-sm" onClick={openPreview}>
        {labels.previewLabel}
      </button>
      <RecommendedTestPreviewModal
        open={previewOpen}
        payload={recommendation.previewPayload}
        onClose={closePreview}
      />
    </li>
  );
}
