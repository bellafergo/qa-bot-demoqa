import React, { useCallback, useState } from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import RecommendedActionPreviewModal from "./RecommendedActionPreviewModal.jsx";
import { buildRecommendedActionPreviewPayload } from "../../utils/incidentRecommendedActionsViewUtils.js";
import { useLang } from "../../i18n/LangContext.jsx";

export default function RecommendedActionCard({ action, labels }) {
  const { t } = useLang();
  const [previewOpen, setPreviewOpen] = useState(false);

  const openPreview = useCallback(() => {
    setPreviewOpen(true);
  }, []);

  const closePreview = useCallback(() => {
    setPreviewOpen(false);
  }, []);

  const previewPayload = buildRecommendedActionPreviewPayload(action, t);

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
          {labels.priorityLabel}: {action.priority}
        </span>
        <span className="badge badge-orange">
          {labels.confidenceLabel}: {action.confidenceText}
        </span>
        <span className="badge badge-gray">
          {labels.actionTypeLabel}: {action.action_type}
        </span>
      </div>
      <strong style={{ color: "var(--text-1)", display: "block", marginBottom: 6 }}>
        {action.title}
      </strong>
      <div style={{ color: "var(--text-3)", marginBottom: 4 }}>{action.description}</div>
      <div style={{ color: "var(--text-2)", marginBottom: action.drilldownItem ? 10 : 12, fontSize: 12 }}>
        {action.reason}
      </div>
      {action.drilldownItem ? (
        <div style={{ marginBottom: 12 }}>
          <EvidenceCorrelationDrilldownCell item={action.drilldownItem} />
        </div>
      ) : null}
      <button
        type="button"
        className="btn btn-secondary btn-sm"
        onClick={openPreview}
      >
        {action.approveButtonLabel}
      </button>
      <RecommendedActionPreviewModal
        open={previewOpen}
        payload={previewPayload}
        onClose={closePreview}
      />
    </li>
  );
}
