import React, { useCallback, useState } from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import ContractChangePreviewModal from "./ContractChangePreviewModal.jsx";

export default function ContractRiskCard({ assessment, changes, labels, drilldownItem }) {
  const [previewOpen, setPreviewOpen] = useState(false);
  const [previewPayload, setPreviewPayload] = useState(null);

  const openPreview = useCallback((payload) => {
    setPreviewPayload(payload);
    setPreviewOpen(true);
  }, []);

  const closePreview = useCallback(() => {
    setPreviewOpen(false);
    setPreviewPayload(null);
  }, []);

  return (
    <div>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
        {labels.riskAssessmentLabel}
      </div>
      <div style={{ color: "var(--text-2)", marginBottom: 8 }}>{assessment.summary}</div>
      {drilldownItem ? (
        <div style={{ marginBottom: 8 }}>
          <EvidenceCorrelationDrilldownCell item={drilldownItem} />
        </div>
      ) : null}
      {changes?.length ? (
        <>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {labels.contractChangesLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)" }}>
            {changes.map((change) => (
              <li key={change.change_id} style={{ marginBottom: 8 }}>
                <span>{change.description}</span>
                <div style={{ marginTop: 6 }}>
                  <button
                    type="button"
                    className="btn btn-secondary btn-sm"
                    onClick={() => openPreview(change.previewPayload)}
                  >
                    {labels.previewLabel}
                  </button>
                </div>
              </li>
            ))}
          </ul>
        </>
      ) : null}
      <ContractChangePreviewModal open={previewOpen} payload={previewPayload} onClose={closePreview} />
    </div>
  );
}
