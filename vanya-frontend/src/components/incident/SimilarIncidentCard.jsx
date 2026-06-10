import React, { useCallback, useState } from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import SimilarIncidentPreviewModal from "./SimilarIncidentPreviewModal.jsx";

export default function SimilarIncidentCard({ incident, labels }) {
  const [previewOpen, setPreviewOpen] = useState(false);

  const openPreview = useCallback(() => setPreviewOpen(true), []);
  const closePreview = useCallback(() => setPreviewOpen(false), []);

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 6,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 6 }}>
        <strong style={{ color: "var(--text-1)" }}>{incident.title}</strong>
        <span className="badge badge-orange">
          {labels.similarityLabel}: {incident.similarityText}
        </span>
        {incident.timestampText ? (
          <span className="badge badge-gray">{incident.timestampText}</span>
        ) : null}
      </div>
      <div style={{ color: "var(--text-3)", marginBottom: incident.drilldownItem ? 8 : 10 }}>
        {incident.summary}
      </div>
      {incident.drilldownItem ? (
        <div style={{ marginBottom: 10 }}>
          <EvidenceCorrelationDrilldownCell item={incident.drilldownItem} />
        </div>
      ) : (
        <button type="button" className="btn btn-secondary btn-sm" onClick={openPreview}>
          {labels.previewLabel}
        </button>
      )}
      <SimilarIncidentPreviewModal open={previewOpen} payload={incident.previewPayload} onClose={closePreview} />
    </li>
  );
}
