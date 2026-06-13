import React, { useCallback, useState } from "react";
import JourneyStageCard from "./JourneyStageCard.jsx";
import DataJourneyPreviewModal from "./DataJourneyPreviewModal.jsx";
import IncidentInsightTracePanel from "../incidents/IncidentInsightTracePanel.jsx";

export default function DataJourneyCard({ journey, labels }) {
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
        <strong style={{ color: "var(--text-1)" }}>{journey.name}</strong>
        <span className={journey.statusBadgeClass}>
          {labels.journeyStatusLabel}: {journey.status}
        </span>
        <span className="badge badge-blue">
          {labels.completedStagesLabel}: {journey.completedText}
        </span>
        <span className="badge badge-orange">
          {labels.confidenceLabel}: {journey.confidenceText}
        </span>
      </div>
      {journey.result?.summary ? (
        <div style={{ color: "var(--text-2)", marginBottom: 10 }}>{journey.result.summary}</div>
      ) : null}
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          gap: 4,
          marginBottom: 12,
          padding: "12px 8px",
          borderRadius: 8,
          background: "var(--bg-1, rgba(0,0,0,0.15))",
        }}
      >
        {journey.stages.map((stage, index) => (
          <JourneyStageCard
            key={stage.stage_id}
            stage={stage}
            stageStatusLabel={labels.stageStatusLabel}
            isLast={index === journey.stages.length - 1}
          />
        ))}
      </div>
      {(journey.result?.missing_stages?.length || journey.result?.inconsistent_stages?.length) ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 10 }}>
          {journey.result.missing_stages?.length ? (
            <div>
              {labels.missingStagesLabel}: {journey.result.missing_stages.join(", ")}
            </div>
          ) : null}
          {journey.result.inconsistent_stages?.length ? (
            <div>
              {labels.inconsistentStagesLabel}: {journey.result.inconsistent_stages.join(", ")}
            </div>
          ) : null}
        </div>
      ) : null}
      {journey.showTrace ? <IncidentInsightTracePanel trace={journey.trace} /> : null}
      <button type="button" className="btn btn-secondary btn-sm" onClick={openPreview}>
        {labels.previewLabel}
      </button>
      <DataJourneyPreviewModal open={previewOpen} payload={journey.previewPayload} onClose={closePreview} />
    </li>
  );
}
