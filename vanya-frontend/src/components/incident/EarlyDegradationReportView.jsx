import React from "react";
import DegradationAssessmentCard from "./DegradationAssessmentCard.jsx";

export default function EarlyDegradationReportView({ vm }) {
  if (!vm?.report) return null;
  const report = vm.report;

  const labels = {
    riskProjectionLabel: vm.riskProjectionLabel,
    confidenceLabel: vm.confidenceLabel,
    recommendedAttentionLabel: vm.recommendedAttentionLabel,
    scoreLabel: "Score",
  };

  return (
    <>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 12 }}>
        <span className={report.overallStatusBadgeClass}>
          {vm.overallStatusLabel}: {report.overallStatusText}
        </span>
        <span className="badge badge-orange">
          {vm.degradingAreasLabel}: {report.degrading_areas}
        </span>
        <span className="badge badge-red">
          {vm.criticalAreasLabel}: {report.critical_areas}
        </span>
        <span className="badge badge-blue">
          {vm.confidenceLabel}: {report.confidenceText}
        </span>
      </div>

      {report.summary ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
          {report.summary}
        </div>
      ) : null}

      <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
        {report.assessments.map((assessment) => (
          <DegradationAssessmentCard key={assessment.assessment_id} assessment={assessment} labels={labels} />
        ))}
      </ul>
    </>
  );
}
