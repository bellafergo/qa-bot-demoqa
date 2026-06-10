import React from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import QualityTrendCard from "./QualityTrendCard.jsx";

export default function QualityTrendReportView({ vm }) {
  if (!vm?.report) return null;
  const report = vm.report;

  const labels = {
    scoreChangeLabel: vm.scoreChangeLabel,
    historicalPointsLabel: vm.historicalPointsLabel,
  };

  return (
    <>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 12 }}>
        <span className={report.overallTrendBadgeClass}>
          {vm.overallTrendLabel}: {report.overallTrendLabel}
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
        {report.trends.map((trend) => (
          <QualityTrendCard key={trend.trend_id} trend={trend} labels={labels} />
        ))}
      </ul>
      {vm.drilldownItem ? (
        <div style={{ marginTop: 10 }}>
          <EvidenceCorrelationDrilldownCell item={vm.drilldownItem} />
        </div>
      ) : null}
    </>
  );
}
