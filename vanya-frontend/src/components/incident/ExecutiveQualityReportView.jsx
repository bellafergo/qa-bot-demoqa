import React from "react";
import ExecutiveQualityScoreCard from "./ExecutiveQualityScoreCard.jsx";
import ExecutiveRiskCard from "./ExecutiveRiskCard.jsx";
import ExecutiveRecommendationCard from "./ExecutiveRecommendationCard.jsx";

export default function ExecutiveQualityReportView({ vm }) {
  if (!vm?.report) return null;
  const report = vm.report;

  const labels = {
    qualityScoreLabel: vm.qualityScoreLabel,
    riskLevelLabel: vm.riskLevelLabel,
    confidenceLabel: vm.confidenceLabel,
  };

  return (
    <>
      <ExecutiveQualityScoreCard report={report} labels={labels} />
      <div style={{ marginBottom: 12 }}>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
          {vm.executiveSummaryLabel}
        </div>
        <div style={{ fontSize: 14, color: "var(--text-1)", lineHeight: 1.6 }}>
          {report.executive_summary}
        </div>
      </div>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 14 }}>
        <span className="badge badge-blue">
          {vm.openIncidentsLabel}: {report.open_incident_count}
        </span>
        <span className="badge badge-red">
          {vm.criticalContractsLabel}: {report.critical_contract_count}
        </span>
        <span className="badge badge-orange">
          {vm.brokenJourneysLabel}: {report.broken_journey_count}
        </span>
        <span className="badge badge-blue">
          {vm.recommendedTestsLabel}: {report.recommended_test_count}
        </span>
        {report.quality_trend ? (
          <span className="badge badge-gray">
            {vm.qualityTrendLabel}: {report.quality_trend}
          </span>
        ) : null}
      </div>
      {report.top_risks?.length ? (
        <div style={{ marginBottom: 12 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {vm.topRisksLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {report.top_risks.map((risk) => (
              <ExecutiveRiskCard key={risk} risk={risk} />
            ))}
          </ul>
        </div>
      ) : null}
      {report.top_recommendations?.length ? (
        <div style={{ marginBottom: 4 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {vm.topRecommendationsLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {report.top_recommendations.map((rec) => (
              <ExecutiveRecommendationCard key={rec} recommendation={rec} />
            ))}
          </ul>
        </div>
      ) : null}
    </>
  );
}
