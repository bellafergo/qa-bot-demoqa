import React from "react";
import EnvironmentStatusCard from "./EnvironmentStatusCard.jsx";
import EnvironmentComparisonCard from "./EnvironmentComparisonCard.jsx";
import PromotionReadinessCard from "./PromotionReadinessCard.jsx";

export default function MultiEnvironmentIntelligenceView({ vm }) {
  if (!vm?.report) return null;
  const report = vm.report;

  const envLabels = {
    statusLabel: vm.statusLabel,
    signalsLabel: vm.signalsLabel,
  };

  const comparisonLabels = {
    riskDeltaLabel: vm.riskDeltaLabel,
  };

  const readinessLabels = {
    readinessScoreLabel: vm.readinessScoreLabel,
    blockersLabel: vm.blockersLabel,
    warningsLabel: vm.warningsLabel,
    recommendedValidationsLabel: vm.recommendedValidationsLabel,
  };

  return (
    <>
      <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
        {report.summary}
      </div>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 14 }}>
        <span className="badge badge-orange">{vm.confidenceLabel}: {report.confidenceText}</span>
      </div>

      {report.environments?.length ? (
        <div style={{ marginBottom: 14 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.environmentLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {report.environments.map((environment) => (
              <EnvironmentStatusCard
                key={environment.environment_id}
                environment={environment}
                labels={envLabels}
              />
            ))}
          </ul>
        </div>
      ) : null}

      {report.comparisons?.length ? (
        <div style={{ marginBottom: 14 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.comparisonsLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {report.comparisons.map((comparison) => (
              <EnvironmentComparisonCard
                key={comparison.comparison_id}
                comparison={comparison}
                labels={comparisonLabels}
              />
            ))}
          </ul>
        </div>
      ) : null}

      {report.promotion_readiness?.length ? (
        <div style={{ marginBottom: 4 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.promotionReadinessLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {report.promotion_readiness.map((readiness) => (
              <PromotionReadinessCard
                key={`${readiness.source_environment_id}:${readiness.target_environment_id}`}
                readiness={readiness}
                labels={readinessLabels}
              />
            ))}
          </ul>
        </div>
      ) : null}
    </>
  );
}
