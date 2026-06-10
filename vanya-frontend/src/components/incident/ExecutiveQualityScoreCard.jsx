import React from "react";

export default function ExecutiveQualityScoreCard({ report, labels }) {
  return (
    <div
      style={{
        padding: "20px 22px",
        borderRadius: 12,
        border: `3px solid ${report.scoreColor}`,
        background: "var(--bg-2)",
        marginBottom: 16,
        textAlign: "center",
      }}
    >
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
        {labels.qualityScoreLabel}
      </div>
      <div style={{ fontSize: 48, fontWeight: 800, color: report.scoreColor, lineHeight: 1.1 }}>
        {report.overall_quality_score}
        <span style={{ fontSize: 18, fontWeight: 500, color: "var(--text-3)" }}> / 100</span>
      </div>
      <div style={{ fontSize: 14, fontWeight: 600, color: report.scoreColor, marginTop: 6 }}>
        {report.scoreBandLabel}
      </div>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", justifyContent: "center", marginTop: 12 }}>
        <span className={report.riskBadgeClass}>
          {labels.riskLevelLabel}: {report.overall_risk_level}
        </span>
        <span className="badge badge-blue">
          {labels.confidenceLabel}: {report.confidenceText}
        </span>
      </div>
    </div>
  );
}
