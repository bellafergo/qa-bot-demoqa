import React, { useState } from "react";
import QualityHealthScoreCard from "./QualityHealthScoreCard.jsx";
import ExplainabilityTracePanel from "../explainability/ExplainabilityTracePanel.jsx";

function ScoreGroup({ title, scores, labels, compact = true }) {
  if (!scores?.length) return null;
  return (
    <div style={{ marginBottom: 12 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{title}</div>
      <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
        {scores.map((score) => (
          <QualityHealthScoreCard key={score.score_id} score={score} labels={labels} compact={compact} />
        ))}
      </ul>
    </div>
  );
}

export default function QualityHealthReportView({ vm }) {
  if (!vm?.report) return null;
  const report = vm.report;
  const [traceOpen, setTraceOpen] = useState(false);

  const labels = {
    trendLabel: vm.trendLabel,
    contributingFactorsLabel: vm.contributingFactorsLabel,
  };

  return (
    <>
      <div
        style={{
          padding: "20px 22px",
          borderRadius: 12,
          border: `3px solid ${report.overallStatusColor}`,
          background: "var(--bg-2)",
          marginBottom: 16,
          textAlign: "center",
        }}
      >
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
          {vm.overallQualityHealthLabel}
        </div>
        <div style={{ fontSize: 48, fontWeight: 800, color: report.overallStatusColor, lineHeight: 1.1 }}>
          {report.overall_score}
          <span style={{ fontSize: 18, fontWeight: 500, color: "var(--text-3)" }}> / 100</span>
        </div>
        <div style={{ fontSize: 14, fontWeight: 600, color: report.overallStatusColor, marginTop: 6 }}>
          {report.overallStatusLabel}
        </div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap", justifyContent: "center", marginTop: 12 }}>
          <span className="badge badge-gray">
            {vm.trendLabel}: {report.overallTrendLabel}
          </span>
          <span className="badge badge-blue">
            {vm.confidenceLabel}: {report.confidenceText}
          </span>
        </div>
      </div>

      {report.summary ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 14 }}>
          {report.summary}
        </div>
      ) : null}

      {report.showTrace ? (
        <div style={{ marginBottom: 16 }}>
          <button
            type="button"
            onClick={() => setTraceOpen((open) => !open)}
            style={{
              display: "flex",
              alignItems: "center",
              gap: 6,
              background: "none",
              border: "none",
              padding: 0,
              cursor: "pointer",
              fontSize: 11,
              fontWeight: 600,
              color: "var(--text-3)",
              textTransform: "uppercase",
              letterSpacing: "0.05em",
            }}
          >
            <span>{traceOpen ? "▾" : "▸"}</span>
            <span>{traceOpen ? report.traceToggleHideLabel : report.traceToggleShowLabel}</span>
          </button>
          {traceOpen ? <ExplainabilityTracePanel trace={report.trace} /> : null}
        </div>
      ) : null}

      {report.projectScore ? (
        <ScoreGroup title={vm.projectScoresLabel} scores={[report.projectScore]} labels={labels} compact={false} />
      ) : null}
      <ScoreGroup title={vm.environmentScoresLabel} scores={report.environmentScores} labels={labels} />
      <ScoreGroup title={vm.moduleScoresLabel} scores={report.moduleScores} labels={labels} />
      <ScoreGroup title={vm.journeyScoresLabel} scores={report.journeyScores} labels={labels} />
      <ScoreGroup title={vm.contractScoresLabel} scores={report.contractScores} labels={labels} />
    </>
  );
}
