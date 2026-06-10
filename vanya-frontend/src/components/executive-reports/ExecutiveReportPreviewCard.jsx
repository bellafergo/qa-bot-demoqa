import React from "react";

export default function ExecutiveReportPreviewCard({ preview, labels }) {
  if (!preview) return null;

  return (
    <div
      style={{
        padding: "14px 16px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center", marginBottom: 10 }}>
        <strong style={{ color: "var(--text-1)" }}>{preview.title}</strong>
        <span className={preview.trendBadgeClass}>{preview.quality_trend}</span>
        <span className={preview.riskBadgeClass}>{preview.risk_level}</span>
      </div>

      <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
        <div>
          <span style={{ fontWeight: 600, color: "var(--text-3)", fontSize: 12 }}>{preview.qualityScoreLabel}: </span>
          <span style={{ fontWeight: 700, color: "var(--text-1)" }}>{preview.quality_score}</span>
        </div>
        <div style={{ fontSize: 12, color: "var(--text-2)" }}>
          <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{preview.executiveSummaryLabel}: </span>
          {preview.executive_summary}
        </div>
      </div>

      {preview.top_risks?.length ? (
        <div style={{ marginBottom: 10 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>{preview.topRisksLabel}</div>
          <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)", fontSize: 12 }}>
            {preview.top_risks.map((risk) => (
              <li key={risk}>{risk}</li>
            ))}
          </ul>
        </div>
      ) : null}

      {preview.top_recommendations?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>{preview.topRecommendationsLabel}</div>
          <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)", fontSize: 12 }}>
            {preview.top_recommendations.map((rec) => (
              <li key={rec}>{rec}</li>
            ))}
          </ul>
        </div>
      ) : null}

      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginTop: 12 }}>
        <button type="button" className="btn btn-secondary btn-sm">
          {labels?.previewReportLabel}
        </button>
        <button type="button" className="btn btn-ghost btn-sm" disabled title={labels?.sendDisabledNote}>
          {labels?.sendReportLabel}
        </button>
      </div>
    </div>
  );
}
