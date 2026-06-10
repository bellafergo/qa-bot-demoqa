import React from "react";
import JiraIssueCorrelationCard from "./JiraIssueCorrelationCard.jsx";

export default function JiraIssueIntelligenceView({ vm }) {
  if (!vm?.show) return null;

  return (
    <div
      style={{
        padding: "16px 18px",
        background: "var(--bg-2)",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      {vm.empty ? (
        <p style={{ margin: 0, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>{vm.emptyMessage}</p>
      ) : (
        <>
          {vm.summary ? (
            <p style={{ margin: "0 0 14px", fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>{vm.summary}</p>
          ) : null}
          <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(120px, 1fr))", gap: 10, marginBottom: 16 }}>
            <div style={{ textAlign: "center", padding: "10px 8px", borderRadius: 6, background: "var(--bg-3)" }}>
              <div style={{ fontSize: 20, fontWeight: 700 }}>{vm.correlatedIssues}</div>
              <div style={{ fontSize: 11, color: "var(--text-3)" }}>{vm.correlatedIssuesLabel}</div>
            </div>
            <div style={{ textAlign: "center", padding: "10px 8px", borderRadius: 6, background: "var(--bg-3)" }}>
              <div style={{ fontSize: 20, fontWeight: 700, color: "var(--red, #ef4444)" }}>{vm.blockerCount}</div>
              <div style={{ fontSize: 11, color: "var(--text-3)" }}>{vm.blockersLabel}</div>
            </div>
            <div style={{ textAlign: "center", padding: "10px 8px", borderRadius: 6, background: "var(--bg-3)" }}>
              <div style={{ fontSize: 20, fontWeight: 700 }}>{vm.highPriorityCount}</div>
              <div style={{ fontSize: 11, color: "var(--text-3)" }}>{vm.highPriorityLabel}</div>
            </div>
          </div>

          {vm.showTopBlockers ? (
            <div style={{ marginBottom: 16 }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
                {vm.topBlockersLabel}
              </div>
              <ul style={{ margin: 0, padding: 0 }}>
                {vm.topBlockers.map((item) => (
                  <JiraIssueCorrelationCard key={`blocker-${item.issueKey}`} vm={item} />
                ))}
              </ul>
            </div>
          ) : null}

          {vm.showCorrelations ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
                {vm.correlatedListLabel}
              </div>
              <ul style={{ margin: 0, padding: 0 }}>
                {vm.correlations.map((item) => (
                  <JiraIssueCorrelationCard key={item.issueKey} vm={item} />
                ))}
              </ul>
            </div>
          ) : null}
        </>
      )}
      <p style={{ margin: "12px 0 0", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
