import React from "react";

export default function JiraIssueCorrelationCard({ vm }) {
  if (!vm) return null;

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
        listStyle: "none",
        borderLeft: vm.isBlocker ? "3px solid var(--red, #ef4444)" : "3px solid var(--border)",
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
        <span style={{ fontFamily: "monospace", fontWeight: 700 }}>{vm.issueKey}</span>
        <div style={{ display: "flex", gap: 6, flexWrap: "wrap" }}>
          {vm.isBlocker ? <span className={vm.blockerBadgeClass}>Blocker</span> : null}
          <span className="badge badge-blue">{vm.issueType}</span>
          <span className="badge badge-gray">{vm.priority}</span>
        </div>
      </div>
      <div style={{ fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>{vm.summary}</div>
      <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 4 }}>
        {vm.scoreLabel}: <strong style={{ color: "var(--text-1)" }}>{vm.score}</strong>
        {" · "}{vm.status}
      </div>
      <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 6 }}>
        <strong>{vm.reasonLabel}:</strong> {vm.reason}
      </div>
      {(vm.relatedModule || vm.relatedEnvironment) ? (
        <div style={{ display: "flex", gap: 12, flexWrap: "wrap", fontSize: 11, color: "var(--text-3)" }}>
          {vm.relatedModule ? <span>{vm.relatedModuleLabel}: {vm.relatedModule}</span> : null}
          {vm.relatedEnvironment ? <span>{vm.relatedEnvironmentLabel}: {vm.relatedEnvironment}</span> : null}
        </div>
      ) : null}
    </li>
  );
}
