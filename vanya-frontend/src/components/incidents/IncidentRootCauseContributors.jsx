import React from "react";

export default function IncidentRootCauseContributors({ vm }) {
  if (!vm) return null;

  return (
    <div style={{ marginBottom: 12 }}>
      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em", marginBottom: 6 }}>
        {vm.title}
      </div>
      {vm.empty ? (
        <div style={{ fontSize: 13, color: "var(--text-3)", fontStyle: "italic" }}>{vm.emptyMessage}</div>
      ) : (
        <ol style={{ margin: 0, paddingLeft: 20, fontSize: 13, color: "var(--text-2)", lineHeight: 1.55 }}>
          {vm.contributors.map((item, index) => (
            <li key={`${item.title}-${index}`} style={{ marginBottom: 8 }}>
              <div style={{ fontWeight: 600, color: "var(--text-1)" }}>{item.title}</div>
              <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 2 }}>
                {vm.confidenceLabel} {item.confidenceText}
              </div>
            </li>
          ))}
        </ol>
      )}
    </div>
  );
}
