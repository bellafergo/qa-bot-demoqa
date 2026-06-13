import React from "react";

export default function IncidentEvidenceSummary({ vm }) {
  if (!vm) return null;

  return (
    <div style={{ marginBottom: 12 }}>
      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em", marginBottom: 6 }}>
        {vm.title}
      </div>
      {vm.empty ? (
        <div style={{ fontSize: 13, color: "var(--text-3)", fontStyle: "italic" }}>{vm.emptyMessage}</div>
      ) : (
        <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.55 }}>
          {vm.bullets.map((bullet) => (
            <li key={bullet} style={{ marginBottom: 4 }}>{bullet}</li>
          ))}
        </ul>
      )}
    </div>
  );
}
