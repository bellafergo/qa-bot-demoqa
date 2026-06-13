import React from "react";

export default function IncidentWhyExplanation({ vm }) {
  if (!vm?.show) return null;

  return (
    <div
      style={{
        marginBottom: 12,
        padding: "12px 14px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-1, rgba(0,0,0,0.12))",
      }}
    >
      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em", marginBottom: 6 }}>
        {vm.title}
      </div>
      <div style={{ fontSize: 13, color: "var(--text-1)", fontWeight: 600, lineHeight: 1.5, marginBottom: 8 }}>
        {vm.prefix}
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
