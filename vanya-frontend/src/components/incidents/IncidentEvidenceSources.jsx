import React, { useState } from "react";

export default function IncidentEvidenceSources({ vm }) {
  const [open, setOpen] = useState(false);
  if (!vm) return null;

  return (
    <div style={{ marginBottom: 12 }}>
      <button
        type="button"
        onClick={() => setOpen((v) => !v)}
        style={{
          display: "flex",
          alignItems: "center",
          gap: 8,
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
        <span>{open ? "▾" : "▸"}</span>
        <span>{vm.title}</span>
      </button>
      {open ? (
        vm.empty ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", fontStyle: "italic", marginTop: 6 }}>{vm.emptyMessage}</div>
        ) : (
          <div
            style={{
              display: "grid",
              gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))",
              gap: 8,
              marginTop: 8,
            }}
          >
            {vm.sources.map((source) => (
              <div
                key={source.key}
                style={{
                  padding: "8px 10px",
                  borderRadius: 8,
                  border: "1px solid var(--border)",
                  background: "var(--bg-2)",
                  fontSize: 12,
                }}
              >
                <div style={{ color: "var(--text-3)" }}>{source.label}</div>
                <div style={{ fontSize: 16, fontWeight: 700, color: "var(--text-1)", marginTop: 4 }}>{source.count}</div>
              </div>
            ))}
          </div>
        )
      ) : null}
    </div>
  );
}
