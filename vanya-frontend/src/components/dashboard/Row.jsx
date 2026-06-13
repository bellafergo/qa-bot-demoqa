import React from "react";

export default function Row({ label, value, accent }) {
  return (
    <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", fontSize: 13 }}>
      <span style={{ color: "var(--text-3)", fontWeight: 400 }}>{label}</span>
      <span style={{ fontWeight: 600, color: accent || "var(--text-1)" }}>{value}</span>
    </div>
  );
}
