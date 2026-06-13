import React from "react";

export default function WidgetCard({ title, subtitle, children }) {
  return (
    <div className="card" style={{ padding: "20px 24px" }}>
      <div style={{ marginBottom: 16 }}>
        <div className="section-title" style={{ margin: 0 }}>{title}</div>
        {subtitle && <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{subtitle}</div>}
      </div>
      {children}
    </div>
  );
}
