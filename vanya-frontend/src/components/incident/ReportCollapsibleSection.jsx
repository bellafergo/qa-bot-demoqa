import React from "react";

export default function ReportCollapsibleSection({
  title,
  subtitle = "",
  defaultOpen = false,
  hidden = false,
  children,
}) {
  if (hidden || children == null) return null;

  return (
    <div
      className="card"
      style={{
        marginBottom: 16,
        padding: 0,
        overflow: "hidden",
      }}
    >
      <details open={defaultOpen ? true : undefined}>
        <summary
          style={{
            cursor: "pointer",
            listStyle: "none",
            padding: "14px 18px",
            borderBottom: defaultOpen ? "1px solid var(--border)" : "none",
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            gap: 12,
          }}
        >
          <div>
            <div style={{ fontSize: 13, fontWeight: 700, color: "var(--text-1)" }}>{title}</div>
            {subtitle ? (
              <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4, lineHeight: 1.45 }}>{subtitle}</div>
            ) : null}
          </div>
          <span style={{ fontSize: 11, color: "var(--text-3)", flexShrink: 0 }} aria-hidden>
            {defaultOpen ? "▾" : "▸"}
          </span>
        </summary>
        <div style={{ padding: "16px 18px" }}>{children}</div>
      </details>
    </div>
  );
}
