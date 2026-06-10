import React from "react";

export default function ApiEndpointCard({ endpoint, labels }) {
  if (!endpoint) return null;

  return (
    <li
      style={{
        marginBottom: 6,
        padding: "8px 10px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 6,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 12,
        lineHeight: 1.45,
        display: "flex",
        gap: 8,
        flexWrap: "wrap",
        alignItems: "center",
      }}
    >
      <code style={{ color: "var(--text-1)" }}>
        {endpoint.method} {endpoint.path_label}
      </code>
      {endpoint.read_only ? (
        <span className="badge badge-blue">{labels?.readOnlyLabel}</span>
      ) : (
        <span className="badge badge-red">{endpoint.blockedLabel || labels?.blockedLabel}</span>
      )}
    </li>
  );
}
