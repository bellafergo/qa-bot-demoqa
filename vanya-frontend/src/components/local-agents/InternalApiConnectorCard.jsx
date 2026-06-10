import React from "react";
import ApiEndpointCard from "./ApiEndpointCard.jsx";

export default function InternalApiConnectorCard({ connector, labels }) {
  if (!connector) return null;

  return (
    <li
      style={{
        marginBottom: 12,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)" }}>{connector.name}</strong>
        <span className="badge badge-blue">{connector.api_type}</span>
        <span className="badge badge-gray">{connector.environment}</span>
        <span className={connector.statusBadgeClass}>{connector.status}</span>
        <span style={{ fontSize: 12, color: "var(--text-2)" }}>
          {connector.endpoint_count} {labels?.endpointsLabel}
        </span>
      </div>
      {connector.endpoints?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {labels?.endpointsLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {connector.endpoints.map((endpoint) => (
              <ApiEndpointCard key={endpoint.endpoint_id} endpoint={endpoint} labels={labels} />
            ))}
          </ul>
        </div>
      ) : null}
    </li>
  );
}
