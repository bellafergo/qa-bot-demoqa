import React from "react";

function AgentCard({ item, selected, labels, onSelect }) {
  return (
    <button
      type="button"
      onClick={() => onSelect(item.agentId)}
      className="card"
      style={{
        width: "100%",
        textAlign: "left",
        padding: "14px 16px",
        marginBottom: 10,
        cursor: "pointer",
        border: selected ? "1px solid var(--accent)" : "1px solid var(--border)",
        background: selected ? "var(--accent-light)" : undefined,
        boxShadow: item.needsAttention && !selected ? "inset 3px 0 0 #f59e0b" : undefined,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 10, alignItems: "flex-start", marginBottom: 10 }}>
        <div>
          <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text-1)" }}>{item.name}</div>
          {item.isFoundation ? (
            <div style={{ fontSize: 10, color: "var(--text-3)", marginTop: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>
              Foundation
            </div>
          ) : null}
        </div>
        <span className={item.status.badgeClass} style={{ fontSize: 10, flexShrink: 0 }}>
          {item.statusLabel}
        </span>
      </div>

      <div style={{ display: "flex", flexWrap: "wrap", gap: 6, marginBottom: 10 }}>
        <span className="badge badge-gray" style={{ fontSize: 10 }}>{item.environment}</span>
        {item.capabilityLabels.map((label) => (
          <span key={label} className="badge badge-gray" style={{ fontSize: 10 }}>{label}</span>
        ))}
      </div>

      <div style={{ display: "flex", gap: 16, fontSize: 12, color: "var(--text-3)" }}>
        <span>{labels.created} {item.createdText}</span>
        <span>{labels.lastSeen} {item.lastSeenText}</span>
      </div>
    </button>
  );
}

export default function LocalAgentsList({ agents, selectedId, labels, onSelect }) {
  return (
    <div>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10, textTransform: "uppercase", letterSpacing: "0.06em" }}>
        {labels.title}
      </div>
      {agents.map((item) => (
        <AgentCard
          key={item.agentId}
          item={item}
          selected={selectedId === item.agentId}
          labels={labels}
          onSelect={onSelect}
        />
      ))}
    </div>
  );
}
