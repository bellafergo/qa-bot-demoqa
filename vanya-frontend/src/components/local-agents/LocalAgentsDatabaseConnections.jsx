import React from "react";

function ConnectionCard({ item }) {
  return (
    <div className="card" style={{ padding: "14px 16px", marginBottom: 10 }}>
      <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text-1)", marginBottom: 8 }}>{item.name}</div>
      <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginBottom: 10 }}>
        <span className="badge badge-gray" style={{ fontSize: 10, textTransform: "capitalize" }}>{item.databaseType}</span>
        <span className="badge badge-gray" style={{ fontSize: 10 }}>{item.agentLabel}</span>
      </div>
      <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 8, fontSize: 12 }}>
        <div>
          <div style={{ color: "var(--text-3)", fontSize: 10, fontWeight: 600 }}>Status</div>
          <div style={{ marginTop: 4 }}>
            <span className="badge badge-green" style={{ fontSize: 10 }}>{item.statusLabel}</span>
          </div>
        </div>
        <div>
          <div style={{ color: "var(--text-3)", fontSize: 10, fontWeight: 600 }}>{item.createdLabel}</div>
          <div style={{ marginTop: 4, fontWeight: 600, color: "var(--text-1)" }}>{item.createdText}</div>
        </div>
      </div>
    </div>
  );
}

export default function LocalAgentsDatabaseConnections({
  title,
  emptyTitle,
  emptyDesc,
  createLabel,
  connections,
  empty,
  creating,
  canCreate,
  noAgentHint,
  onCreate,
}) {
  return (
    <div style={{ marginTop: 20 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10, textTransform: "uppercase", letterSpacing: "0.06em" }}>
        {title}
      </div>
      {empty ? (
        <div className="card" style={{ padding: 24 }}>
          <div style={{ fontSize: 15, fontWeight: 600 }}>{emptyTitle}</div>
          <div style={{ fontSize: 13, color: "var(--text-2)", marginTop: 8, lineHeight: 1.55, maxWidth: 520 }}>{emptyDesc}</div>
          <button
            type="button"
            className="btn btn-primary btn-sm"
            style={{ marginTop: 16 }}
            disabled={creating || !canCreate}
            onClick={onCreate}
          >
            {creating ? "…" : createLabel}
          </button>
          {!canCreate && noAgentHint ? (
            <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 10 }}>{noAgentHint}</div>
          ) : null}
        </div>
      ) : (
        connections.map((item) => <ConnectionCard key={item.connectionId} item={item} />)
      )}
    </div>
  );
}
