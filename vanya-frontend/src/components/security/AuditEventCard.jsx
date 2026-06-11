import React from "react";

export default function AuditEventCard({ event }) {
  if (!event) return null;

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "10px 12px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-2)",
        listStyle: "none",
        fontSize: 12,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
        <span style={{ fontWeight: 600, fontSize: 13 }}>{event.eventTypeLabel}</span>
        <span className={event.resultBadgeClass}>{event.resultLabel}</span>
      </div>
      <div style={{ color: "var(--text-3)", marginBottom: 4 }}>
        {event.timestampLabel}: {event.timestamp}
      </div>
      <div style={{ color: "var(--text-2)", lineHeight: 1.45 }}>
        <div>{event.userLabel}: {event.userEmail !== "—" ? event.userEmail : event.userId}</div>
        <div>{event.resourceLabel}: {event.resourceType} / {event.resourceId}</div>
        <div>{event.actionLabel}: {event.action}</div>
      </div>
    </li>
  );
}
