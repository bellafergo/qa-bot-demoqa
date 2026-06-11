import React from "react";
import AuditEventCard from "./AuditEventCard.jsx";

export default function AuditEventsView({ vm, onFilterChange }) {
  if (!vm?.show) return null;

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))", gap: 10 }}>
        <div style={{ textAlign: "center", padding: "8px 6px", borderRadius: 6, background: "var(--bg-2)" }}>
          <div style={{ fontSize: 18, fontWeight: 700 }}>{vm.totalEvents}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{vm.summaryTotalLabel}</div>
        </div>
        <div style={{ padding: "8px 10px", borderRadius: 6, background: "var(--bg-2)" }}>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 4 }}>{vm.summaryRecentLabel}</div>
          <div style={{ fontSize: 12, fontWeight: 600 }}>{vm.latestActivity}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>{vm.latestActivityDetail}</div>
        </div>
      </div>

      <div>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
          {vm.filterEventTypeLabel}
        </div>
        <select
          className="input"
          value={vm.selectedEventType}
          onChange={(e) => onFilterChange?.(e.target.value)}
          style={{ maxWidth: 360 }}
        >
          <option value="">{vm.filterAllLabel}</option>
          {vm.eventTypeOptions.map((option) => (
            <option key={option.value} value={option.value}>{option.label}</option>
          ))}
        </select>
      </div>

      {vm.empty ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
          {vm.emptyMessage}
        </p>
      ) : (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.latestEventsLabel}
          </div>
          <ul style={{ margin: 0, padding: 0 }}>
            {vm.events.map((event) => (
              <AuditEventCard key={event.eventId} event={event} />
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
