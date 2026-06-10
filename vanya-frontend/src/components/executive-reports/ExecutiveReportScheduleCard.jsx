import React from "react";

export default function ExecutiveReportScheduleCard({ schedule, labels }) {
  if (!schedule) return null;

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)" }}>{schedule.reportTypeLabel || schedule.name}</strong>
        <span className={schedule.enabled ? "badge badge-green" : "badge badge-gray"}>{schedule.enabledLabel}</span>
        <span className="badge badge-blue">{schedule.frequencyLabel}</span>
      </div>
      <div style={{ fontSize: 12, color: "var(--text-2)" }}>
        <div>{schedule.recipientsText}</div>
        <div style={{ marginTop: 4 }}>
          <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{schedule.nextRunPreviewLabel}: </span>
          {schedule.nextRunPreviewText}
        </div>
      </div>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginTop: 10 }}>
        <button type="button" className="btn btn-secondary btn-sm">
          {labels?.previewReportLabel}
        </button>
        <button type="button" className="btn btn-ghost btn-sm">
          {labels?.editScheduleLabel}
        </button>
      </div>
    </li>
  );
}
