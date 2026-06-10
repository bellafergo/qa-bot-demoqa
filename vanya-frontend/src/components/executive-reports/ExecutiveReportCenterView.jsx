import React from "react";
import ExecutiveReportScheduleCard from "./ExecutiveReportScheduleCard.jsx";
import ExecutiveReportPreviewCard from "./ExecutiveReportPreviewCard.jsx";

export default function ExecutiveReportCenterView({ vm }) {
  if (!vm?.show) return null;

  const cardLabels = {
    previewReportLabel: vm.previewReportLabel,
    editScheduleLabel: vm.editScheduleLabel,
    sendReportLabel: vm.sendReportLabel,
    sendDisabledNote: vm.sendDisabledNote,
  };

  if (vm.empty) {
    return (
      <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {vm.emptyMessage}
      </p>
    );
  }

  return (
    <>
      <div style={{ marginBottom: 16 }}>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
          {vm.schedulesLabel}
        </div>
        <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
          {vm.schedules.map((schedule) => (
            <ExecutiveReportScheduleCard
              key={schedule.schedule_id}
              schedule={schedule}
              labels={cardLabels}
            />
          ))}
        </ul>
      </div>

      {vm.preview ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.latestPreviewLabel}
          </div>
          <ExecutiveReportPreviewCard preview={vm.preview} labels={cardLabels} />
        </div>
      ) : null}
    </>
  );
}
