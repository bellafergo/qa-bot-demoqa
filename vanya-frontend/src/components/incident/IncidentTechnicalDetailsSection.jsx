import React from "react";
import ReportCollapsibleSection from "./ReportCollapsibleSection.jsx";

export default function IncidentTechnicalDetailsSection({ vm }) {
  if (!vm?.show) return null;

  return (
    <ReportCollapsibleSection title={vm.title} defaultOpen={false}>
      <div style={{ display: "flex", flexDirection: "column", gap: 8, fontSize: 13, color: "var(--text-2)" }}>
        {vm.engineVersion ? (
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.engineLabel}: </span>
            <span style={{ fontFamily: "ui-monospace, monospace", fontSize: 12 }}>{vm.engineVersion}</span>
          </div>
        ) : null}
        {vm.reportId ? (
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.reportIdLabel}: </span>
            <span style={{ fontFamily: "ui-monospace, monospace", fontSize: 12 }}>{vm.reportId}</span>
          </div>
        ) : null}
        {vm.incidentId ? (
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.incidentIdLabel}: </span>
            <span style={{ fontFamily: "ui-monospace, monospace", fontSize: 12 }}>{vm.incidentId}</span>
          </div>
        ) : null}
        {vm.analyzeOnlyLabel ? (
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.analyzeOnlyLabel}: </span>
            {vm.analyzeOnlyText}
          </div>
        ) : null}
      </div>
    </ReportCollapsibleSection>
  );
}
