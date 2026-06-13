import React from "react";
import IncidentEvidenceSummary from "./IncidentEvidenceSummary.jsx";
import IncidentRootCauseContributors from "./IncidentRootCauseContributors.jsx";
import IncidentEvidenceSources from "./IncidentEvidenceSources.jsx";
import IncidentWhyExplanation from "./IncidentWhyExplanation.jsx";

export default function IncidentInsightTracePanel({ trace }) {
  if (!trace?.show) return null;

  return (
    <div
      style={{
        marginTop: 12,
        paddingTop: 12,
        borderTop: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      <IncidentWhyExplanation vm={trace.whyExplanation} />
      <IncidentEvidenceSummary vm={trace.evidenceSummary} />
      <IncidentRootCauseContributors vm={trace.rootCauseContributors} />
      <IncidentEvidenceSources vm={trace.evidenceSources} />
    </div>
  );
}
