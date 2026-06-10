import React, { useState } from "react";
import InternalApiConnectorCard from "./InternalApiConnectorCard.jsx";
import ApiValidationRequestCard from "./ApiValidationRequestCard.jsx";
import InternalApiValidationPreviewModal from "./InternalApiValidationPreviewModal.jsx";
import EvidenceCorrelationDrilldownCell from "../incident/EvidenceCorrelationDrilldownCell.jsx";

export default function InternalApiConnectorView({ vm }) {
  const [previewPayload, setPreviewPayload] = useState(null);

  if (!vm?.show) return null;

  const labels = {
    endpointsLabel: vm.endpointsLabel,
    validationsLabel: vm.validationsLabel,
    readOnlyLabel: vm.readOnlyLabel || vm.readOnlyNote,
    blockedLabel: vm.blockedLabel,
    approvalRequiredLabel: vm.approvalRequiredLabel,
    previewValidationLabel: vm.previewValidationLabel,
    executeValidationLabel: vm.executeValidationLabel,
    executeDisabledNote: vm.executeDisabledNote,
  };

  if (vm.empty) {
    return (
      <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5, fontStyle: "italic" }}>
        {vm.emptyMessage}
      </p>
    );
  }

  return (
    <>
      {vm.summary ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>{vm.summary}</div>
      ) : null}

      <ul style={{ margin: "0 0 16px", padding: 0, listStyle: "none" }}>
        {vm.connectors.map((connector) => (
          <InternalApiConnectorCard key={connector.connector_id} connector={connector} labels={labels} />
        ))}
      </ul>

      {vm.validations?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.validationsLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {vm.validations.map((validation) => (
              <ApiValidationRequestCard
                key={validation.request_id}
                validation={validation}
                labels={labels}
                onPreview={(item) => setPreviewPayload(item.previewPayload)}
              />
            ))}
          </ul>
        </div>
      ) : null}

      {previewPayload?.drilldownItem ? (
        <div style={{ marginTop: 12 }}>
          <EvidenceCorrelationDrilldownCell item={previewPayload.drilldownItem} />
        </div>
      ) : null}

      <InternalApiValidationPreviewModal
        open={Boolean(previewPayload)}
        payload={previewPayload}
        onClose={() => setPreviewPayload(null)}
      />
    </>
  );
}
