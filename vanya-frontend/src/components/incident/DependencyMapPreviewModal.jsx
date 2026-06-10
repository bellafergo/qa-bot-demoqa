import React from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS } from "../../utils/enterpriseDependencyMapViewUtils.js";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";

export default function DependencyMapPreviewModal({ open, payload, onClose }) {
  const { t } = useLang();
  if (!open || !payload) return null;

  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex: 10050,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
      aria-labelledby="dependency-map-preview-title"
    >
      <div className="card" style={{ width: "min(720px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="dependency-map-preview-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {payload.title}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>
            {t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.previewSubtitle)}
          </div>
        </div>
        <div style={{ padding: "16px 20px", maxHeight: "70vh", overflowY: "auto" }}>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
            <span className="badge badge-gray">{payload.nodeType}</span>
            <span className="badge badge-red">
              {payload.labels.riskLevel}: {payload.riskLevel}
            </span>
            <span className="badge badge-blue">{payload.confidence}</span>
          </div>
          {payload.description ? (
            <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, margin: "0 0 12px" }}>
              {payload.description}
            </p>
          ) : null}
          {payload.drilldownItem ? (
            <div style={{ marginBottom: 12 }}>
              <EvidenceCorrelationDrilldownCell item={payload.drilldownItem} />
            </div>
          ) : null}
          {payload.dependencies?.length ? (
            <div style={{ marginBottom: 10 }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {payload.labels.dependencies}
              </div>
              <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)", fontSize: 13 }}>
                {payload.dependencies.map((node) => (
                  <li key={node.node_id}>{node.name}</li>
                ))}
              </ul>
            </div>
          ) : null}
          {payload.dependents?.length ? (
            <div style={{ marginBottom: 10 }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {payload.labels.dependents}
              </div>
              <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)", fontSize: 13 }}>
                {payload.dependents.map((node) => (
                  <li key={node.node_id}>{node.name}</li>
                ))}
              </ul>
            </div>
          ) : null}
          {payload.relatedJourneys?.length ? (
            <div style={{ marginBottom: 8, fontSize: 13, color: "var(--text-2)" }}>
              <strong>{payload.labels.relatedJourneys}:</strong> {payload.relatedJourneys.join(", ")}
            </div>
          ) : null}
          {payload.relatedContracts?.length ? (
            <div style={{ marginBottom: 8, fontSize: 13, color: "var(--text-2)" }}>
              <strong>{payload.labels.relatedContracts}:</strong> {payload.relatedContracts.join(", ")}
            </div>
          ) : null}
          {payload.relatedValidations?.length ? (
            <div style={{ marginBottom: 12, fontSize: 13, color: "var(--text-2)" }}>
              <strong>{payload.labels.relatedValidations}:</strong> {payload.relatedValidations.join(", ")}
            </div>
          ) : null}
          <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
            {payload.readOnlyNote}
          </p>
        </div>
        <div style={{ padding: "12px 20px", borderTop: "1px solid var(--border)", textAlign: "right" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onClose}>
            {t("common.close")}
          </button>
        </div>
      </div>
    </div>
  );
}
