import React from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { DATA_JOURNEY_VALIDATION_I18N_KEYS } from "../../utils/dataJourneyValidationViewUtils.js";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";

export default function DataJourneyPreviewModal({ open, payload, onClose }) {
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
      aria-labelledby="data-journey-preview-title"
    >
      <div className="card" style={{ width: "min(720px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="data-journey-preview-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {payload.title}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>
            {t(DATA_JOURNEY_VALIDATION_I18N_KEYS.previewSubtitle)}
          </div>
        </div>
        <div style={{ padding: "16px 20px", maxHeight: "70vh", overflowY: "auto" }}>
          {payload.description ? (
            <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, margin: "0 0 12px" }}>
              {payload.description}
            </p>
          ) : null}
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
            <span className="badge badge-blue">
              {payload.labels.journeyStatus}: {payload.journeyStatus}
            </span>
            <span className="badge badge-blue">
              {payload.labels.completedStages}: {payload.completedStages}
            </span>
            <span className="badge badge-blue">
              {payload.labels.confidence}: {payload.confidence}
            </span>
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, margin: "0 0 12px" }}>
            {payload.summary}
          </p>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {payload.labels.stages}
          </div>
          <ul style={{ margin: "0 0 12px", padding: 0, listStyle: "none" }}>
            {payload.stages.map((stage) => (
              <li
                key={stage.name}
                style={{
                  marginBottom: 8,
                  padding: "10px 12px",
                  borderRadius: 8,
                  background: "var(--bg-2)",
                  border: "1px solid var(--border, rgba(255,255,255,0.08))",
                }}
              >
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 4 }}>
                  <strong style={{ color: "var(--text-1)" }}>{stage.name}</strong>
                  <span className="badge badge-gray">{stage.stageType}</span>
                  <span className="badge badge-orange">
                    {payload.labels.stageStatus}: {stage.status}
                  </span>
                </div>
                {stage.drilldownItems?.length ? (
                  <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
                    {stage.drilldownItems.map((item) => (
                      <EvidenceCorrelationDrilldownCell
                        key={`${item.related_entity_type}:${item.related_entity_id}`}
                        item={item}
                      />
                    ))}
                  </div>
                ) : null}
              </li>
            ))}
          </ul>
          {payload.missingStages?.length ? (
            <div style={{ marginBottom: 10 }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {payload.labels.missingStages}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)" }}>{payload.missingStages.join(", ")}</div>
            </div>
          ) : null}
          {payload.inconsistentStages?.length ? (
            <div style={{ marginBottom: 10 }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {payload.labels.inconsistentStages}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)" }}>{payload.inconsistentStages.join(", ")}</div>
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
