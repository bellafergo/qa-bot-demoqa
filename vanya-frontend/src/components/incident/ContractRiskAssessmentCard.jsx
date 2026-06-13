import React, { useCallback, useState } from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import ContractRiskFactorCard from "./ContractRiskFactorCard.jsx";
import ContractRiskPreviewModal from "./ContractRiskPreviewModal.jsx";
import IncidentInsightTracePanel from "../incidents/IncidentInsightTracePanel.jsx";

export default function ContractRiskAssessmentCard({ assessment, labels }) {
  const [previewOpen, setPreviewOpen] = useState(false);
  const openPreview = useCallback(() => setPreviewOpen(true), []);
  const closePreview = useCallback(() => setPreviewOpen(false), []);

  const contractName = assessment.contract?.service_name || assessment.contract_id;

  return (
    <li
      style={{
        marginBottom: 12,
        padding: "14px 16px",
        background: "var(--bg-2)",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)" }}>{contractName}</strong>
        <span className="badge badge-red">
          {labels.riskScoreLabel}: {assessment.risk_score}
        </span>
        <span className={assessment.riskBadgeClass}>
          {labels.riskLevelLabel}: {assessment.overall_risk_level}
        </span>
        <span className="badge badge-orange">
          {labels.confidenceLabel}: {assessment.confidenceText}
        </span>
      </div>
      {assessment.contractDrilldown ? (
        <div style={{ marginBottom: 8 }}>
          <EvidenceCorrelationDrilldownCell item={assessment.contractDrilldown} />
        </div>
      ) : null}
      <div style={{ color: "var(--text-2)", marginBottom: 10 }}>{assessment.summary}</div>
      {assessment.affected_journeys?.length ? (
        <div style={{ marginBottom: 8 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.affectedJourneysLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)" }}>
            {assessment.affected_journeys.map((journey) => (
              <li key={journey} style={{ marginBottom: 4 }}>
                {journey}
                {assessment.journeyDrilldowns
                  ?.filter((d) => d.title === journey || d.reason === journey)
                  .map((item) => (
                    <div key={item.related_entity_id} style={{ marginTop: 4 }}>
                      <EvidenceCorrelationDrilldownCell item={item} />
                    </div>
                  ))}
              </li>
            ))}
          </ul>
        </div>
      ) : null}
      {assessment.affected_modules?.length ? (
        <div style={{ marginBottom: 8 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.affectedModulesLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)" }}>
            {assessment.affected_modules.map((module) => (
              <li key={module} style={{ marginBottom: 4 }}>
                {module}
                {assessment.moduleDrilldowns
                  ?.filter((d) => d.title === module)
                  .map((item) => (
                    <div key={item.related_entity_id} style={{ marginTop: 4 }}>
                      <EvidenceCorrelationDrilldownCell item={item} />
                    </div>
                  ))}
              </li>
            ))}
          </ul>
        </div>
      ) : null}
      {assessment.affected_tests?.length ? (
        <div style={{ marginBottom: 8 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.affectedTestsLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)" }}>
            {assessment.affected_tests.map((testName) => (
              <li key={testName} style={{ marginBottom: 4 }}>
                {testName}
                {assessment.testDrilldowns
                  ?.filter((d) => d.title === testName)
                  .map((item) => (
                    <div key={item.related_entity_id} style={{ marginTop: 4 }}>
                      <EvidenceCorrelationDrilldownCell item={item} />
                    </div>
                  ))}
              </li>
            ))}
          </ul>
        </div>
      ) : null}
      {assessment.factors?.length ? (
        <div style={{ marginBottom: 10 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {labels.riskFactorsLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {assessment.factors.slice(0, 3).map((factor) => (
              <ContractRiskFactorCard key={factor.factor_id} factor={factor} severityLabel={labels.riskLevelLabel} />
            ))}
          </ul>
        </div>
      ) : null}
      {assessment.showTrace ? <IncidentInsightTracePanel trace={assessment.trace} /> : null}
      <button type="button" className="btn btn-secondary btn-sm" onClick={openPreview}>
        {labels.previewLabel}
      </button>
      <ContractRiskPreviewModal open={previewOpen} payload={assessment.previewPayload} onClose={closePreview} />
    </li>
  );
}
