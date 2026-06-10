import React from "react";
import ContractRiskCard from "./ContractRiskCard.jsx";

export default function ApiContractCard({ contract, labels }) {
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
        <strong style={{ color: "var(--text-1)" }}>{contract.service_name}</strong>
        <span className={contract.riskBadgeClass}>
          {labels.riskLevelLabel}: {contract.riskLevel}
        </span>
        <span className="badge badge-blue">
          {labels.changeCountLabel}: {contract.changeCount}
        </span>
      </div>
      <div style={{ color: "var(--text-2)", marginBottom: 6 }}>
        {labels.methodLabel}: {contract.method} · {labels.endpointLabel}: {contract.endpoint}
      </div>
      <div style={{ color: "var(--text-3)", marginBottom: 10, fontSize: 12 }}>
        {labels.versionLabel}: {contract.version}
      </div>
      {contract.assessment ? (
        <ContractRiskCard
          assessment={contract.assessment}
          changes={contract.changes}
          labels={labels}
          drilldownItem={contract.drilldownItem}
        />
      ) : null}
    </li>
  );
}
