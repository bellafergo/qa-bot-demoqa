import React, { useCallback, useState } from "react";
import DependencyNodeCard from "./DependencyNodeCard.jsx";
import DependencyEdgeCard from "./DependencyEdgeCard.jsx";
import DependencyMapPreviewModal from "./DependencyMapPreviewModal.jsx";

export default function EnterpriseDependencyMapView({ vm }) {
  const [previewOpen, setPreviewOpen] = useState(false);
  const [previewPayload, setPreviewPayload] = useState(null);

  const openPreview = useCallback((node) => {
    setPreviewPayload(node.previewPayload);
    setPreviewOpen(true);
  }, []);

  const closePreview = useCallback(() => {
    setPreviewOpen(false);
    setPreviewPayload(null);
  }, []);

  if (!vm?.map) return null;

  return (
    <>
      <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
        {vm.map.summary}
      </div>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
        <span className="badge badge-blue">
          {vm.nodeCountLabel}: {vm.map.nodeCount}
        </span>
        <span className="badge badge-blue">
          {vm.relationshipCountLabel}: {vm.map.relationshipCount}
        </span>
        <span className="badge badge-orange">
          {vm.map.confidenceText}
        </span>
      </div>
      {vm.map.affectedAreas?.length ? (
        <div style={{ marginBottom: 12 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.affectedAreasLabel}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)" }}>{vm.map.affectedAreas.join(", ")}</div>
        </div>
      ) : null}
      <div style={{ marginBottom: 12 }}>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
          {vm.riskDistributionLabel}
        </div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
          {Object.entries(vm.map.riskDistribution).map(([level, count]) => (
            <span key={level} className="badge badge-gray">
              {level}: {count}
            </span>
          ))}
        </div>
      </div>
      {vm.map.graphChains?.map((chain) => (
        <div
          key={chain[0]?.node_id}
          style={{
            marginBottom: 14,
            padding: "12px 10px",
            borderRadius: 8,
            background: "var(--bg-1, rgba(0,0,0,0.12))",
          }}
        >
          <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
            {chain.map((node, index) => (
              <div key={node.node_id} style={{ display: "flex", alignItems: "stretch", gap: 4 }}>
                <div
                  style={{
                    flex: 1,
                    padding: "8px 10px",
                    borderRadius: 8,
                    border: `2px solid ${node.riskColor || "var(--text-3)"}`,
                    background: "var(--bg-2)",
                    fontSize: 12,
                  }}
                >
                  <strong style={{ color: "var(--text-1)" }}>{node.name}</strong>
                  <div>
                    <span className={node.riskBadgeClass}>{node.risk_level}</span>
                  </div>
                </div>
                {index < chain.length - 1 ? (
                  <div style={{ display: "flex", alignItems: "center", color: "var(--text-3)", fontWeight: 600 }}>↓</div>
                ) : null}
              </div>
            ))}
          </div>
        </div>
      ))}
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
        {vm.dependenciesLabel}
      </div>
      <ul style={{ margin: "0 0 12px", padding: 0, listStyle: "none" }}>
        {vm.map.edges.slice(0, 8).map((edge) => (
          <DependencyEdgeCard key={edge.edge_id} edge={edge} />
        ))}
      </ul>
      <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
        {vm.map.nodes.slice(0, 6).map((node) => (
          <DependencyNodeCard
            key={node.node_id}
            node={node}
            riskLevelLabel={vm.riskDistributionLabel}
            previewLabel={vm.previewLabel}
            onPreview={openPreview}
          />
        ))}
      </ul>
      <DependencyMapPreviewModal open={previewOpen} payload={previewPayload} onClose={closePreview} />
    </>
  );
}
