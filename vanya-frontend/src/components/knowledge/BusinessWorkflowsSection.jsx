import React, { useState } from "react";

function SubSection({ title, count, empty, children }) {
  return (
    <div style={{ marginTop: 12 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
        {title} ({count})
      </div>
      {count === 0 ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", fontStyle: "italic" }}>{empty}</div>
      ) : children}
    </div>
  );
}

export default function BusinessWorkflowsSection({ vm }) {
  const [expanded, setExpanded] = useState("");

  if (!vm?.show) return null;

  return (
    <div className="card" style={{ padding: "16px 20px", marginBottom: 16 }}>
      <div className="section-title" style={{ margin: "0 0 16px" }}>{vm.title}</div>
      {!vm.hasWorkflows ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{vm.emptyMessage}</p>
      ) : (
        <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
          {vm.workflows.map((wf) => {
            const isOpen = expanded === wf.type;
            return (
              <div
                key={wf.type}
                style={{
                  border: "1px solid var(--border, rgba(255,255,255,0.08))",
                  borderRadius: 8,
                  overflow: "hidden",
                  background: "var(--bg-3, rgba(255,255,255,0.02))",
                }}
              >
                <button
                  type="button"
                  onClick={() => setExpanded(isOpen ? "" : wf.type)}
                  style={{
                    width: "100%",
                    display: "flex",
                    justifyContent: "space-between",
                    alignItems: "center",
                    gap: 12,
                    padding: "12px 14px",
                    background: "transparent",
                    border: "none",
                    color: "var(--text-1)",
                    cursor: "pointer",
                    textAlign: "left",
                  }}
                >
                  <div>
                    <div style={{ fontSize: 14, fontWeight: 700 }}>{wf.name}</div>
                    <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>
                      {wf.sectionLabels.routes} ({wf.counts.routes}) · {wf.sectionLabels.apis} ({wf.counts.apis}) · {wf.sectionLabels.tests} ({wf.counts.tests}) · {wf.sectionLabels.clusters} ({wf.counts.clusters})
                    </div>
                  </div>
                  <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                    <span className={wf.confidenceBadgeClass}>{wf.confidenceLabel}</span>
                    <span style={{ fontSize: 12, color: "var(--text-3)" }}>{isOpen ? "▾" : "▸"}</span>
                  </div>
                </button>

                {isOpen ? (
                  <div style={{ padding: "0 14px 14px", borderTop: "1px solid var(--border, rgba(255,255,255,0.06))" }}>
                    {wf.summary ? (
                      <p style={{ fontSize: 12, color: "var(--text-2)", margin: "12px 0 0", lineHeight: 1.5 }}>
                        <strong>{wf.sectionLabels.summary}: </strong>
                        {wf.summary}
                      </p>
                    ) : null}

                    <SubSection title={wf.sectionLabels.modules} count={wf.modules.length} empty={wf.emptyLabels.modules}>
                      <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
                        {wf.modules.map((m) => (
                          <span key={m} className="badge badge-gray">{m}</span>
                        ))}
                      </div>
                    </SubSection>

                    <SubSection title={wf.sectionLabels.routes} count={wf.counts.routes} empty={wf.emptyLabels.routes}>
                      <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, fontFamily: "monospace", color: "var(--text-2)", lineHeight: 1.7 }}>
                        {wf.routes.map((r) => (
                          <li key={r}>{r}</li>
                        ))}
                      </ul>
                    </SubSection>

                    <SubSection title={wf.sectionLabels.apis} count={wf.counts.apis} empty={wf.emptyLabels.apis}>
                      <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, fontFamily: "monospace", color: "var(--text-2)", lineHeight: 1.7 }}>
                        {wf.apis.map((a) => (
                          <li key={a}>{a}</li>
                        ))}
                      </ul>
                    </SubSection>

                    <SubSection title={wf.sectionLabels.tests} count={wf.counts.tests} empty={wf.emptyLabels.tests}>
                      <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
                        {wf.tests.map((tc) => (
                          <li key={tc}><strong>{tc}</strong></li>
                        ))}
                      </ul>
                    </SubSection>

                    <SubSection title={wf.sectionLabels.clusters} count={wf.counts.clusters} empty={wf.emptyLabels.clusters}>
                      <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
                        {wf.failure_clusters.map((c) => (
                          <li key={c}><strong>{c}</strong></li>
                        ))}
                      </ul>
                    </SubSection>
                  </div>
                ) : null}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}
