import React, { useState } from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { formatTestDisplayNameWithMeta } from "../../utils/humanizeTestNameUtils.js";

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

export default function MemoryExplorerSection({ vm }) {
  const { t } = useLang();
  const [expanded, setExpanded] = useState(vm?.defaultExpanded || "");

  if (!vm?.show) return null;

  if (!vm.hasLinks && vm.modules.length > 0) {
    return (
      <div className="card" style={{ padding: "16px 20px", marginBottom: 16 }}>
        <div className="section-title" style={{ margin: 0 }}>{vm.title}</div>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: "12px 0 0" }}>{vm.emptyMessage}</p>
      </div>
    );
  }

  return (
    <div className="card" style={{ padding: "16px 20px", marginBottom: 16 }}>
      <div className="section-title" style={{ margin: "0 0 16px" }}>{vm.title}</div>
      <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
        {vm.modules.map((mod) => {
          const isOpen = expanded === mod.module;
          const total = mod.counts.routes + mod.counts.apis + mod.counts.tests + mod.counts.failure_clusters;
          return (
            <div
              key={mod.module}
              style={{
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
                borderRadius: 8,
                overflow: "hidden",
                background: "var(--bg-3, rgba(255,255,255,0.02))",
              }}
            >
              <button
                type="button"
                onClick={() => setExpanded(isOpen ? "" : mod.module)}
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
                  <div style={{ fontSize: 14, fontWeight: 700 }}>{mod.module}</div>
                  <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>
                    {mod.sectionLabels.routes} ({mod.counts.routes}) · {mod.sectionLabels.apis} ({mod.counts.apis}) · {mod.sectionLabels.tests} ({mod.counts.tests}) · {mod.sectionLabels.clusters} ({mod.counts.failure_clusters})
                  </div>
                </div>
                <span style={{ fontSize: 12, color: "var(--text-3)" }}>{isOpen ? "▾" : "▸"}</span>
              </button>

              {isOpen ? (
                <div style={{ padding: "0 14px 14px", borderTop: "1px solid var(--border, rgba(255,255,255,0.06))" }}>
                  {mod.summary ? (
                    <p style={{ fontSize: 12, color: "var(--text-2)", margin: "12px 0 0", lineHeight: 1.5 }}>
                      {mod.summary}
                    </p>
                  ) : null}

                  <SubSection title={mod.sectionLabels.routes} count={mod.counts.routes} empty={mod.emptyLabels.routes}>
                    <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, fontFamily: "monospace", color: "var(--text-2)", lineHeight: 1.7 }}>
                      {mod.routes.map((r) => (
                        <li key={r.url}>
                          {r.url}
                          {r.file_path ? <span style={{ color: "var(--text-3)", marginLeft: 8 }}>({r.file_path})</span> : null}
                        </li>
                      ))}
                    </ul>
                  </SubSection>

                  <SubSection title={mod.sectionLabels.apis} count={mod.counts.apis} empty={mod.emptyLabels.apis}>
                    <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, fontFamily: "monospace", color: "var(--text-2)", lineHeight: 1.7 }}>
                      {mod.apis.map((a) => (
                        <li key={`${a.method}-${a.url}`}>
                          {a.method} {a.url}
                          {a.file_path ? <span style={{ color: "var(--text-3)", marginLeft: 8 }}>({a.file_path})</span> : null}
                        </li>
                      ))}
                    </ul>
                  </SubSection>

                  <SubSection title={mod.sectionLabels.tests} count={mod.counts.tests} empty={mod.emptyLabels.tests}>
                    <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
                      {mod.tests.map((tc) => {
                        const testMeta = formatTestDisplayNameWithMeta({
                          testCaseId: tc.test_case_id,
                          testName: tc.name,
                        }, t);
                        return (
                        <li key={tc.test_case_id}>
                          <strong title={testMeta.showTechnicalId ? testMeta.technicalId : undefined}>{testMeta.display}</strong>
                          {tc.last_run_status ? <span className="badge badge-gray" style={{ marginLeft: 6 }}>{tc.last_run_status}</span> : null}
                        </li>
                        );
                      })}
                    </ul>
                  </SubSection>

                  <SubSection title={mod.sectionLabels.clusters} count={mod.counts.failure_clusters} empty={mod.emptyLabels.clusters}>
                    <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
                      {mod.failure_clusters.map((c) => (
                        <li key={c.cluster_id}>
                          <strong>{c.cluster_id}</strong>
                          {" — "}
                          {c.category}
                          {" · "}
                          {c.occurrences}×
                          <span className={c.confidenceBadgeClass} style={{ marginLeft: 6 }}>{c.confidence}</span>
                        </li>
                      ))}
                    </ul>
                  </SubSection>
                </div>
              ) : null}
            </div>
          );
        })}
      </div>
    </div>
  );
}
