// src/pages/KnowledgePage.jsx
/**
 * System Memory — per-project App Knowledge Graph (Phase 1).
 */
import React, { useCallback, useEffect, useState } from "react";
import { getProjectKnowledge, refreshProjectKnowledge, apiErrorMessage } from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import KpiStrip from "../components/KpiStrip.jsx";
import InitializeProjectPanel from "../components/InitializeProjectPanel.jsx";
import {
  computeMemoryDepth,
  formatMemoryDepthLabel,
  formatSourcesLabel,
} from "../utils/knowledgeDepthUtils.js";

function fmtTs(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short", day: "numeric", hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

function riskBadge(score) {
  const v = Number(score) || 0;
  if (v >= 75) return "badge badge-red";
  if (v >= 50) return "badge badge-orange";
  if (v >= 25) return "badge badge-blue";
  return "badge badge-gray";
}

function knowledgeKpiItems(knowledge, t) {
  if (!knowledge) return [];
  const modN = (knowledge.modules || []).length;
  const routeN = (knowledge.routes || []).length;
  const apiN = (knowledge.apis || []).length;
  const testN = (knowledge.related_tests || []).length;
  const relN = (knowledge.workflows || []).length + (knowledge.forms || []).length;
  const depth = computeMemoryDepth(knowledge);
  const signalCount = modN + routeN + apiN;
  const confKey =
    signalCount >= 5 && testN >= 3 ? "knowledge.kpi.conf_high"
      : signalCount >= 2 || testN >= 1 ? "knowledge.kpi.conf_medium"
        : "knowledge.kpi.conf_low";
  return [
    { key: "mod", label: t("knowledge.kpi.modules"), value: modN },
    { key: "routes", label: t("knowledge.kpi.routes"), value: routeN },
    { key: "apis", label: t("knowledge.kpi.apis"), value: apiN },
    { key: "tests", label: t("knowledge.kpi.tests"), value: testN },
    { key: "rel", label: t("knowledge.kpi.relations"), value: relN },
    {
      key: "depth",
      label: t("knowledge.kpi.memory_depth"),
      value: `${depth.score}% (${formatMemoryDepthLabel(depth.label, t)})`,
    },
    { key: "conf", label: t("knowledge.kpi.confidence"), value: t(confKey) },
    { key: "upd", label: t("knowledge.updated"), value: fmtTs(knowledge.updated_at) },
  ];
}

function riskLevelBadge(level) {
  const lv = (level || "LOW").toUpperCase();
  if (lv === "CRITICAL") return "badge badge-red";
  if (lv === "HIGH") return "badge badge-orange";
  if (lv === "MEDIUM") return "badge badge-blue";
  return "badge badge-gray";
}

function Section({ title, count, children, empty }) {
  return (
    <div className="card" style={{ padding: "16px 20px", marginBottom: 16 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12 }}>
        <div className="section-title" style={{ margin: 0 }}>{title}</div>
        <span className="badge badge-gray">{count}</span>
      </div>
      {count === 0 ? (
        <div style={{ fontSize: 13, color: "var(--text-3)" }}>{empty}</div>
      ) : children}
    </div>
  );
}

export default function KnowledgePage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const projectId = currentProject?.id;

  const [knowledge, setKnowledge] = useState(null);
  const [loading, setLoading] = useState(true);
  const [refreshing, setRefreshing] = useState(false);
  const [error, setError] = useState("");

  const load = useCallback(async () => {
    if (!projectId) {
      setKnowledge(null);
      setLoading(false);
      setError("");
      return;
    }
    setLoading(true);
    setError("");
    try {
      const data = await getProjectKnowledge(projectId);
      setKnowledge(data);
    } catch (e) {
      if (e?.status === 404) {
        setKnowledge(null);
        setError("");
      } else {
        setKnowledge(null);
        setError(apiErrorMessage(e) || t("knowledge.error"));
      }
    } finally {
      setLoading(false);
    }
  }, [projectId, t]);

  useEffect(() => {
    load();
  }, [load]);

  const handleRefresh = async (mode = "replace", includeRepository = false) => {
    if (!projectId) return;
    setRefreshing(true);
    setError("");
    try {
      const data = await refreshProjectKnowledge(projectId, mode, { includeRepository });
      setKnowledge(data);
    } catch (e) {
      setError(apiErrorMessage(e) || t("knowledge.error"));
    } finally {
      setRefreshing(false);
    }
  };

  if (!projectId) {
    return (
      <div style={{ padding: "32px 40px", maxWidth: 960, margin: "0 auto" }}>
        <h1 className="page-title">{t("knowledge.title")}</h1>
        <p className="page-subtitle">{t("knowledge.pick_project")}</p>
      </div>
    );
  }

  return (
    <div style={{ padding: "32px 40px", maxWidth: 1100, margin: "0 auto" }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 16, marginBottom: 24, flexWrap: "wrap" }}>
        <div>
          <h1 className="page-title" style={{ margin: 0 }}>{t("knowledge.title")}</h1>
          <p className="page-subtitle" style={{ marginTop: 6 }}>{t("knowledge.subtitle")}</p>
          <p style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4 }}>
            {t("knowledge.scope", { name: currentProject?.name || projectId })}
          </p>
        </div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
          <button type="button" className="btn btn-primary btn-sm" onClick={() => handleRefresh("replace")} disabled={refreshing || loading}>
            {refreshing ? t("knowledge.refreshing") : t("knowledge.refresh")}
          </button>
          <button type="button" className="btn btn-secondary btn-sm" onClick={() => handleRefresh("replace", true)} disabled={refreshing || loading}>
            {refreshing ? t("knowledge.refreshing") : t("knowledge.refresh_repo")}
          </button>
          <button type="button" className="btn btn-secondary btn-sm" onClick={() => handleRefresh("merge")} disabled={refreshing || loading}>
            {t("knowledge.merge")}
          </button>
        </div>
      </div>

      {error ? <div className="alert alert-error" style={{ marginBottom: 16 }}>{error}</div> : null}

      {loading ? (
        <div style={{ fontSize: 13, color: "var(--text-3)", padding: 24 }}>{t("knowledge.loading")}</div>
      ) : !knowledge ? (
        <>
          <InitializeProjectPanel
            projectId={projectId}
            projectName={currentProject?.name}
            onInitialized={() => load()}
          />
          <div className="card" style={{ padding: 24 }}>
            <p style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 16 }}>{t("knowledge.empty")}</p>
            <button type="button" className="btn btn-primary" onClick={() => handleRefresh("replace")} disabled={refreshing}>
              {t("knowledge.build")}
            </button>
          </div>
        </>
      ) : (
        <>
          <KpiStrip items={knowledgeKpiItems(knowledge, t)} />
          {(() => {
            const depth = computeMemoryDepth(knowledge);
            return (
              <div className="card" style={{ padding: "12px 20px", marginBottom: 16, fontSize: 13, color: "var(--text-2)" }}>
                <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "center" }}>
                  <span>
                    {t("knowledge.sources.label")}: {formatSourcesLabel(depth.sources, t)}
                  </span>
                  <span className={`badge ${depth.repositoryIndexed ? "badge-blue" : "badge-gray"}`}>
                    {depth.repositoryIndexed ? t("knowledge.repo.indexed") : t("knowledge.repo.not_indexed")}
                  </span>
                  {depth.repositoryIndexed && depth.repositoryFilesScanned > 0 ? (
                    <span style={{ color: "var(--text-3)" }}>
                      {t("knowledge.repo.files_scanned", { count: depth.repositoryFilesScanned })}
                    </span>
                  ) : null}
                </div>
              </div>
            );
          })()}
          <div className="card" style={{ padding: "16px 20px", marginBottom: 16, display: "flex", gap: 16, flexWrap: "wrap", alignItems: "center" }}>
            <div>
              <div style={{ fontSize: 11, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("knowledge.risk")}</div>
              <div style={{ marginTop: 6 }}><span className={riskBadge(knowledge.risk_score)}>{knowledge.risk_score ?? 0}/100</span></div>
            </div>
            <div>
              <div style={{ fontSize: 11, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("knowledge.risk_level")}</div>
              <div style={{ marginTop: 6 }}>
                <span className={riskLevelBadge(knowledge.risk_level)}>
                  {t(`knowledge.risk_level.${(knowledge.risk_level || "LOW").toUpperCase()}`)}
                </span>
              </div>
            </div>
            <div>
              <div style={{ fontSize: 11, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("knowledge.updated")}</div>
              <div style={{ fontSize: 13, color: "var(--text-2)", marginTop: 6 }}>{fmtTs(knowledge.updated_at)}</div>
            </div>
          </div>

          {(knowledge.risk_explanation || []).length > 0 ? (
            <Section title={t("knowledge.explanation")} count={(knowledge.risk_explanation || []).length} empty={t("knowledge.none")}>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
                {(knowledge.risk_explanation || []).map((line, i) => (
                  <li key={`exp-${i}`}>{line}</li>
                ))}
              </ul>
            </Section>
          ) : null}

          {(knowledge.module_risks || []).filter((m) => (m.module_risk_score || 0) >= 25).length > 0 ? (
            <Section
              title={t("knowledge.risky_modules")}
              count={(knowledge.module_risks || []).filter((m) => (m.module_risk_score || 0) >= 25).length}
              empty={t("knowledge.none")}
            >
              <ol style={{ margin: 0, paddingLeft: 20, fontSize: 13, color: "var(--text-2)", lineHeight: 1.8 }}>
                {(knowledge.module_risks || [])
                  .filter((m) => (m.module_risk_score || 0) >= 25)
                  .slice(0, 8)
                  .map((m) => (
                    <li key={m.module}>
                      <strong>{m.module}</strong>
                      {" — "}
                      <span className={riskLevelBadge(m.module_risk_level)}>{m.module_risk_score ?? 0}</span>
                    </li>
                  ))}
              </ol>
            </Section>
          ) : null}

          {(knowledge.recommended_tests || []).length > 0 ? (
            <Section title={t("knowledge.recommended_tests")} count={(knowledge.recommended_tests || []).length} empty={t("knowledge.none")}>
              <div className="card" style={{ overflow: "hidden", padding: 0 }}>
                <table className="data-table">
                  <thead>
                    <tr>
                      <th>{t("knowledge.col.test")}</th>
                      <th>{t("knowledge.col.module")}</th>
                      <th>{t("knowledge.col.reason")}</th>
                    </tr>
                  </thead>
                  <tbody>
                    {(knowledge.recommended_tests || []).slice(0, 12).map((tc) => (
                      <tr key={tc.test_case_id}>
                        <td style={{ fontSize: 12 }}>{tc.name || tc.test_case_id}</td>
                        <td style={{ fontSize: 12, color: "var(--text-3)" }}>{tc.module || "—"}</td>
                        <td style={{ fontSize: 12 }}>{tc.reason || "—"}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </Section>
          ) : null}

          <Section title={t("knowledge.modules")} count={(knowledge.modules || []).length} empty={t("knowledge.none")}>
            <div style={{ display: "flex", flexWrap: "wrap", gap: 8 }}>
              {(knowledge.modules || []).map((m) => (
                <span key={m.name} className="badge badge-gray">
                  {m.name} ({m.test_count})
                  {(m.routes || []).length > 0 ? ` · ${m.routes.length} routes` : ""}
                  {(m.apis || []).length > 0 ? ` · ${m.apis.length} apis` : ""}
                </span>
              ))}
            </div>
          </Section>

          <Section title={t("knowledge.routes")} count={(knowledge.routes || []).length} empty={t("knowledge.none")}>
            <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
              {(knowledge.routes || []).slice(0, 20).map((r, i) => (
                <li key={`${r.url}-${i}`}>
                  <span style={{ fontFamily: "monospace", fontSize: 12 }}>{r.url}</span>
                  {r.module ? <span className="badge badge-gray" style={{ marginLeft: 6 }}>{r.module}</span> : null}
                  {r.title ? ` — ${r.title}` : ""}
                </li>
              ))}
            </ul>
          </Section>

          <Section title={t("knowledge.apis")} count={(knowledge.apis || []).length} empty={t("knowledge.none")}>
            <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, fontFamily: "monospace", color: "var(--text-2)", lineHeight: 1.7 }}>
              {(knowledge.apis || []).slice(0, 15).map((a, i) => (
                <li key={`${a.method}-${a.url}-${i}`}>
                  {a.method} {a.url}
                  {a.module ? <span className="badge badge-gray" style={{ marginLeft: 6, fontFamily: "inherit" }}>{a.module}</span> : null}
                </li>
              ))}
            </ul>
          </Section>

          <Section title={t("knowledge.tests")} count={(knowledge.related_tests || []).length} empty={t("knowledge.none")}>
            <div className="card" style={{ overflow: "hidden", padding: 0 }}>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("knowledge.col.test")}</th>
                    <th>{t("knowledge.col.module")}</th>
                    <th>{t("knowledge.col.status")}</th>
                  </tr>
                </thead>
                <tbody>
                  {(knowledge.related_tests || []).slice(0, 15).map((tc) => (
                    <tr key={tc.test_case_id}>
                      <td style={{ fontSize: 12 }}>{tc.name || tc.test_case_id}</td>
                      <td style={{ fontSize: 12, color: "var(--text-3)" }}>{tc.module || "—"}</td>
                      <td style={{ fontSize: 12 }}>{tc.last_run_status || "—"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </Section>

          <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
            <Section title={t("knowledge.incidents")} count={(knowledge.incident_history || []).length} empty={t("knowledge.none")}>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
                {(knowledge.incident_history || []).slice(0, 8).map((inc) => (
                  <li key={inc.id}>{inc.description?.slice(0, 100) || inc.id} <span className="badge badge-gray" style={{ marginLeft: 6 }}>{inc.severity}</span></li>
                ))}
              </ul>
            </Section>

            <Section title={t("knowledge.regressions")} count={(knowledge.failure_history || []).length} empty={t("knowledge.none")}>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
                {(knowledge.failure_history || []).slice(0, 8).map((f, i) => (
                  <li key={`${f.test_case_id}-${i}`}>{f.test_name || f.test_case_id} ({f.module}) — {f.count}×</li>
                ))}
              </ul>
            </Section>
          </div>
        </>
      )}
    </div>
  );
}
