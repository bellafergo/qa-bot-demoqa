// src/pages/LocalAgentsPage.jsx
/** Phase 4E — Local agents admin UI (list, health heuristic, detail, disable). */
import React, { useCallback, useEffect, useMemo, useState } from "react";
import {
  listLocalAgents,
  getLocalAgent,
  disableLocalAgent,
  getLocalAgentFoundationReport,
  listDatabaseConnections,
  listDatabaseValidationExecutions,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import { useToast } from "../context/ToastContext.jsx";
import ConfirmDialog from "../components/ConfirmDialog.jsx";
import { buildLocalAgentsViewModel } from "../utils/localAgentsViewUtils.js";
import {
  buildDatabaseConnectionsViewModel,
  buildDatabaseExecutionsViewModel,
  DATABASE_CONNECTOR_I18N_KEYS,
} from "../utils/databaseConnectorViewUtils.js";
import { buildInternalApiConnectorViewModel } from "../utils/internalApiConnectorViewUtils.js";
import InternalApiConnectorView from "../components/local-agents/InternalApiConnectorView.jsx";

function fmtTs(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

/** MVP: online <2m, stale <10m, else offline; disabled from DB flag / status. */
function agentHealthKey(agent) {
  if (!agent?.enabled || String(agent.status || "").toLowerCase() === "disabled") return "disabled";
  const iso = agent.last_seen_at;
  if (!iso) return "offline";
  const ms = Date.now() - new Date(iso).getTime();
  if (!Number.isFinite(ms)) return "offline";
  const two = 2 * 60 * 1000;
  const ten = 10 * 60 * 1000;
  if (ms < two) return "online";
  if (ms < ten) return "stale";
  return "offline";
}

function healthBadgeClass(key) {
  if (key === "online") return "badge-green";
  if (key === "stale") return "badge-orange";
  if (key === "disabled") return "badge-gray";
  return "badge-gray";
}

function shortFp(fp) {
  const s = String(fp || "").trim();
  if (!s) return "—";
  return s.length <= 10 ? s : `${s.slice(0, 8)}…`;
}

export default function LocalAgentsPage() {
  const { t } = useLang();
  const { currentProject, projects } = useProject();
  const { showToast } = useToast();
  const [agents, setAgents] = useState([]);
  const [foundationReport, setFoundationReport] = useState(null);
  const [dbConnections, setDbConnections] = useState([]);
  const [dbExecutions, setDbExecutions] = useState([]);
  const [internalApiReport, setInternalApiReport] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [selectedId, setSelectedId] = useState(null);
  const [detail, setDetail] = useState(null);
  const [detailLoading, setDetailLoading] = useState(false);
  const [busyId, setBusyId] = useState(null);
  const [confirmDisable, setConfirmDisable] = useState(null);

  const projectName = useCallback(
    (pid) => {
      if (!pid) return "—";
      const hit = projects.find((p) => p.id === pid);
      return hit?.name ? `${hit.name} (${pid})` : pid;
    },
    [projects],
  );

  const loadAgents = useCallback(
    async (opts = {}) => {
      const silent = Boolean(opts.silent);
      if (!silent) {
        setLoading(true);
        setError(null);
      }
      try {
        const pid = currentProject?.id ? String(currentProject.id).trim() : "";
        const [data, report, connections, executions] = await Promise.all([
          listLocalAgents({ project_id: pid || undefined, limit: 200 }),
          getLocalAgentFoundationReport({ project_id: pid || undefined, limit: 200 }).catch(() => null),
          listDatabaseConnections({ limit: 200 }).catch(() => []),
          listDatabaseValidationExecutions({ limit: 100 }).catch(() => []),
        ]);
        setAgents(Array.isArray(data) ? data : []);
        setFoundationReport(report && typeof report === "object" ? report : null);
        setDbConnections(Array.isArray(connections) ? connections : []);
        setDbExecutions(Array.isArray(executions) ? executions : []);
        setInternalApiReport(report?.internal_api_connectors || null);
      } catch (e) {
        const msg = apiErrorMessage(e);
        if (!silent) {
          setError(msg);
          setAgents([]);
        } else {
          showToast(msg, "error");
        }
      } finally {
        if (!silent) setLoading(false);
      }
    },
    [currentProject?.id, showToast],
  );

  useEffect(() => {
    loadAgents({ silent: false });
  }, [loadAgents]);

  useEffect(() => {
    if (!selectedId) {
      setDetail(null);
      return;
    }
    let cancelled = false;
    (async () => {
      setDetailLoading(true);
      try {
        const d = await getLocalAgent(selectedId);
        if (!cancelled) setDetail(d);
      } catch (e) {
        if (!cancelled) {
          setDetail(null);
          showToast(apiErrorMessage(e), "error");
        }
      } finally {
        if (!cancelled) setDetailLoading(false);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [selectedId, showToast]);

  const onRefreshRow = async (agentId) => {
    setBusyId(agentId);
    try {
      const d = await getLocalAgent(agentId);
      setAgents((prev) =>
        prev.map((a) => {
          if (a.agent_id !== agentId) return a;
          const { recent_jobs: _rj, ...rest } = d;
          return { ...a, ...rest };
        }),
      );
      if (selectedId === agentId) setDetail(d);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setBusyId(null);
    }
  };

  const onDisable = async () => {
    const id = confirmDisable;
    if (!id) return;
    setBusyId(id);
    try {
      await disableLocalAgent(id);
      showToast(t("localAgents.toast.disabled"), "success");
      setConfirmDisable(null);
      if (selectedId === id) setSelectedId(null);
      await loadAgents({ silent: true });
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setBusyId(null);
    }
  };

  const selectedSummary = useMemo(() => agents.find((a) => a.agent_id === selectedId) || null, [agents, selectedId]);
  const foundationVm = useMemo(
    () => buildLocalAgentsViewModel(foundationReport || { agents: [], inventory: [], summary: "" }, t, fmtTs),
    [foundationReport, t],
  );
  const selectedFoundation = useMemo(
    () => foundationVm.agents.find((a) => a.agent_id === selectedId) || null,
    [foundationVm.agents, selectedId],
  );
  const connectionsVm = useMemo(
    () => buildDatabaseConnectionsViewModel(dbConnections, t, fmtTs),
    [dbConnections, t],
  );
  const executionsVm = useMemo(
    () => buildDatabaseExecutionsViewModel(dbExecutions, t, fmtTs),
    [dbExecutions, t],
  );
  const internalApiVm = useMemo(
    () => buildInternalApiConnectorViewModel(internalApiReport || { connectors: [], endpoints: [], validations: [] }, t),
    [internalApiReport, t],
  );

  return (
    <div style={{ padding: "24px 24px 40px", maxWidth: 1400, margin: "0 auto" }}>
      <ConfirmDialog
        open={Boolean(confirmDisable)}
        title={t("localAgents.action.disable")}
        description={t("localAgents.disable_confirm")}
        danger
        confirmLabel={t("localAgents.action.disable")}
        busy={busyId === confirmDisable}
        onCancel={() => !busyId && setConfirmDisable(null)}
        onConfirm={onDisable}
      />

      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "flex-start",
          gap: 16,
          flexWrap: "wrap",
        }}
      >
        <div>
          <div style={{ display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
            <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>{t("localAgents.title")}</h1>
            <span className="badge badge-orange">{t("localAgents.beta_badge")}</span>
          </div>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "6px 0 0" }}>{t("localAgents.subtitle")}</p>
          <p style={{ fontSize: 12, color: "var(--text-2)", margin: "8px 0 0", maxWidth: 560, lineHeight: 1.5 }}>
            {t("localAgents.beta_desc")}
          </p>
          {foundationVm.summary ? (
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: "8px 0 0", maxWidth: 560, lineHeight: 1.5 }}>
              <strong>{foundationVm.summaryLabel}:</strong> {foundationVm.summary}
            </p>
          ) : null}
          <p style={{ fontSize: 11, color: "var(--text-3)", margin: "8px 0 0", maxWidth: 560, lineHeight: 1.5, fontStyle: "italic" }}>
            {foundationVm.readOnlyNote}
          </p>
          <p style={{ fontSize: 12, color: "var(--text-2)", margin: "8px 0 0" }}>
            {currentProject ? t("localAgents.scope_project", { name: currentProject.name }) : t("localAgents.scope_all")}
          </p>
        </div>
        <button type="button" className="btn btn-secondary btn-sm" onClick={() => loadAgents({ silent: false })} disabled={loading}>
          {t("localAgents.refresh")}
        </button>
      </div>

      <div style={{ display: "flex", gap: 20, marginTop: 20, flexWrap: "wrap", alignItems: "stretch" }}>
        <div style={{ flex: "1 1 560px", minWidth: 300 }}>
          {loading && <div style={{ padding: 20, color: "var(--text-3)", fontSize: 13 }}>{t("localAgents.loading")}</div>}
          {!loading && error && (
            <div className="alert alert-error" style={{ marginTop: 8 }}>
              {t("localAgents.error")} {error}
            </div>
          )}
          {!loading && !error && agents.length === 0 && (
            <div className="card" style={{ padding: 28, textAlign: "center" }}>
              <div style={{ fontSize: 32, marginBottom: 12, opacity: 0.35 }}>⎔</div>
              <div style={{ fontSize: 15, fontWeight: 600 }}>{t("localAgents.empty_title")}</div>
              <div style={{ fontSize: 13, color: "var(--text-2)", marginTop: 8, maxWidth: 420, margin: "8px auto 0", lineHeight: 1.55 }}>
                {t("localAgents.empty_desc")}
              </div>
              <button type="button" className="btn btn-primary btn-sm" style={{ marginTop: 18 }} disabled title={t("localAgents.register_hint")}>
                {t("localAgents.register_cta")}
              </button>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 10 }}>{t("localAgents.register_hint")}</div>
            </div>
          )}
          {!loading && !error && agents.length > 0 && (
            <div style={{ overflowX: "auto", border: "1px solid var(--border)", borderRadius: 8 }}>
              <table className="data-table" style={{ margin: 0, minWidth: 960 }}>
                <thead>
                  <tr>
                    <th>{t("localAgents.col.name")}</th>
                    <th>{t("localAgents.col.health")}</th>
                    <th>{t("localAgents.foundation.environment")}</th>
                    <th>{t("localAgents.col.status")}</th>
                    <th>{t("localAgents.col.project")}</th>
                    <th>{t("localAgents.foundation.capabilities")}</th>
                    <th>{t("localAgents.col.version")}</th>
                    <th>{t("localAgents.col.last_seen")}</th>
                    <th>{t("localAgents.col.enabled")}</th>
                    <th>{t("localAgents.col.fingerprint")}</th>
                    <th>{t("localAgents.col.actions")}</th>
                  </tr>
                </thead>
                <tbody>
                  {agents.map((a) => {
                    const hk = agentHealthKey(a);
                    const foundationRow = foundationVm.agents.find((row) => row.agent_id === a.agent_id);
                    return (
                      <tr
                        key={a.agent_id}
                        onClick={() => setSelectedId(a.agent_id)}
                        style={{
                          cursor: "pointer",
                          background: selectedId === a.agent_id ? "var(--accent-light)" : undefined,
                        }}
                      >
                        <td style={{ fontSize: 12, fontWeight: 600 }}>{a.name || a.agent_id}</td>
                        <td>
                          <span className={`badge ${healthBadgeClass(hk)}`} style={{ fontSize: 10 }}>
                            {t(`localAgents.health.${hk}`)}
                          </span>
                        </td>
                        <td style={{ fontSize: 11 }}>
                          {foundationRow?.environment || a.metadata?.environment || "—"}
                        </td>
                        <td style={{ fontSize: 11 }}>
                          {foundationRow ? (
                            <span className={`badge ${foundationRow.statusBadgeClass}`} style={{ fontSize: 10 }}>
                              {foundationRow.statusLabel}
                            </span>
                          ) : (
                            a.status || "—"
                          )}
                        </td>
                        <td style={{ fontSize: 11, maxWidth: 160, wordBreak: "break-word" }}>{projectName(a.project_id)}</td>
                        <td style={{ fontSize: 11, maxWidth: 220 }}>
                          {(foundationRow?.capabilityLabels || a.capabilities || []).join(", ") || "—"}
                        </td>
                        <td style={{ fontSize: 11 }}>{a.version || "—"}</td>
                        <td style={{ fontSize: 11, whiteSpace: "nowrap" }}>{fmtTs(a.last_seen_at)}</td>
                        <td style={{ fontSize: 11 }}>{a.enabled ? "✓" : "—"}</td>
                        <td style={{ fontFamily: "monospace", fontSize: 10 }}>{shortFp(a.token_fingerprint)}</td>
                        <td onClick={(e) => e.stopPropagation()}>
                          <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
                            <button
                              type="button"
                              className="btn btn-secondary btn-sm"
                              disabled={busyId === a.agent_id}
                              onClick={() => onRefreshRow(a.agent_id)}
                            >
                              {t("localAgents.refresh")}
                            </button>
                            <button
                              type="button"
                              className="btn btn-primary btn-sm"
                              style={{ background: "var(--red)", borderColor: "var(--red)" }}
                              disabled={busyId === a.agent_id || !a.enabled}
                              onClick={() => setConfirmDisable(a.agent_id)}
                            >
                              {t("localAgents.action.disable")}
                            </button>
                          </div>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          )}
        </div>

        <div style={{ flex: "1 1 320px", minWidth: 280 }}>
          {!selectedId && (
            <div
              style={{
                border: "1px dashed var(--border)",
                borderRadius: 8,
                padding: 20,
                color: "var(--text-3)",
                fontSize: 13,
              }}
            >
              {t("localAgents.detail.select")}
            </div>
          )}
          {selectedId && (
            <div className="card" style={{ padding: 16 }}>
              {detailLoading && <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("localAgents.loading")}</div>}
              {!detailLoading && detail && (
                <>
                  <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 8 }}>{detail.name}</div>
                  <div style={{ fontSize: 11, color: "var(--text-3)", wordBreak: "break-all", marginBottom: 12 }}>{detail.agent_id}</div>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>{t("localAgents.col.health")}</div>
                  <div style={{ marginBottom: 12 }}>
                    <span className={`badge ${healthBadgeClass(agentHealthKey(detail))}`} style={{ fontSize: 10 }}>
                      {t(`localAgents.health.${agentHealthKey(detail)}`)}
                    </span>
                  </div>
                  {selectedFoundation ? (
                    <>
                      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                        {foundationVm.environmentLabel}
                      </div>
                      <div style={{ marginBottom: 10, fontSize: 12, color: "var(--text-2)" }}>
                        {selectedFoundation.environment} · {foundationVm.versionLabel}: {selectedFoundation.version}
                      </div>
                      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                        {foundationVm.lastHeartbeatLabel}
                      </div>
                      <div style={{ marginBottom: 10, fontSize: 12, color: "var(--text-2)" }}>
                        {selectedFoundation.lastHeartbeatText}
                      </div>
                      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                        {foundationVm.capabilitiesLabel}
                      </div>
                      <ul style={{ margin: "0 0 12px", paddingLeft: 18, fontSize: 12, color: "var(--text-2)" }}>
                        {selectedFoundation.capabilityLabels.map((label) => (
                          <li key={label}>{label}</li>
                        ))}
                      </ul>
                      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                        {foundationVm.inventoryLabel}
                      </div>
                      {selectedFoundation.hasInventory ? (
                        <ul style={{ margin: "0 0 12px", paddingLeft: 18, fontSize: 12, color: "var(--text-2)" }}>
                          {selectedFoundation.inventoryLines.databases.map((item) => (
                            <li key={`db-${item}`}>
                              {foundationVm.databasesLabel}: {item}
                            </li>
                          ))}
                          {selectedFoundation.inventoryLines.repositories.map((item) => (
                            <li key={`repo-${item}`}>
                              {foundationVm.repositoriesLabel}: {item}
                            </li>
                          ))}
                          {selectedFoundation.inventoryLines.services.map((item) => (
                            <li key={`svc-${item}`}>
                              {foundationVm.servicesLabel}: {item}
                            </li>
                          ))}
                        </ul>
                      ) : (
                        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12 }}>—</div>
                      )}
                    </>
                  ) : null}
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>{t("localAgents.detail.meta")}</div>
                  <pre
                    style={{
                      fontSize: 11,
                      margin: "0 0 12px",
                      padding: 8,
                      background: "var(--bg-subtle, rgba(0,0,0,0.04))",
                      borderRadius: 6,
                      maxHeight: 140,
                      overflow: "auto",
                      whiteSpace: "pre-wrap",
                      wordBreak: "break-word",
                    }}
                  >
                    {JSON.stringify(detail.metadata && typeof detail.metadata === "object" ? detail.metadata : {}, null, 2)}
                  </pre>
                  <dl style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: "6px 10px", margin: 0, fontSize: 12 }}>
                    <dt style={{ color: "var(--text-3)" }}>capabilities</dt>
                    <dd style={{ margin: 0 }}>{(detail.capabilities || []).join(", ") || "—"}</dd>
                    <dt style={{ color: "var(--text-3)" }}>created_at</dt>
                    <dd style={{ margin: 0 }}>{fmtTs(detail.created_at)}</dd>
                    <dt style={{ color: "var(--text-3)" }}>last_seen</dt>
                    <dd style={{ margin: 0 }}>{fmtTs(detail.last_seen_at)}</dd>
                    <dt style={{ color: "var(--text-3)" }}>project</dt>
                    <dd style={{ margin: 0, wordBreak: "break-all" }}>{projectName(detail.project_id)}</dd>
                  </dl>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", margin: "14px 0 6px" }}>{t("localAgents.detail.jobs")}</div>
                  {!detail.recent_jobs?.length ? (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>{t("localAgents.detail.no_jobs")}</div>
                  ) : (
                    <ul style={{ listStyle: "none", margin: 0, padding: 0, maxHeight: 220, overflowY: "auto" }}>
                      {detail.recent_jobs.map((j) => (
                        <li key={j.job_id} style={{ borderBottom: "1px solid var(--border)", padding: "6px 0", fontSize: 11 }}>
                          <span className="badge badge-gray" style={{ fontSize: 9 }}>
                            {j.status}
                          </span>{" "}
                          <span style={{ color: "var(--text-2)" }}>{j.target_url?.slice(0, 80) || j.job_id}</span>
                          <div style={{ color: "var(--text-3)", marginTop: 2 }}>{fmtTs(j.created_at)}</div>
                        </li>
                      ))}
                    </ul>
                  )}
                </>
              )}
              {!detailLoading && !detail && selectedSummary && (
                <div style={{ fontSize: 13, color: "var(--text-2)" }}>{selectedSummary.name}</div>
              )}
            </div>
          )}
        </div>
      </div>

      <div style={{ marginTop: 28 }}>
        <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 10 }}>
          {connectionsVm.title}
        </div>
        {connectionsVm.empty ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", fontStyle: "italic" }}>—</div>
        ) : (
          <div style={{ overflowX: "auto", border: "1px solid var(--border)", borderRadius: 8, marginBottom: 20 }}>
            <table className="data-table" style={{ margin: 0, minWidth: 720 }}>
              <thead>
                <tr>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.name)}</th>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.type)}</th>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.agent)}</th>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.status)}</th>
                </tr>
              </thead>
              <tbody>
                {connectionsVm.connections.map((conn) => (
                  <tr key={conn.connection_id}>
                    <td style={{ fontSize: 12 }}>{conn.name}</td>
                    <td style={{ fontSize: 12 }}>{conn.database_type}</td>
                    <td style={{ fontSize: 11, wordBreak: "break-all" }}>{conn.agent_id}</td>
                    <td>
                      <span className={`badge ${conn.statusBadgeClass}`} style={{ fontSize: 10 }}>
                        {conn.statusLabel}
                      </span>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}

        <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 10 }}>
          {executionsVm.title}
        </div>
        {executionsVm.empty ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", fontStyle: "italic" }}>—</div>
        ) : (
          <div style={{ overflowX: "auto", border: "1px solid var(--border)", borderRadius: 8 }}>
            <table className="data-table" style={{ margin: 0, minWidth: 820 }}>
              <thead>
                <tr>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.check)}</th>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.status)}</th>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.timestamp)}</th>
                  <th>{t(DATABASE_CONNECTOR_I18N_KEYS.resultSummary)}</th>
                </tr>
              </thead>
              <tbody>
                {executionsVm.executions.map((item) => (
                  <tr key={item.execution_id}>
                    <td style={{ fontSize: 12 }}>{item.check_id}</td>
                    <td>
                      <span className={`badge ${item.statusBadgeClass}`} style={{ fontSize: 10 }}>
                        {item.statusLabel}
                      </span>
                    </td>
                    <td style={{ fontSize: 11, whiteSpace: "nowrap" }}>{item.executedAtText}</td>
                    <td style={{ fontSize: 12, maxWidth: 360 }}>{item.summary}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>

      <div style={{ marginTop: 28 }}>
        <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 10 }}>
          {internalApiVm.title}
        </div>
        <InternalApiConnectorView vm={internalApiVm} />
        <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
          {internalApiVm.readOnlyNote}
        </p>
      </div>
    </div>
  );
}
