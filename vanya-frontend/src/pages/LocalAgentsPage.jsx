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
  registerFoundationLocalAgent,
  registerDatabaseConnection,
  getProjectOnboarding,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import { useToast } from "../context/ToastContext.jsx";
import ConfirmDialog from "../components/ConfirmDialog.jsx";
import {
  buildLocalAgentsConsoleViewModel,
  buildAgentDetailViewModel,
  buildDatabaseConnectionCardViewModel,
  FOUNDATION_AGENT_DEFAULTS,
  LOCAL_AGENTS_I18N_KEYS,
  LOCAL_AGENTS_ENTERPRISE_I18N_KEYS,
} from "../utils/localAgentsViewUtils.js";
import LocalAgentsSummaryHeader from "../components/local-agents/LocalAgentsSummaryHeader.jsx";
import LocalAgentsList from "../components/local-agents/LocalAgentsList.jsx";
import LocalAgentDetailPanel from "../components/local-agents/LocalAgentDetailPanel.jsx";
import LocalAgentsDatabaseConnections from "../components/local-agents/LocalAgentsDatabaseConnections.jsx";
import LocalAgentsCapabilitiesOverview from "../components/local-agents/LocalAgentsCapabilitiesOverview.jsx";
import {
  buildDatabaseConnectionsViewModel,
  buildDatabaseExecutionsViewModel,
  DATABASE_CONNECTOR_I18N_KEYS,
  resolveTargetAgentId,
  SAMPLE_DATABASE_CONNECTION_DEFAULTS,
} from "../utils/databaseConnectorViewUtils.js";
import { buildInternalApiConnectorViewModel } from "../utils/internalApiConnectorViewUtils.js";
import InternalApiConnectorView from "../components/local-agents/InternalApiConnectorView.jsx";
import { buildEnterpriseSystemViewModel } from "../utils/enterpriseSystemViewUtils.js";
import EnterpriseSystemView from "../components/local-agents/EnterpriseSystemView.jsx";

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

export default function LocalAgentsPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const { showToast } = useToast();
  const [agents, setAgents] = useState([]);
  const [foundationReport, setFoundationReport] = useState(null);
  const [dbConnections, setDbConnections] = useState([]);
  const [dbExecutions, setDbExecutions] = useState([]);
  const [internalApiReport, setInternalApiReport] = useState(null);
  const [enterpriseSystemReport, setEnterpriseSystemReport] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [selectedId, setSelectedId] = useState(null);
  const [detail, setDetail] = useState(null);
  const [detailLoading, setDetailLoading] = useState(false);
  const [busyId, setBusyId] = useState(null);
  const [creatingFoundation, setCreatingFoundation] = useState(false);
  const [creatingSampleConnection, setCreatingSampleConnection] = useState(false);
  const [confirmDisable, setConfirmDisable] = useState(null);

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
        const agentIds = new Set((Array.isArray(data) ? data : []).map((a) => a.agent_id));
        const scopedConnections = (Array.isArray(connections) ? connections : []).filter((c) =>
          agentIds.has(c.agent_id),
        );
        setDbConnections(scopedConnections);
        setDbExecutions(Array.isArray(executions) ? executions : []);
        setInternalApiReport(report?.internal_api_connectors || null);
        setEnterpriseSystemReport(report?.enterprise_systems || null);
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

  const onCreateFoundationAgent = async () => {
    const pid = currentProject?.id ? String(currentProject.id).trim() : "";
    if (!pid) {
      showToast(t(LOCAL_AGENTS_I18N_KEYS.toastNoProject), "error");
      return;
    }
    setCreatingFoundation(true);
    try {
      const result = await registerFoundationLocalAgent({
        project_id: pid,
        name: FOUNDATION_AGENT_DEFAULTS.name,
        environment: FOUNDATION_AGENT_DEFAULTS.environment,
        version: FOUNDATION_AGENT_DEFAULTS.version,
        capabilities: FOUNDATION_AGENT_DEFAULTS.capabilities,
      });
      const toastKey = result?.already_exists
        ? LOCAL_AGENTS_I18N_KEYS.toastFoundationExists
        : LOCAL_AGENTS_I18N_KEYS.toastFoundationCreated;
      showToast(t(toastKey), result?.already_exists ? "warning" : "success");
      await loadAgents({ silent: true });
      if (result?.agent_id) setSelectedId(result.agent_id);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setCreatingFoundation(false);
    }
  };

  const onCreateSampleConnection = async () => {
    const agentId = resolveTargetAgentId(agents, selectedId);
    if (!agentId) {
      showToast(t(DATABASE_CONNECTOR_I18N_KEYS.toastNoAgent), "error");
      return;
    }
    setCreatingSampleConnection(true);
    try {
      const result = await registerDatabaseConnection({
        agent_id: agentId,
        ...SAMPLE_DATABASE_CONNECTION_DEFAULTS,
      });
      const toastKey = result?.already_exists
        ? DATABASE_CONNECTOR_I18N_KEYS.toastConnectionExists
        : DATABASE_CONNECTOR_I18N_KEYS.toastConnectionCreated;
      showToast(t(toastKey), result?.already_exists ? "warning" : "success");
      const pid = currentProject?.id ? String(currentProject.id).trim() : "";
      await Promise.all([
        loadAgents({ silent: true }),
        pid ? getProjectOnboarding(pid).catch(() => null) : Promise.resolve(),
      ]);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setCreatingSampleConnection(false);
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

  const consoleVm = useMemo(
    () =>
      buildLocalAgentsConsoleViewModel({
        agents,
        dbConnections,
        foundationReport,
        t,
        formatTimestamp: fmtTs,
      }),
    [agents, dbConnections, foundationReport, t],
  );

  const agentNameById = useMemo(
    () => new Map(agents.map((a) => [a.agent_id, a.name || a.agent_id])),
    [agents],
  );

  const dbConnectionCards = useMemo(
    () => dbConnections.map((c) => buildDatabaseConnectionCardViewModel(c, agentNameById, t, fmtTs)),
    [dbConnections, agentNameById, t],
  );

  const selectedFoundation = useMemo(
    () => consoleVm.foundationById.get(selectedId) || null,
    [consoleVm.foundationById, selectedId],
  );

  const detailVm = useMemo(() => {
    if (!detail) return null;
    const systemsCount = (enterpriseSystemReport?.connectors || []).length;
    return buildAgentDetailViewModel({
      detail,
      foundationRow: selectedFoundation,
      dbConnectionCount: dbConnections.filter((c) => c.agent_id === detail.agent_id).length,
      validationCount: dbExecutions.length,
      systemsCount,
      t,
      formatTimestamp: fmtTs,
    });
  }, [detail, selectedFoundation, dbConnections, dbExecutions, enterpriseSystemReport, t]);

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
  const enterpriseSystemVm = useMemo(
    () => buildEnterpriseSystemViewModel(enterpriseSystemReport || { connectors: [], modules: [], validations: [] }, t),
    [enterpriseSystemReport, t],
  );

  const targetAgentId = resolveTargetAgentId(agents, selectedId);

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
            <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>
              {t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.commandCenterTitle)}
            </h1>
            <span className="badge badge-orange">{t("localAgents.beta_badge")}</span>
          </div>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "6px 0 0" }}>{t("localAgents.subtitle")}</p>
          <p style={{ fontSize: 12, color: "var(--text-2)", margin: "8px 0 0" }}>
            {currentProject ? t("localAgents.scope_project", { name: currentProject.name }) : t("localAgents.scope_all")}
          </p>
        </div>
        <button type="button" className="btn btn-secondary btn-sm" onClick={() => loadAgents({ silent: false })} disabled={loading}>
          {t("localAgents.refresh")}
        </button>
      </div>

      {!loading && !error && agents.length > 0 ? (
        <LocalAgentsSummaryHeader metrics={consoleVm.summary} labels={consoleVm.summaryLabels} />
      ) : null}

      <div className="local-agents-layout" style={{ marginTop: 20 }}>
        <div className="local-agents-layout__list">
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
              <button
                type="button"
                className="btn btn-primary btn-sm"
                style={{ marginTop: 18 }}
                disabled={creatingFoundation || !currentProject?.id}
                onClick={onCreateFoundationAgent}
              >
                {creatingFoundation ? t("localAgents.loading") : t(LOCAL_AGENTS_I18N_KEYS.createFoundationAgent)}
              </button>
              {!currentProject?.id ? (
                <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 10 }}>
                  {t(LOCAL_AGENTS_I18N_KEYS.toastNoProject)}
                </div>
              ) : null}
            </div>
          )}
          {!loading && !error && agents.length > 0 && (
            <LocalAgentsList
              agents={consoleVm.agents}
              selectedId={selectedId}
              labels={{
                title: consoleVm.agentsListTitle,
                created: consoleVm.createdLabel,
                lastSeen: consoleVm.lastSeenLabel,
              }}
              onSelect={setSelectedId}
            />
          )}
        </div>

        <LocalAgentDetailPanel
          vm={detailVm}
          loading={Boolean(selectedId) && detailLoading}
          busy={busyId === selectedId}
          t={t}
          selectLabel={t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailSelect)}
          onRefresh={() => selectedId && onRefreshRow(selectedId)}
          onDisable={() => selectedId && setConfirmDisable(selectedId)}
        />
      </div>

      <LocalAgentsDatabaseConnections
        title={t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.dbSectionTitle)}
        emptyTitle={t("localAgents.database.connections_empty_title")}
        emptyDesc={t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.dbEmptyDesc)}
        createLabel={connectionsVm.createSampleConnectionLabel}
        connections={dbConnectionCards}
        empty={connectionsVm.empty}
        creating={creatingSampleConnection}
        canCreate={Boolean(targetAgentId)}
        noAgentHint={t(DATABASE_CONNECTOR_I18N_KEYS.toastNoAgent)}
        onCreate={onCreateSampleConnection}
      />

      {!loading && !error && agents.length > 0 ? (
        <LocalAgentsCapabilitiesOverview title={consoleVm.capabilitiesOverviewTitle} items={consoleVm.capabilitySummary} />
      ) : null}

      <div style={{ marginTop: 28 }}>
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

      <div style={{ marginTop: 28 }}>
        <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 10 }}>
          {enterpriseSystemVm.title}
        </div>
        <EnterpriseSystemView vm={enterpriseSystemVm} />
        <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
          {enterpriseSystemVm.readOnlyNote}
        </p>
      </div>
    </div>
  );
}
