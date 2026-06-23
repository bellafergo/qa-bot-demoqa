import React, { useState, useEffect, useCallback } from "react";
import { Link } from "react-router-dom";
import { getRunsAnalytics, listTestRuns } from "../../api";
import { useLang } from "../../i18n/LangContext";
import { useProject } from "../../context/ProjectContext.jsx";
import KpiStrip from "../KpiStrip.jsx";
import InitializeProjectPanel from "../InitializeProjectPanel.jsx";
import { ErrorState, EmptyStateLinkAction } from "../../ui/EmptyState.jsx";
import { fmtRunsKpiDate, fmtRunsKpiMs } from "../../utils/runHelpers.js";

export default function RunsAnalyticsKpi({ onInitialized }) {
  const { t } = useLang();
  const { currentProject } = useProject();
  const projectId = currentProject?.id;
  const [analytics, setAnalytics] = useState(null);
  const [lastRunAt, setLastRunAt] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const load = useCallback(() => {
    setLoading(true);
    setError("");
    const runOpts = { limit: 1 };
    if (projectId) runOpts.project_id = projectId;
    Promise.all([
      getRunsAnalytics(projectId),
      listTestRuns(runOpts).catch(() => []),
    ])
      .then(([a, runs]) => {
        setAnalytics(a);
        const first = Array.isArray(runs) ? runs[0] : runs?.runs?.[0];
        setLastRunAt(first?.started_at || first?.finished_at || null);
      })
      .catch((e) => {
        setAnalytics(null);
        setLastRunAt(null);
        setError(e?.message || t("runs.kpi.load_error"));
      })
      .finally(() => setLoading(false));
  }, [projectId, t]);

  useEffect(() => {
    load();
  }, [load]);

  const s = analytics?.summary;
  const total = s?.total_runs ?? 0;
  const passRate = s?.pass_rate != null ? `${Number(s.pass_rate).toFixed(1)}%` : "—";
  const failRate =
    s?.total_runs > 0 && s?.failed_runs != null
      ? `${((s.failed_runs / s.total_runs) * 100).toFixed(1)}%`
      : "—";

  if (!loading && error) {
    return (
      <div style={{ marginBottom: 20 }}>
        <ErrorState
          title={t("runs.kpi.load_error")}
          description={error}
          onRetry={load}
          retryLabel={t("common.retry")}
          className="card"
        />
      </div>
    );
  }

  if (!loading && !error && total === 0) {
    return (
      <div style={{ marginBottom: 20 }}>
        {projectId ? (
          <InitializeProjectPanel
            projectId={projectId}
            projectName={currentProject?.name}
            compact
            onInitialized={() => {
              onInitialized?.();
              load();
            }}
          />
        ) : (
          <div className="card" style={{ padding: 16, fontSize: 13, color: "var(--text-2)" }}>
            {t("runs.kpi.empty_cta")}
          </div>
        )}
        <div style={{ marginTop: 10, display: "flex", gap: 8, flexWrap: "wrap" }}>
          {projectId ? (
            <Link to="/dashboard" className="btn btn-secondary btn-sm">{t("runs.kpi.init_cta")}</Link>
          ) : null}
          <EmptyStateLinkAction to="/batch" label={t("runs.kpi.batch_cta")} />
        </div>
      </div>
    );
  }

  return (
    <KpiStrip
      loading={loading}
      loadingLabel={t("common.loading")}
      items={[
        { key: "total", label: t("runs.kpi.total"), value: total },
        { key: "pass", label: t("runs.kpi.pass_rate"), value: passRate, accent: "var(--green)" },
        { key: "fail", label: t("runs.kpi.fail_rate"), value: failRate, accent: "var(--red)" },
        { key: "dur", label: t("runs.kpi.avg_duration"), value: fmtRunsKpiMs(s?.avg_duration_ms) },
        { key: "last", label: t("runs.kpi.last_run"), value: fmtRunsKpiDate(lastRunAt) },
      ]}
    />
  );
}
