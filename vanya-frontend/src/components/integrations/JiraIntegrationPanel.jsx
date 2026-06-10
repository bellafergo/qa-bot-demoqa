import React, { useCallback, useEffect, useState } from "react";
import {
  getJiraStatus,
  listJiraProjects,
  listJiraIssues,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import JiraConnectionCard from "./JiraConnectionCard.jsx";
import JiraProjectCard from "./JiraProjectCard.jsx";
import JiraIssueCard from "./JiraIssueCard.jsx";
import { buildJiraIntegrationViewModel } from "../../utils/jiraViewUtils.js";

export default function JiraIntegrationPanel({ refreshToken = 0 }) {
  const { t } = useLang();
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [status, setStatus] = useState(null);
  const [projects, setProjects] = useState([]);
  const [issues, setIssues] = useState([]);

  const load = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const [st, pr, is] = await Promise.all([
        getJiraStatus(),
        listJiraProjects(),
        listJiraIssues(),
      ]);
      setStatus(st);
      setProjects(Array.isArray(pr?.projects) ? pr.projects : []);
      setIssues(Array.isArray(is?.issues) ? is.issues : []);
    } catch (e) {
      setError(e.message || t("integrations.jira.load_error"));
      setStatus(null);
      setProjects([]);
      setIssues([]);
    } finally {
      setLoading(false);
    }
  }, [t]);

  useEffect(() => {
    load();
  }, [load, refreshToken]);

  const vm = buildJiraIntegrationViewModel({ status, projects, issues, t });

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 10 }}>
        <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)" }}>
          {t("integrations.jira.discovery_title")}
        </div>
        <button type="button" className="btn btn-sm" onClick={load} disabled={loading}>
          {loading ? "…" : vm.refreshLabel}
        </button>
      </div>

      {error ? (
        <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>{error}</div>
      ) : null}

      {loading && !status ? (
        <div style={{ fontSize: 12, color: "var(--text-2)", padding: "8px 0" }}>{vm.loadingLabel}</div>
      ) : (
        <JiraConnectionCard vm={vm.connection} />
      )}

      {!loading && vm.connection.connected ? (
        <>
          <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
            {vm.projectsLabel}
          </div>
          {vm.showEmptyProjects ? (
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 12px" }}>{vm.emptyProjectsText}</p>
          ) : (
            <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
              {vm.projects.map((p) => (
                <JiraProjectCard key={p.projectId || p.projectKey} vm={p} />
              ))}
            </div>
          )}

          <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
            {vm.issuesLabel}
          </div>
          {vm.showEmptyIssues ? (
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: 0 }}>{vm.emptyIssuesText}</p>
          ) : (
            <div style={{ display: "grid", gap: 8 }}>
              {vm.issues.map((issue) => (
                <JiraIssueCard key={issue.issueId || issue.issueKey} vm={issue} />
              ))}
            </div>
          )}
        </>
      ) : null}
    </div>
  );
}
