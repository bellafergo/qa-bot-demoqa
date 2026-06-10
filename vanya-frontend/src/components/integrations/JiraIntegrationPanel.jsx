import React, { useCallback, useEffect, useState } from "react";
import {
  getJiraStatus,
  listJiraProjects,
  listJiraIssues,
  listJiraEpics,
  listJiraReleases,
  listJiraFixVersions,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import JiraConnectionCard from "./JiraConnectionCard.jsx";
import JiraProjectCard from "./JiraProjectCard.jsx";
import JiraIssueCard from "./JiraIssueCard.jsx";
import { buildJiraIntegrationViewModel } from "../../utils/jiraViewUtils.js";

function JiraDiscoveryListSection({ section }) {
  if (!section) return null;
  return (
    <div style={{ marginBottom: 12 }}>
      <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
        {section.label}
      </div>
      {section.showEmpty ? (
        <p style={{ fontSize: 12, color: "var(--text-2)", margin: 0 }}>{section.emptyText}</p>
      ) : (
        <div style={{ display: "grid", gap: 6 }}>
          {section.items.map((item) => (
            <div
              key={item.key}
              style={{
                padding: "8px 10px",
                borderRadius: 6,
                border: "1px solid var(--border)",
                background: "var(--bg-2)",
                fontSize: 12,
                display: "flex",
                justifyContent: "space-between",
                gap: 8,
                flexWrap: "wrap",
              }}
            >
              <span style={{ fontFamily: "monospace", fontWeight: 600 }}>{item.key}</span>
              <span style={{ color: "var(--text-2)" }}>{item.label}</span>
              {item.meta ? <span style={{ color: "var(--text-3)", fontSize: 11 }}>{item.meta}</span> : null}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default function JiraIntegrationPanel({ refreshToken = 0 }) {
  const { t } = useLang();
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [status, setStatus] = useState(null);
  const [projects, setProjects] = useState([]);
  const [issues, setIssues] = useState([]);
  const [epics, setEpics] = useState([]);
  const [releases, setReleases] = useState([]);
  const [fixVersions, setFixVersions] = useState([]);

  const load = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const [st, pr, is, ep, rel, fv] = await Promise.all([
        getJiraStatus(),
        listJiraProjects(),
        listJiraIssues(),
        listJiraEpics(),
        listJiraReleases(),
        listJiraFixVersions(),
      ]);
      setStatus(st);
      setProjects(Array.isArray(pr?.projects) ? pr.projects : []);
      setIssues(Array.isArray(is?.issues) ? is.issues : []);
      setEpics(Array.isArray(ep?.epics) ? ep.epics : []);
      setReleases(Array.isArray(rel?.releases) ? rel.releases : []);
      setFixVersions(Array.isArray(fv?.fix_versions) ? fv.fix_versions : []);
    } catch (e) {
      setError(e.message || t("integrations.jira.load_error"));
      setStatus(null);
      setProjects([]);
      setIssues([]);
      setEpics([]);
      setReleases([]);
      setFixVersions([]);
    } finally {
      setLoading(false);
    }
  }, [t]);

  useEffect(() => {
    load();
  }, [load, refreshToken]);

  const vm = buildJiraIntegrationViewModel({
    status,
    projects,
    issues,
    epics,
    releases,
    fixVersions,
    t,
  });

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
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 12px" }}>{vm.emptyIssuesText}</p>
          ) : (
            <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
              {vm.issues.map((issue) => (
                <JiraIssueCard key={issue.issueId || issue.issueKey} vm={issue} />
              ))}
            </div>
          )}

          <JiraDiscoveryListSection section={vm.epics} />
          <JiraDiscoveryListSection section={vm.releases} />
          <JiraDiscoveryListSection section={vm.fixVersions} />
        </>
      ) : null}
    </div>
  );
}
