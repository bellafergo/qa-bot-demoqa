// src/pages/IncidentInvestigatorPage.jsx
/**
 * Autonomous Incident Investigator — describe a problem, Vanya probes with Playwright
 * and returns heuristic diagnosis + evidence.
 */
import React, { useCallback, useEffect, useState } from "react";
import {
  investigateProjectIncident,
  listProjectIncidentHistory,
  getProjectIncidentReport,
  listIncidentRuns,
  getIncidentRun,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import { normalizeProjectIncidentHistory } from "../utils/incidentHistoryViewUtils.js";
import {
  fmtTs,
  severityBadge,
  reproducedLabel,
  confidencePct,
} from "../utils/incidentInvestigatorHelpers.js";
import { isInsufficientEvidence } from "../utils/qaInvestigationReportLayoutUtils.js";
import QaInvestigationReport from "../components/incident/QaInvestigationReport.jsx";
import InvestigationResult from "../components/incident/InvestigationResult.jsx";

export default function IncidentInvestigatorPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const projectId = currentProject?.id;

  const [description, setDescription] = useState("");
  const [targetUrl, setTargetUrl] = useState("");
  const [moduleHint, setModuleHint] = useState("");
  const [severity, setSeverity] = useState("medium");
  const [timeWindowHours, setTimeWindowHours] = useState(72);
  const [includeBrowserProbe, setIncludeBrowserProbe] = useState(false);
  const [investigating, setInvestigating] = useState(false);
  const [error, setError] = useState("");
  const [qaReport, setQaReport] = useState(null);
  const [result, setResult] = useState(null);

  const [history, setHistory] = useState([]);
  const [historyLoading, setHistoryLoading] = useState(true);
  const [historyError, setHistoryError] = useState("");

  const [qaHistory, setQaHistory] = useState([]);
  const [qaHistoryLoading, setQaHistoryLoading] = useState(true);
  const [qaHistoryError, setQaHistoryError] = useState("");

  const loadBrowserHistory = useCallback(async () => {
    setHistoryLoading(true);
    setHistoryError("");
    try {
      const data = await listIncidentRuns({
        limit: 30,
        project_id: projectId || undefined,
      });
      setHistory(Array.isArray(data?.items) ? data.items : []);
    } catch (e) {
      setHistory([]);
      setHistoryError(apiErrorMessage(e) || t("incident.history.error"));
    } finally {
      setHistoryLoading(false);
    }
  }, [projectId, t]);

  const loadQaHistory = useCallback(async () => {
    if (!projectId) {
      setQaHistory([]);
      setQaHistoryLoading(false);
      return;
    }
    setQaHistoryLoading(true);
    setQaHistoryError("");
    try {
      const data = await listProjectIncidentHistory(projectId, { limit: 30 });
      setQaHistory(normalizeProjectIncidentHistory(data));
    } catch (e) {
      setQaHistory([]);
      setQaHistoryError(apiErrorMessage(e) || t("incident.qa.history.error"));
    } finally {
      setQaHistoryLoading(false);
    }
  }, [projectId, t]);

  useEffect(() => {
    loadBrowserHistory();
    loadQaHistory();
  }, [loadBrowserHistory, loadQaHistory]);

  const handleInvestigate = async (e) => {
    e.preventDefault();
    const desc = description.trim();
    if (desc.length < 3) {
      setError(t("incident.form.desc_required"));
      return;
    }
    if (!projectId) {
      setError(t("incident.form.no_project"));
      return;
    }
    setInvestigating(true);
    setError("");
    setQaReport(null);
    setResult(null);
    try {
      const body = {
        description: desc,
        severity,
        time_window_hours: Number(timeWindowHours) || 72,
        include_browser_probe: includeBrowserProbe && !!targetUrl.trim(),
      };
      if (targetUrl.trim()) body.target_url = targetUrl.trim();
      if (moduleHint.trim()) body.module = moduleHint.trim();
      const report = await investigateProjectIncident(projectId, body);
      setQaReport(report);
      await loadQaHistory();
      if (report.browser_investigation) {
        setResult(report.browser_investigation);
        await loadBrowserHistory();
      }
    } catch (err) {
      setError(apiErrorMessage(err) || t("incident.form.error"));
    } finally {
      setInvestigating(false);
    }
  };

  const openQaHistoryItem = async (id) => {
    if (!projectId) return;
    setError("");
    try {
      const report = await getProjectIncidentReport(projectId, id);
      setQaReport(report);
      setDescription(report.description || "");
      setSeverity(report.severity || "medium");
      setTimeWindowHours(report.time_window_hours || 72);
      if (report.browser_investigation) {
        setResult(report.browser_investigation);
      } else {
        setResult(null);
      }
      window.scrollTo({ top: 0, behavior: "smooth" });
    } catch (e) {
      setError(apiErrorMessage(e) || t("incident.qa.history.error"));
    }
  };

  const openHistoryItem = async (id) => {
    setError("");
    try {
      const run = await getIncidentRun(id);
      setResult(run);
      setDescription(run.incident_description || "");
      setTargetUrl(run.target_url || "");
      setModuleHint(run.module || "");
      window.scrollTo({ top: 0, behavior: "smooth" });
    } catch (e) {
      setError(apiErrorMessage(e) || t("incident.history.load_one_error"));
    }
  };

  return (
    <div style={{ padding: "32px 40px", maxWidth: 960, margin: "0 auto" }}>
      <div style={{ marginBottom: 28 }}>
        <h1 style={{ fontSize: 22, fontWeight: 700, margin: 0, color: "var(--text-1)" }}>{t("incident.title")}</h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", marginTop: 6, lineHeight: 1.6 }}>{t("incident.subtitle")}</p>
        {currentProject ? (
          <p style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4 }}>{t("incident.scope", { name: currentProject.name })}</p>
        ) : null}
      </div>

      <form className="card" style={{ padding: "20px 24px" }} onSubmit={handleInvestigate}>
        <div style={{ marginBottom: 14 }}>
          <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
            {t("incident.form.description")}
          </label>
          <textarea
            className="input"
            rows={4}
            value={description}
            onChange={(e) => setDescription(e.target.value)}
            placeholder={t("incident.form.description_ph")}
            disabled={investigating}
            style={{ width: "100%", resize: "vertical", minHeight: 96 }}
          />
        </div>

        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 12, marginBottom: 12 }}>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.target_url")} ({t("incident.form.optional")})
            </label>
            <input
              className="input"
              type="url"
              value={targetUrl}
              onChange={(e) => setTargetUrl(e.target.value)}
              placeholder="https://app.example.com/dashboard"
              disabled={investigating}
              style={{ width: "100%" }}
            />
          </div>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.module")} ({t("incident.form.optional")})
            </label>
            <input
              className="input"
              value={moduleHint}
              onChange={(e) => setModuleHint(e.target.value)}
              placeholder="/vacancies"
              disabled={investigating}
              style={{ width: "100%" }}
            />
          </div>
        </div>

        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 12, marginBottom: 16 }}>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.severity")}
            </label>
            <select className="input" value={severity} onChange={(e) => setSeverity(e.target.value)} disabled={investigating} style={{ width: "100%" }}>
              <option value="low">low</option>
              <option value="medium">medium</option>
              <option value="high">high</option>
              <option value="critical">critical</option>
            </select>
          </div>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.time_window")}
            </label>
            <input
              className="input"
              type="number"
              min={1}
              max={720}
              value={timeWindowHours}
              onChange={(e) => setTimeWindowHours(e.target.value)}
              disabled={investigating}
              style={{ width: "100%" }}
            />
          </div>
          <div style={{ display: "flex", alignItems: "flex-end", paddingBottom: 4 }}>
            <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: 12, color: "var(--text-2)", cursor: "pointer" }}>
              <input
                type="checkbox"
                checked={includeBrowserProbe}
                onChange={(e) => setIncludeBrowserProbe(e.target.checked)}
                disabled={investigating || !targetUrl.trim()}
              />
              {t("incident.form.browser_probe")}
            </label>
          </div>
        </div>

        {error ? (
          <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 13 }}>{error}</div>
        ) : null}

        <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
          <button type="submit" className="btn btn-primary" disabled={investigating}>
            {investigating ? t("incident.form.investigating") : t("incident.form.submit")}
          </button>
          <span style={{ fontSize: 11, color: "var(--text-3)" }}>{t("incident.form.safe_note")}</span>
        </div>
      </form>

      {investigating ? (
        <div style={{ padding: "24px 0", textAlign: "center", color: "var(--text-3)", fontSize: 13 }}>
          {includeBrowserProbe && targetUrl.trim()
            ? t("incident.form.investigating_hint")
            : t("incident.form.investigating")}
        </div>
      ) : null}

      <QaInvestigationReport report={qaReport} t={t} />
      {result ? <InvestigationResult run={result} t={t} titleKey="incident.qa.browser_title" /> : null}

      {projectId ? (
        <div style={{ marginTop: 32 }}>
          <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12 }}>
            <div className="section-title" style={{ margin: 0 }}>{t("incident.history.title")}</div>
            <button type="button" className="btn btn-ghost btn-sm" onClick={loadQaHistory} disabled={qaHistoryLoading}>
              {t("dash.refresh")}
            </button>
          </div>
          {qaHistoryLoading ? (
            <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.history.loading")}</div>
          ) : qaHistoryError ? (
            <div className="alert alert-error" style={{ fontSize: 13 }}>{qaHistoryError}</div>
          ) : qaHistory.length === 0 ? (
            <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.history.empty")}</div>
          ) : (
            <div className="card" style={{ overflow: "hidden" }}>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("incident.history.col.date")}</th>
                    <th>{t("incident.history.col.description")}</th>
                    <th>{t("incident.history.col.severity")}</th>
                    <th>{t("incident.qa.evidence_quality")}</th>
                  </tr>
                </thead>
                <tbody>
                  {qaHistory.map((h) => (
                    <tr key={h.id} style={{ cursor: "pointer" }} onClick={() => openQaHistoryItem(h.id)}>
                      <td style={{ fontSize: 12, whiteSpace: "nowrap" }}>{fmtTs(h.created_at)}</td>
                      <td style={{ fontSize: 13, maxWidth: 320, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                        {h.description}
                      </td>
                      <td><span className={severityBadge(h.severity)}>{h.severity}</span></td>
                      <td style={{ fontSize: 12 }}>
                        {isInsufficientEvidence(h.confidence)
                          ? t("incident.qa.insufficient_evidence_title")
                          : confidencePct(h.confidence)}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      ) : null}

      <div style={{ marginTop: 32 }}>
        <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12 }}>
          <div className="section-title" style={{ margin: 0 }}>{t("incident.qa.browser_history")}</div>
          <button type="button" className="btn btn-ghost btn-sm" onClick={loadBrowserHistory} disabled={historyLoading}>
            {t("dash.refresh")}
          </button>
        </div>

        {historyLoading ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.qa.browser_history.loading")}</div>
        ) : historyError ? (
          <div className="alert alert-error" style={{ fontSize: 13 }}>{historyError}</div>
        ) : history.length === 0 ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.qa.browser_history.empty")}</div>
        ) : (
          <div className="card" style={{ overflow: "hidden" }}>
            <table className="data-table">
              <thead>
                <tr>
                  <th>{t("incident.history.col.date")}</th>
                  <th>{t("incident.history.col.description")}</th>
                  <th>{t("incident.history.col.severity")}</th>
                  <th>{t("incident.history.col.status")}</th>
                  <th>{t("incident.history.col.reproduced")}</th>
                </tr>
              </thead>
              <tbody>
                {history.map((h) => (
                  <tr
                    key={h.id}
                    style={{ cursor: "pointer" }}
                    onClick={() => openHistoryItem(h.id)}
                  >
                    <td style={{ fontSize: 12, whiteSpace: "nowrap" }}>{fmtTs(h.created_at)}</td>
                    <td style={{ fontSize: 13, maxWidth: 320, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                      {h.incident_description}
                    </td>
                    <td><span className={severityBadge(h.severity)}>{h.severity}</span></td>
                    <td style={{ fontSize: 12 }}>{h.status}</td>
                    <td style={{ fontSize: 12 }}>{reproducedLabel(h.reproduced, t)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  );
}
