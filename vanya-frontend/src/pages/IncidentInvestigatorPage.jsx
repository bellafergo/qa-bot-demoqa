// src/pages/IncidentInvestigatorPage.jsx
/**
 * Autonomous Incident Investigator — describe a problem, Vanya probes with Playwright
 * and returns heuristic diagnosis + evidence.
 */
import React, { useCallback, useEffect, useState } from "react";
import {
  investigateIncident,
  listIncidentRuns,
  getIncidentRun,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";

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

function severityBadge(sev) {
  const v = String(sev || "info").toLowerCase();
  if (v === "critical") return "badge badge-red";
  if (v === "high") return "badge badge-orange";
  if (v === "medium") return "badge badge-orange";
  if (v === "low") return "badge badge-blue";
  return "badge badge-gray";
}

function reproducedLabel(rep, t) {
  const v = String(rep || "unknown").toLowerCase();
  if (v === "true") return t("incident.reproduced.yes");
  if (v === "false") return t("incident.reproduced.no");
  return t("incident.reproduced.unknown");
}

function InvestigationResult({ run, t }) {
  if (!run) return null;
  const screenshot = run.screenshot_url || (run.screenshot_b64 ? `data:image/png;base64,${run.screenshot_b64}` : null);

  return (
    <div className="card" style={{ padding: "20px 24px", marginTop: 20 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 12, flexWrap: "wrap", marginBottom: 16 }}>
        <div>
          <div className="section-title" style={{ margin: 0 }}>{t("incident.result.title")}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>
            {fmtTs(run.created_at)} · {run.id?.slice(0, 8)}…
          </div>
        </div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
          <span className={severityBadge(run.severity)}>{run.severity}</span>
          <span className="badge badge-gray">{reproducedLabel(run.reproduced, t)}</span>
          <span className="badge badge-gray">{run.suspected_area || "unknown"}</span>
        </div>
      </div>

      {run.error_message ? (
        <div className="alert alert-error" style={{ marginBottom: 16, fontSize: 13 }}>{run.error_message}</div>
      ) : null}

      <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 8 }}>
        {run.diagnosis_summary || t("incident.result.no_diagnosis")}
      </div>

      {run.symptom_observed ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
          <strong>{t("incident.result.symptom")}:</strong> {run.symptom_observed}
        </div>
      ) : null}

      {run.probable_cause ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
          <strong>{t("incident.result.cause")}:</strong> {run.probable_cause}
        </div>
      ) : null}

      {run.suspected_endpoint ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, fontFamily: "monospace", wordBreak: "break-all" }}>
          {t("incident.result.endpoint")}: {run.suspected_endpoint}
        </div>
      ) : null}

      {run.target_url ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, wordBreak: "break-all" }}>
          {t("incident.form.target_url")}: {run.target_url}
        </div>
      ) : null}

      {screenshot ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8, textTransform: "uppercase", letterSpacing: "0.06em" }}>
            {t("incident.result.screenshot")}
          </div>
          <img
            src={screenshot}
            alt="investigation screenshot"
            style={{ maxWidth: "100%", borderRadius: 8, border: "1px solid var(--border)" }}
          />
        </div>
      ) : null}

      {run.recommendations?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.recommendations")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {run.recommendations.map((r, i) => <li key={i}>{r}</li>)}
          </ul>
        </div>
      ) : null}

      {run.reproduction_steps?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.repro_steps")}</div>
          <ol style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {run.reproduction_steps.map((s, i) => <li key={i}>{s}</li>)}
          </ol>
        </div>
      ) : null}

      {run.steps_executed?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.steps")}</div>
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {run.steps_executed.map((s, i) => (
              <span key={i} className="badge badge-gray" style={{ fontFamily: "monospace", fontSize: 10 }}>{s}</span>
            ))}
          </div>
        </div>
      ) : null}

      {(run.console_errors?.length > 0 || run.network_errors?.length > 0 || run.http_errors?.length > 0) ? (
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))", gap: 16 }}>
          {run.console_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.console")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.console_errors.map((e, i) => `${i + 1}. ${e.text || JSON.stringify(e)}`).join("\n")}
              </pre>
            </div>
          ) : null}
          {run.http_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.http")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.http_errors.map((e, i) => `${i + 1}. ${e.status} ${e.method || "GET"} ${e.url}`).join("\n")}
              </pre>
            </div>
          ) : null}
          {run.network_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.network")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.network_errors.map((e, i) => `${i + 1}. ${e.url} — ${e.failure || e.error || ""}`).join("\n")}
              </pre>
            </div>
          ) : null}
        </div>
      ) : null}
    </div>
  );
}

export default function IncidentInvestigatorPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const projectId = currentProject?.id;

  const [description, setDescription] = useState("");
  const [targetUrl, setTargetUrl] = useState("");
  const [moduleHint, setModuleHint] = useState("");
  const [investigating, setInvestigating] = useState(false);
  const [error, setError] = useState("");
  const [result, setResult] = useState(null);

  const [history, setHistory] = useState([]);
  const [historyLoading, setHistoryLoading] = useState(true);
  const [historyError, setHistoryError] = useState("");

  const loadHistory = useCallback(async () => {
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

  useEffect(() => {
    loadHistory();
  }, [loadHistory]);

  const handleInvestigate = async (e) => {
    e.preventDefault();
    const desc = description.trim();
    if (desc.length < 3) {
      setError(t("incident.form.desc_required"));
      return;
    }
    setInvestigating(true);
    setError("");
    setResult(null);
    try {
      const body = {
        incident_description: desc,
        allow_destructive_actions: false,
        credentials_mode: "none",
      };
      if (targetUrl.trim()) body.target_url = targetUrl.trim();
      if (projectId) body.project_id = projectId;
      if (moduleHint.trim()) body.module = moduleHint.trim();
      const run = await investigateIncident(body);
      setResult(run);
      await loadHistory();
    } catch (err) {
      setError(apiErrorMessage(err) || t("incident.form.error"));
    } finally {
      setInvestigating(false);
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

        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 12, marginBottom: 16 }}>
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
          {t("incident.form.investigating_hint")}
        </div>
      ) : null}

      <InvestigationResult run={result} t={t} />

      <div style={{ marginTop: 32 }}>
        <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12 }}>
          <div className="section-title" style={{ margin: 0 }}>{t("incident.history.title")}</div>
          <button type="button" className="btn btn-ghost btn-sm" onClick={loadHistory} disabled={historyLoading}>
            {t("dash.refresh")}
          </button>
        </div>

        {historyLoading ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.history.loading")}</div>
        ) : historyError ? (
          <div className="alert alert-error" style={{ fontSize: 13 }}>{historyError}</div>
        ) : history.length === 0 ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.history.empty")}</div>
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
