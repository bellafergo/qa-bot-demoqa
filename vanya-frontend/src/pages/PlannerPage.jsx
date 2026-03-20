// src/pages/PlannerPage.jsx
/**
 * PlannerPage — Natural-language test generation and execution.
 * POST /plan_from_text  |  POST /execute_text
 */
import React, { useState } from "react";
import { useLang } from "../i18n/LangContext";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

export default function PlannerPage({ embedded = false }) {
  const { t } = useLang();
  const [text, setText]       = useState("");
  const [baseUrl, setBaseUrl] = useState("");
  const [appHint, setAppHint] = useState("");
  const [confirm, setConfirm] = useState(false);

  const [plan, setPlan]       = useState(null);
  const [run, setRun]         = useState(null);
  const [error, setError]     = useState("");
  const [loading, setLoading] = useState(false);
  const [mode, setMode]       = useState("plan"); // "plan" | "execute"

  const handlePlan = async () => {
    if (!text.trim()) return;
    setLoading(true); setError(""); setPlan(null); setRun(null);
    try {
      const res = await fetch(`${API_BASE}/plan_from_text`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ text: text.trim(), base_url: baseUrl.trim() || null, app_hint: appHint.trim() || null, max_steps: 25 }),
      });
      const data = await res.json();
      if (!res.ok) setError(data?.detail || `HTTP ${res.status}`);
      else setPlan(data?.plan || data);
    } catch (e) { setError(e?.message || "Network error"); }
    finally { setLoading(false); }
  };

  const handleExecute = async () => {
    if (!text.trim()) return;
    setLoading(true); setError(""); setPlan(null); setRun(null);
    try {
      const res = await fetch(`${API_BASE}/execute_text`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ text: text.trim(), base_url: baseUrl.trim() || null, app_hint: appHint.trim() || null, max_steps: 25, confirm, headless: true }),
      });
      const data = await res.json();
      if (!res.ok) { setError(data?.detail || `HTTP ${res.status}`); }
      else {
        setPlan(data?.plan || null);
        setRun(data?.run || data);
        if (data?.reason) setError(data.reason);
      }
    } catch (e) { setError(e?.message || "Network error"); }
    finally { setLoading(false); }
  };

  const hasPaymentRisk = plan?.risk_flags?.includes("payment");
  const planOk = plan?.ok;

  return (
    <div className="page-wrap" style={{ maxWidth: 900 }}>
      {/* ── Page header ────────────────────────────────── */}
      {!embedded && (
        <div className="page-header">
          <h1 className="page-title">{t("planner.title")}</h1>
          <p className="page-subtitle">{t("planner.subtitle")}</p>
        </div>
      )}

      {/* ── Input card ─────────────────────────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("planner.section.desc")}</div>

        <div style={{ display: "grid", gap: 12 }}>
          <textarea
            className="input"
            value={text}
            onChange={e => setText(e.target.value)}
            placeholder={t("planner.ph.text")}
            rows={4}
          />

          <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
            <input
              className="input"
              value={baseUrl}
              onChange={e => setBaseUrl(e.target.value)}
              placeholder={t("planner.ph.base_url")}
              style={{ flex: 1, minWidth: 200 }}
            />
            <input
              className="input"
              value={appHint}
              onChange={e => setAppHint(e.target.value)}
              placeholder={t("planner.ph.app_hint")}
              style={{ flex: 1, minWidth: 140 }}
            />
          </div>

          <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: 13, color: "var(--text-2)", cursor: "pointer" }}>
            <input
              type="checkbox"
              checked={confirm}
              onChange={e => setConfirm(e.target.checked)}
              style={{ width: 14, height: 14, accentColor: "var(--accent)" }}
            />
            {t("planner.confirm_risky")}
          </label>
        </div>

        {/* Action buttons */}
        <div style={{ display: "flex", gap: 10, marginTop: 16 }}>
          <button
            className={`btn ${mode === "plan" ? "btn-secondary" : "btn-ghost"}`}
            onClick={() => { setMode("plan"); handlePlan(); }}
            disabled={loading || !text.trim()}
            style={mode === "plan" ? { borderColor: "var(--accent)", color: "var(--accent)" } : {}}
          >
            {loading && mode === "plan" ? t("planner.btn.generating") : t("planner.btn.generate")}
          </button>
          <button
            className="btn btn-primary"
            onClick={() => { setMode("execute"); handleExecute(); }}
            disabled={loading || !text.trim()}
          >
            {loading && mode === "execute" ? t("planner.btn.executing") : t("planner.btn.execute")}
          </button>
        </div>

        {error && (
          <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>
        )}
      </div>

      {/* ── Plan output ────────────────────────────────── */}
      {plan && (
        <div className="card" style={{ marginBottom: 20 }}>
          <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 16, flexWrap: "wrap" }}>
            <h2 style={{ margin: 0, fontSize: 16, fontWeight: 800, color: "var(--text)" }}>
              {t("planner.plan.title")}
            </h2>
            <span className={`badge ${planOk ? "badge-green" : "badge-red"}`}>
              {planOk ? t("planner.plan.valid") : t("planner.plan.invalid")}
            </span>
            {plan.language && <span className="badge badge-gray">{plan.language}</span>}
            {plan.intent   && <span style={{ fontSize: 12, color: "var(--text-2)" }}>{plan.intent}</span>}
          </div>

          {plan.summary && (
            <p style={{ margin: "0 0 16px", fontSize: 14, color: "var(--text-2)", lineHeight: 1.6 }}>
              {plan.summary}
            </p>
          )}

          {hasPaymentRisk && (
            <div className="alert alert-warn" style={{ marginBottom: 16 }}>
              {t("planner.plan.payment_risk")}{plan.requires_confirmation ? ` ${t("planner.plan.confirm_req")}` : ""}
            </div>
          )}

          {/* Steps */}
          {plan.steps?.length > 0 && (
            <div style={{ marginBottom: 16 }}>
              <div className="section-title">{t("planner.plan.steps")} ({plan.steps.length})</div>
              <div className="card-inner" style={{ padding: 0, overflow: "hidden" }}>
                {plan.steps.map((step, i) => (
                  <div
                    key={i}
                    style={{
                      padding: "9px 14px",
                      borderBottom: i < plan.steps.length - 1 ? "1px solid var(--border-light)" : "none",
                      display: "flex",
                      gap: 10,
                      alignItems: "baseline",
                    }}
                  >
                    <span style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", width: 22, flexShrink: 0, textAlign: "right" }}>
                      {i + 1}
                    </span>
                    <code style={{ fontSize: 12, color: "var(--accent)", fontWeight: 700 }}>
                      {step.action}
                    </code>
                    {step.url && <span style={{ fontSize: 12, color: "var(--text-2)" }}>{step.url}</span>}
                    {step.value && <span style={{ fontSize: 12, color: "var(--text-2)" }}>→ <em>{step.value}</em></span>}
                    {step.selector && (
                      <span style={{ fontSize: 11, color: "var(--text-3)", fontFamily: "monospace" }}>
                        {step.selector.slice(0, 50)}
                      </span>
                    )}
                    {step.product && <span style={{ fontSize: 12, color: "var(--text-2)" }}>{step.product}</span>}
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Assumptions */}
          {plan.assumptions?.length > 0 && (
            <div style={{ marginBottom: 12 }}>
              <div className="section-title">{t("planner.plan.assumptions")}</div>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
                {plan.assumptions.map((a, i) => <li key={i}>{a}</li>)}
              </ul>
            </div>
          )}

          {plan.errors?.length > 0 && (
            <div>
              <div className="section-title" style={{ color: "var(--red)" }}>{t("planner.plan.errors")}</div>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--red-text)", lineHeight: 1.7 }}>
                {plan.errors.map((e, i) => <li key={i}>{e}</li>)}
              </ul>
            </div>
          )}
        </div>
      )}

      {/* ── Execution result ───────────────────────────── */}
      {run?.evidence_id && (
        <div className="card" style={{ borderColor: "var(--green-border)", background: "var(--green-bg)" }}>
          <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 14 }}>
            <h2 style={{ margin: 0, fontSize: 16, fontWeight: 800, color: "var(--green-text)" }}>
              {t("planner.run.title")}
            </h2>
            {run.status && (
              <span className={`badge ${String(run.status).toLowerCase().includes("pass") ? "badge-green" : "badge-red"}`}>
                {run.status}
              </span>
            )}
          </div>

          <div style={{ display: "grid", gap: 8, fontSize: 13, color: "var(--text)" }}>
            {run.status && (
              <div style={{ display: "flex", gap: 8 }}>
                <span style={{ color: "var(--text-3)", width: 100 }}>{t("planner.run.status")}</span>
                <strong>{run.status}</strong>
              </div>
            )}
            {run.evidence_id && (
              <div style={{ display: "flex", gap: 8 }}>
                <span style={{ color: "var(--text-3)", width: 100 }}>{t("planner.run.evidence_id")}</span>
                <code style={{ fontSize: 12 }}>{run.evidence_id}</code>
              </div>
            )}
            {run.evidence_url && (
              <div style={{ display: "flex", gap: 8 }}>
                <span style={{ color: "var(--text-3)", width: 100 }}>{t("planner.run.evidence")}</span>
                <a href={run.evidence_url} target="_blank" rel="noreferrer" style={{ color: "var(--accent)", fontSize: 13 }}>
                  {t("planner.run.view")}
                </a>
              </div>
            )}
            {run.duration_ms && (
              <div style={{ display: "flex", gap: 8 }}>
                <span style={{ color: "var(--text-3)", width: 100 }}>{t("planner.run.duration")}</span>
                <span>{run.duration_ms}ms</span>
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}
