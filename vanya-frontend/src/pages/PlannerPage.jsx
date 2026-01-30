// src/pages/PlannerPage.jsx
/**
 * PlannerPage - Natural Language Test Planner UI
 *
 * Uses the backend endpoints:
 * - POST /plan_from_text  (generate plan without execution)
 * - POST /execute_text    (generate plan + execute)
 */
import React, { useState } from "react";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

export default function PlannerPage() {
  const [text, setText] = useState("");
  const [baseUrl, setBaseUrl] = useState("");
  const [appHint, setAppHint] = useState("");
  const [confirm, setConfirm] = useState(false);

  const [plan, setPlan] = useState(null);
  const [run, setRun] = useState(null);
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);
  const [mode, setMode] = useState("plan"); // "plan" | "execute"

  const handlePlan = async () => {
    if (!text.trim()) return;
    setLoading(true);
    setError("");
    setPlan(null);
    setRun(null);

    try {
      const res = await fetch(`${API_BASE}/plan_from_text`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          text: text.trim(),
          base_url: baseUrl.trim() || null,
          app_hint: appHint.trim() || null,
          max_steps: 25,
        }),
      });

      const data = await res.json();
      if (!res.ok) {
        setError(data?.detail || `HTTP ${res.status}`);
      } else {
        setPlan(data?.plan || data);
      }
    } catch (e) {
      setError(e?.message || "Network error");
    } finally {
      setLoading(false);
    }
  };

  const handleExecute = async () => {
    if (!text.trim()) return;
    setLoading(true);
    setError("");
    setPlan(null);
    setRun(null);

    try {
      const res = await fetch(`${API_BASE}/execute_text`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          text: text.trim(),
          base_url: baseUrl.trim() || null,
          app_hint: appHint.trim() || null,
          max_steps: 25,
          confirm: confirm,
          headless: true,
        }),
      });

      const data = await res.json();
      if (!res.ok) {
        setError(data?.detail || `HTTP ${res.status}`);
      } else {
        setPlan(data?.plan || null);
        setRun(data?.run || data);
        if (data?.reason) {
          setError(data.reason);
        }
      }
    } catch (e) {
      setError(e?.message || "Network error");
    } finally {
      setLoading(false);
    }
  };

  const hasPaymentRisk = plan?.risk_flags?.includes("payment");

  return (
    <div style={{ padding: 20, maxWidth: 900, margin: "0 auto" }}>
      <h2 style={{ marginBottom: 16, fontWeight: 800, color: "white" }}>
        Natural Language Test Planner
      </h2>

      <p style={{ marginBottom: 20, opacity: 0.75, color: "white", fontSize: 14 }}>
        Describe what you want to test in natural language. Vanya will generate
        a test plan and optionally execute it.
      </p>

      {/* Input area */}
      <div style={{ display: "grid", gap: 12, marginBottom: 20 }}>
        <textarea
          value={text}
          onChange={(e) => setText(e.target.value)}
          placeholder="E.g.: Add tomato and milk to the cart in HEB"
          rows={4}
          style={{
            width: "100%",
            padding: "12px 14px",
            borderRadius: 12,
            border: "1px solid rgba(255,255,255,0.14)",
            background: "rgba(0,0,0,0.25)",
            color: "white",
            fontSize: 14,
            resize: "vertical",
            outline: "none",
          }}
        />

        <div style={{ display: "flex", gap: 12, flexWrap: "wrap" }}>
          <input
            value={baseUrl}
            onChange={(e) => setBaseUrl(e.target.value)}
            placeholder="Base URL (optional)"
            style={{
              flex: 1,
              minWidth: 200,
              padding: "10px 12px",
              borderRadius: 10,
              border: "1px solid rgba(255,255,255,0.14)",
              background: "rgba(0,0,0,0.25)",
              color: "white",
              outline: "none",
            }}
          />

          <input
            value={appHint}
            onChange={(e) => setAppHint(e.target.value)}
            placeholder="App hint (e.g., HEB)"
            style={{
              flex: 1,
              minWidth: 150,
              padding: "10px 12px",
              borderRadius: 10,
              border: "1px solid rgba(255,255,255,0.14)",
              background: "rgba(0,0,0,0.25)",
              color: "white",
              outline: "none",
            }}
          />
        </div>

        <label
          style={{
            display: "flex",
            alignItems: "center",
            gap: 8,
            fontSize: 13,
            color: "white",
            opacity: 0.9,
          }}
        >
          <input
            type="checkbox"
            checked={confirm}
            onChange={(e) => setConfirm(e.target.checked)}
          />
          Confirm risky actions (payment/checkout)
        </label>
      </div>

      {/* Buttons */}
      <div style={{ display: "flex", gap: 12, marginBottom: 20 }}>
        <button
          onClick={() => {
            setMode("plan");
            handlePlan();
          }}
          disabled={loading || !text.trim()}
          style={{
            padding: "10px 20px",
            borderRadius: 10,
            border: "1px solid rgba(255,255,255,0.14)",
            background:
              mode === "plan" ? "rgba(78,107,255,0.3)" : "rgba(0,0,0,0.3)",
            color: "white",
            cursor: loading ? "not-allowed" : "pointer",
            fontWeight: 700,
          }}
        >
          {loading && mode === "plan" ? "Planning..." : "Generate Plan"}
        </button>

        <button
          onClick={() => {
            setMode("execute");
            handleExecute();
          }}
          disabled={loading || !text.trim()}
          style={{
            padding: "10px 20px",
            borderRadius: 10,
            border: "1px solid rgba(78,107,255,0.4)",
            background:
              mode === "execute" ? "rgba(78,107,255,0.4)" : "rgba(78,107,255,0.2)",
            color: "white",
            cursor: loading ? "not-allowed" : "pointer",
            fontWeight: 700,
          }}
        >
          {loading && mode === "execute" ? "Executing..." : "Plan & Execute"}
        </button>
      </div>

      {/* Error */}
      {error && (
        <div
          style={{
            padding: "10px 14px",
            borderRadius: 10,
            background: "rgba(255,60,60,0.15)",
            border: "1px solid rgba(255,60,60,0.3)",
            color: "#ff6b6b",
            marginBottom: 16,
            fontSize: 13,
          }}
        >
          {error}
        </div>
      )}

      {/* Plan output */}
      {plan && (
        <div
          style={{
            padding: 16,
            borderRadius: 12,
            border: "1px solid rgba(255,255,255,0.12)",
            background: "rgba(0,0,0,0.2)",
            marginBottom: 16,
          }}
        >
          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: 10,
              marginBottom: 12,
            }}
          >
            <h3 style={{ margin: 0, fontWeight: 800, color: "white" }}>Plan</h3>

            <span
              style={{
                padding: "2px 8px",
                borderRadius: 999,
                fontSize: 11,
                fontWeight: 700,
                background: plan.ok
                  ? "rgba(82,196,26,0.2)"
                  : "rgba(255,77,79,0.2)",
                color: plan.ok ? "#52c41a" : "#ff4d4f",
                border: `1px solid ${plan.ok ? "#52c41a" : "#ff4d4f"}`,
              }}
            >
              {plan.ok ? "OK" : "INVALID"}
            </span>

            {plan.language && (
              <span style={{ fontSize: 12, opacity: 0.7, color: "white" }}>
                Lang: {plan.language}
              </span>
            )}

            {plan.intent && (
              <span style={{ fontSize: 12, opacity: 0.7, color: "white" }}>
                Intent: {plan.intent}
              </span>
            )}
          </div>

          {plan.summary && (
            <p style={{ margin: "0 0 12px", color: "white", opacity: 0.9 }}>
              {plan.summary}
            </p>
          )}

          {/* Risk flags */}
          {hasPaymentRisk && (
            <div
              style={{
                padding: "8px 12px",
                borderRadius: 8,
                background: "rgba(255,165,0,0.15)",
                border: "1px solid rgba(255,165,0,0.3)",
                color: "#ffa500",
                fontSize: 12,
                marginBottom: 12,
              }}
            >
              ⚠️ Payment risk detected. {plan.requires_confirmation ? "Confirmation required." : ""}
            </div>
          )}

          {/* Steps */}
          {plan.steps?.length > 0 && (
            <div style={{ marginBottom: 12 }}>
              <h4 style={{ margin: "0 0 8px", fontWeight: 700, color: "white" }}>
                Steps ({plan.steps.length})
              </h4>
              <div
                style={{
                  background: "rgba(0,0,0,0.25)",
                  borderRadius: 8,
                  padding: 10,
                  fontSize: 12,
                  overflowX: "auto",
                }}
              >
                {plan.steps.map((step, i) => (
                  <div
                    key={i}
                    style={{
                      padding: "4px 0",
                      borderBottom:
                        i < plan.steps.length - 1
                          ? "1px solid rgba(255,255,255,0.08)"
                          : "none",
                      color: "white",
                    }}
                  >
                    <span style={{ opacity: 0.6, marginRight: 8 }}>{i + 1}.</span>
                    <span style={{ fontWeight: 700 }}>{step.action}</span>
                    {step.url && (
                      <span style={{ marginLeft: 8, opacity: 0.75 }}>
                        url: {step.url}
                      </span>
                    )}
                    {step.value && (
                      <span style={{ marginLeft: 8, opacity: 0.75 }}>
                        value: {step.value}
                      </span>
                    )}
                    {step.selector && (
                      <span style={{ marginLeft: 8, opacity: 0.75 }}>
                        sel: {step.selector.slice(0, 40)}
                      </span>
                    )}
                    {step.product && (
                      <span style={{ marginLeft: 8, opacity: 0.75 }}>
                        product: {step.product}
                      </span>
                    )}
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Assumptions */}
          {plan.assumptions?.length > 0 && (
            <div style={{ marginBottom: 12 }}>
              <h4 style={{ margin: "0 0 6px", fontWeight: 700, color: "white", fontSize: 13 }}>
                Assumptions
              </h4>
              <ul style={{ margin: 0, paddingLeft: 20, color: "white", opacity: 0.8, fontSize: 12 }}>
                {plan.assumptions.map((a, i) => (
                  <li key={i}>{a}</li>
                ))}
              </ul>
            </div>
          )}

          {/* Errors */}
          {plan.errors?.length > 0 && (
            <div>
              <h4 style={{ margin: "0 0 6px", fontWeight: 700, color: "#ff4d4f", fontSize: 13 }}>
                Errors
              </h4>
              <ul style={{ margin: 0, paddingLeft: 20, color: "#ff6b6b", fontSize: 12 }}>
                {plan.errors.map((e, i) => (
                  <li key={i}>{e}</li>
                ))}
              </ul>
            </div>
          )}
        </div>
      )}

      {/* Run output */}
      {run && run.evidence_id && (
        <div
          style={{
            padding: 16,
            borderRadius: 12,
            border: "1px solid rgba(82,196,26,0.3)",
            background: "rgba(82,196,26,0.1)",
          }}
        >
          <h3 style={{ margin: "0 0 10px", fontWeight: 800, color: "#52c41a" }}>
            Execution Result
          </h3>

          <div style={{ display: "grid", gap: 6, fontSize: 13, color: "white" }}>
            <div>
              <span style={{ opacity: 0.7 }}>Status: </span>
              <span style={{ fontWeight: 700 }}>{run.status || "unknown"}</span>
            </div>
            <div>
              <span style={{ opacity: 0.7 }}>Evidence ID: </span>
              <span style={{ fontWeight: 700 }}>{run.evidence_id}</span>
            </div>
            {run.evidence_url && (
              <div>
                <span style={{ opacity: 0.7 }}>Evidence URL: </span>
                <a
                  href={run.evidence_url}
                  target="_blank"
                  rel="noreferrer"
                  style={{ color: "#69b1ff" }}
                >
                  {run.evidence_url}
                </a>
              </div>
            )}
            {run.duration_ms && (
              <div>
                <span style={{ opacity: 0.7 }}>Duration: </span>
                <span>{run.duration_ms}ms</span>
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}
