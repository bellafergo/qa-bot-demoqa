// src/pages/RunsPage.jsx
/**
 * RunsPage - View test executions and evidence
 *
 * Uses the backend endpoint:
 * - GET /runs/{evidence_id}
 */
import React, { useState, useCallback } from "react";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

export default function RunsPage() {
  const [evidenceId, setEvidenceId] = useState("");
  const [loading, setLoading] = useState(false);
  const [run, setRun] = useState(null);
  const [error, setError] = useState("");

  const handleFetch = useCallback(async () => {
    const id = evidenceId.trim();
    if (!id) return;

    setLoading(true);
    setError("");
    setRun(null);

    try {
      const res = await fetch(`${API_BASE}/runs/${id}?format=json`, {
        method: "GET",
        headers: { Accept: "application/json" },
      });

      if (!res.ok) {
        if (res.status === 404) {
          setError(`Run not found: ${id}`);
        } else {
          const data = await res.json().catch(() => ({}));
          setError(data?.detail || `HTTP ${res.status}`);
        }
      } else {
        const data = await res.json();
        setRun(data);
      }
    } catch (e) {
      setError(e?.message || "Network error");
    } finally {
      setLoading(false);
    }
  }, [evidenceId]);

  const statusColor = (status) => {
    const s = String(status || "").toLowerCase();
    if (s.includes("pass")) return "#52c41a";
    if (s.includes("fail") || s.includes("error")) return "#ff4d4f";
    return "rgba(255,255,255,0.7)";
  };

  return (
    <div style={{ padding: 20, maxWidth: 1000, margin: "0 auto" }}>
      <h2 style={{ marginBottom: 16, fontWeight: 800, color: "white" }}>
        Test Runs & Evidence
      </h2>

      <p style={{ marginBottom: 24, opacity: 0.75, color: "white", fontSize: 14 }}>
        Look up test execution results by evidence ID. View steps, screenshots,
        logs, and reports.
      </p>

      {/* Lookup Section */}
      <section
        style={{
          padding: 20,
          borderRadius: 12,
          border: "1px solid rgba(255,255,255,0.12)",
          background: "rgba(0,0,0,0.2)",
          marginBottom: 24,
        }}
      >
        <h3 style={{ margin: "0 0 16px", fontWeight: 700, color: "white" }}>
          Lookup Run
        </h3>

        <div style={{ display: "flex", gap: 12 }}>
          <input
            value={evidenceId}
            onChange={(e) => setEvidenceId(e.target.value)}
            placeholder="Evidence ID (e.g., EV-abc123...)"
            onKeyDown={(e) => e.key === "Enter" && handleFetch()}
            style={{
              flex: 1,
              padding: "10px 12px",
              borderRadius: 10,
              border: "1px solid rgba(255,255,255,0.14)",
              background: "rgba(0,0,0,0.25)",
              color: "white",
              outline: "none",
            }}
          />

          <button
            onClick={handleFetch}
            disabled={loading || !evidenceId.trim()}
            style={{
              padding: "10px 20px",
              borderRadius: 10,
              border: "1px solid rgba(78,107,255,0.4)",
              background: "rgba(78,107,255,0.3)",
              color: "white",
              cursor: loading || !evidenceId.trim() ? "not-allowed" : "pointer",
              fontWeight: 700,
            }}
          >
            {loading ? "Loading..." : "Fetch"}
          </button>
        </div>

        {/* Error */}
        {error && (
          <div
            style={{
              marginTop: 12,
              padding: "10px 14px",
              borderRadius: 10,
              background: "rgba(255,60,60,0.15)",
              border: "1px solid rgba(255,60,60,0.3)",
              color: "#ff6b6b",
              fontSize: 13,
            }}
          >
            {error}
          </div>
        )}
      </section>

      {/* Run Details */}
      {run && (
        <section
          style={{
            padding: 20,
            borderRadius: 12,
            border: "1px solid rgba(255,255,255,0.12)",
            background: "rgba(0,0,0,0.2)",
          }}
        >
          {/* Header */}
          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: 12,
              marginBottom: 16,
              flexWrap: "wrap",
            }}
          >
            <h3 style={{ margin: 0, fontWeight: 800, color: "white" }}>
              Run Details
            </h3>

            <span
              style={{
                padding: "4px 12px",
                borderRadius: 999,
                fontWeight: 700,
                fontSize: 12,
                color: statusColor(run.status),
                border: `1px solid ${statusColor(run.status)}`,
                textTransform: "uppercase",
              }}
            >
              {run.status || "unknown"}
            </span>

            {run.evidence_id && (
              <span style={{ fontSize: 12, opacity: 0.7, color: "white" }}>
                ID: {run.evidence_id}
              </span>
            )}
          </div>

          {/* Summary */}
          <div
            style={{
              display: "grid",
              gridTemplateColumns: "repeat(auto-fit, minmax(200px, 1fr))",
              gap: 12,
              marginBottom: 20,
            }}
          >
            {run.duration_ms && (
              <div
                style={{
                  padding: 12,
                  borderRadius: 8,
                  background: "rgba(0,0,0,0.25)",
                  border: "1px solid rgba(255,255,255,0.08)",
                }}
              >
                <div style={{ fontSize: 11, opacity: 0.6, color: "white" }}>
                  Duration
                </div>
                <div style={{ fontWeight: 700, color: "white" }}>
                  {run.duration_ms}ms
                </div>
              </div>
            )}

            {run.expected && (
              <div
                style={{
                  padding: 12,
                  borderRadius: 8,
                  background: "rgba(0,0,0,0.25)",
                  border: "1px solid rgba(255,255,255,0.08)",
                }}
              >
                <div style={{ fontSize: 11, opacity: 0.6, color: "white" }}>
                  Expected
                </div>
                <div style={{ fontWeight: 700, color: "white" }}>
                  {run.expected}
                </div>
              </div>
            )}

            {run.outcome && (
              <div
                style={{
                  padding: 12,
                  borderRadius: 8,
                  background: "rgba(0,0,0,0.25)",
                  border: "1px solid rgba(255,255,255,0.08)",
                }}
              >
                <div style={{ fontSize: 11, opacity: 0.6, color: "white" }}>
                  Outcome
                </div>
                <div style={{ fontWeight: 700, color: "white" }}>
                  {run.outcome}
                </div>
              </div>
            )}

            {run.meta?.base_url && (
              <div
                style={{
                  padding: 12,
                  borderRadius: 8,
                  background: "rgba(0,0,0,0.25)",
                  border: "1px solid rgba(255,255,255,0.08)",
                }}
              >
                <div style={{ fontSize: 11, opacity: 0.6, color: "white" }}>
                  Base URL
                </div>
                <div
                  style={{
                    fontWeight: 700,
                    color: "white",
                    wordBreak: "break-all",
                    fontSize: 12,
                  }}
                >
                  {run.meta.base_url}
                </div>
              </div>
            )}
          </div>

          {/* Reason */}
          {run.reason && (
            <div style={{ marginBottom: 16 }}>
              <h4 style={{ margin: "0 0 8px", fontWeight: 700, color: "white" }}>
                Reason
              </h4>
              <div
                style={{
                  padding: 12,
                  borderRadius: 8,
                  background: "rgba(0,0,0,0.25)",
                  border: "1px solid rgba(255,255,255,0.08)",
                  color: "white",
                  fontSize: 13,
                }}
              >
                {run.reason}
              </div>
            </div>
          )}

          {/* Links */}
          <div style={{ display: "flex", gap: 12, marginBottom: 20, flexWrap: "wrap" }}>
            {run.evidence_url && (
              <a
                href={run.evidence_url}
                target="_blank"
                rel="noreferrer"
                style={{
                  padding: "8px 14px",
                  borderRadius: 8,
                  background: "rgba(78,107,255,0.2)",
                  border: "1px solid rgba(78,107,255,0.4)",
                  color: "#69b1ff",
                  textDecoration: "none",
                  fontWeight: 600,
                  fontSize: 13,
                }}
              >
                View Evidence
              </a>
            )}
            {run.report_url && (
              <a
                href={run.report_url}
                target="_blank"
                rel="noreferrer"
                style={{
                  padding: "8px 14px",
                  borderRadius: 8,
                  background: "rgba(82,196,26,0.2)",
                  border: "1px solid rgba(82,196,26,0.4)",
                  color: "#52c41a",
                  textDecoration: "none",
                  fontWeight: 600,
                  fontSize: 13,
                }}
              >
                Download Report
              </a>
            )}
          </div>

          {/* Steps */}
          {run.steps?.length > 0 && (
            <div style={{ marginBottom: 16 }}>
              <h4 style={{ margin: "0 0 12px", fontWeight: 700, color: "white" }}>
                Steps ({run.steps.length})
              </h4>
              <div
                style={{
                  overflowX: "auto",
                  borderRadius: 8,
                  border: "1px solid rgba(255,255,255,0.1)",
                }}
              >
                <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                  <thead>
                    <tr style={{ background: "rgba(0,0,0,0.25)" }}>
                      <th style={{ padding: 10, textAlign: "left", color: "white" }}>#</th>
                      <th style={{ padding: 10, textAlign: "left", color: "white" }}>Action</th>
                      <th style={{ padding: 10, textAlign: "left", color: "white" }}>Target</th>
                      <th style={{ padding: 10, textAlign: "left", color: "white" }}>Status</th>
                      <th style={{ padding: 10, textAlign: "left", color: "white" }}>Duration</th>
                    </tr>
                  </thead>
                  <tbody>
                    {run.steps.map((step, i) => (
                      <tr
                        key={i}
                        style={{ borderTop: "1px solid rgba(255,255,255,0.08)" }}
                      >
                        <td style={{ padding: 10, color: "white", opacity: 0.7 }}>
                          {step.index ?? i + 1}
                        </td>
                        <td style={{ padding: 10, color: "white", fontWeight: 600 }}>
                          {step.action || "-"}
                        </td>
                        <td style={{ padding: 10, color: "white", opacity: 0.8, maxWidth: 300, wordBreak: "break-all" }}>
                          {step.url || step.selector || step.value || "-"}
                        </td>
                        <td style={{ padding: 10, color: statusColor(step.status) }}>
                          {step.status || "-"}
                        </td>
                        <td style={{ padding: 10, color: "white", opacity: 0.7 }}>
                          {step.duration_ms ? `${step.duration_ms}ms` : "-"}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {/* Logs */}
          {run.logs?.length > 0 && (
            <div>
              <h4 style={{ margin: "0 0 12px", fontWeight: 700, color: "white" }}>
                Logs
              </h4>
              <pre
                style={{
                  margin: 0,
                  padding: 14,
                  borderRadius: 8,
                  background: "rgba(0,0,0,0.3)",
                  border: "1px solid rgba(255,255,255,0.1)",
                  color: "white",
                  fontSize: 11,
                  lineHeight: 1.4,
                  overflow: "auto",
                  maxHeight: 300,
                }}
              >
                {run.logs.join("\n")}
              </pre>
            </div>
          )}

          {/* Screenshot */}
          {run.screenshot_b64 && (
            <div style={{ marginTop: 16 }}>
              <h4 style={{ margin: "0 0 12px", fontWeight: 700, color: "white" }}>
                Screenshot
              </h4>
              <img
                src={
                  run.screenshot_b64.startsWith("data:image/")
                    ? run.screenshot_b64
                    : `data:image/png;base64,${run.screenshot_b64}`
                }
                alt="Screenshot"
                style={{
                  maxWidth: "100%",
                  borderRadius: 8,
                  border: "1px solid rgba(255,255,255,0.1)",
                }}
              />
            </div>
          )}
        </section>
      )}
    </div>
  );
}
