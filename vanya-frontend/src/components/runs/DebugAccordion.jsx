import React from "react";

export default function DebugAccordion({ detail }) {
  const errType = detail?.error_type || detail?.meta?.error_type;
  const stepIndex = detail?.step_index ?? detail?.meta?.step_index;
  const hint = detail?.hint ?? detail?.meta?.hint;
  const rawReason = detail?.reason || detail?.message || detail?.error_message || "";
  const rawMessage = rawReason || detail?.runner?.reason || "";
  const hasAny = Boolean(errType || stepIndex != null || hint || rawMessage);

  if (!hasAny) return null;

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
        <div className="section-title" style={{ margin: 0 }}>Debug</div>
      </div>
      <div style={{ padding: 16 }}>
        <details>
          <summary style={{ cursor: "pointer", fontSize: 13, color: "var(--text-2)" }}>
            Show error details
          </summary>
          <div style={{ marginTop: 12, display: "grid", gap: 10 }}>
            {errType && (
              <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  error_type
                </span>
                <span className="badge badge-orange">{errType}</span>
              </div>
            )}
            {stepIndex != null && (
              <div>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  step_index
                </span>{" "}
                <code style={{ fontSize: 12, color: "var(--text-2)" }}>{String(stepIndex)}</code>
              </div>
            )}
            {hint && (
              <div>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  hint
                </span>{" "}
                <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginTop: 4 }}>{hint}</div>
              </div>
            )}
            {rawMessage && (
              <div>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  raw reason/message
                </span>
                <pre className="code-block" style={{ marginTop: 6, maxHeight: 180, overflow: "auto", whiteSpace: "pre-wrap" }}>
                  {rawMessage}
                </pre>
              </div>
            )}
          </div>
        </details>
      </div>
    </div>
  );
}
