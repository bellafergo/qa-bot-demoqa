import React from "react";

export default function ApiStepEvidencePanel({ evidence, t }) {
  if (!evidence || typeof evidence !== "object") return null;
  const req = evidence.request;
  const res = evidence.response;
  const fail = evidence.failure;
  const truncNote = (part) =>
    part && part.truncated ? (
      <div style={{ fontSize: 11, color: "var(--orange-text)", marginBottom: 6 }}>
        {t("runs.api_evidence.truncated")}
        {part.original_size != null ? ` (${part.original_size} B)` : ""}
      </div>
    ) : null;
  const jsonPre = (labelKey, obj) => {
    if (obj == null) return null;
    let text;
    try {
      text = typeof obj === "string" ? obj : JSON.stringify(obj, null, 2);
    } catch {
      text = String(obj);
    }
    return (
      <div style={{ marginBottom: 14 }}>
        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
          {t(labelKey)}
        </div>
        <pre
          className="code-block"
          style={{
            margin: 0,
            maxHeight: 280,
            overflow: "auto",
            fontSize: 11,
            lineHeight: 1.45,
            whiteSpace: "pre-wrap",
            wordBreak: "break-word",
          }}
        >
          {text}
        </pre>
      </div>
    );
  };
  return (
    <div
      style={{
        padding: "12px 16px",
        background: "var(--bg-subtle, rgba(0,0,0,0.03))",
        borderTop: "1px dashed var(--border)",
      }}
    >
      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 8 }}>
        {t("runs.api_evidence.title")}
      </div>
      {req && (
        <>
          {truncNote(req)}
          {jsonPre("runs.api_evidence.request", req)}
        </>
      )}
      {res && (
        <>
          {truncNote(res)}
          {jsonPre("runs.api_evidence.response", res)}
        </>
      )}
      {fail && jsonPre("runs.api_evidence.failure", fail)}
    </div>
  );
}
