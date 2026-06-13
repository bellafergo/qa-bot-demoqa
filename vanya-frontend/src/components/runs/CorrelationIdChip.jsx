import React, { useState } from "react";

export default function CorrelationIdChip({ value }) {
  const [copied, setCopied] = useState(false);
  const copy = async () => {
    if (!value) return;
    try {
      await navigator.clipboard.writeText(String(value));
      setCopied(true);
      window.setTimeout(() => setCopied(false), 900);
    } catch {
      // ignore (clipboard may not be available)
    }
  };
  if (!value) return null;
  return (
    <span style={{ display: "inline-flex", alignItems: "center", gap: 8 }}>
      <span className="badge badge-gray" style={{ fontSize: 10, opacity: 0.9 }}>correlation</span>
      <code style={{ fontSize: 11, color: "var(--text-2)" }}>{value}</code>
      <button
        className="btn btn-secondary btn-sm"
        onClick={copy}
        style={{ fontSize: 11, padding: "4px 10px" }}
        title="Copy correlation_id"
      >
        {copied ? "Copied" : "Copy"}
      </button>
    </span>
  );
}
