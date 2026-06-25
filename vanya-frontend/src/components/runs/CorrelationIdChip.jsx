import React, { useState } from "react";
import { useLang } from "../../i18n/LangContext.jsx";

export default function CorrelationIdChip({ value }) {
  const { t } = useLang();
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
      <span className="badge badge-gray" style={{ fontSize: 10, opacity: 0.9 }}>{t("runs.trace.reference_label")}</span>
      <code style={{ fontSize: 11, color: "var(--text-2)" }} title={String(value)}>{String(value).slice(0, 12)}…</code>
      <button
        className="btn btn-secondary btn-sm"
        onClick={copy}
        style={{ fontSize: 11, padding: "4px 10px" }}
        title={t("runs.action_panel.copy_correlation_id")}
      >
        {copied ? t("common.copied") : t("common.copy")}
      </button>
    </span>
  );
}
