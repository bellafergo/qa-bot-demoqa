// src/components/PromptDialog.jsx
import React, { useState } from "react";
import { useLang } from "../i18n/LangContext.jsx";

/** Pass a changing `key` when `defaultValue` should reset (e.g. per run id). */
export default function PromptDialog({
  open,
  title,
  label,
  defaultValue = "",
  submitLabel,
  onSubmit,
  onCancel,
  busy = false,
}) {
  const { t } = useLang();
  const [value, setValue] = useState(() => defaultValue ?? "");

  if (!open) return null;

  const handleSubmit = () => {
    const trimmed = String(value).trim();
    onSubmit(trimmed);
  };

  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex: 10040,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
      aria-labelledby="prompt-dialog-title"
    >
      <div className="card" style={{ width: "min(520px, 100%)", padding: 0, overflow: "hidden" }} onKeyDown={(e) => e.key === "Escape" && !busy && onCancel?.()}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="prompt-dialog-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {title}
          </div>
          {label ? (
            <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 12, marginBottom: 6 }}>
              {label}
            </label>
          ) : null}
          <input
            className="input"
            autoFocus
            value={value}
            onChange={(e) => setValue(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && !busy) {
                e.preventDefault();
                handleSubmit();
              }
            }}
            disabled={busy}
            style={{ width: "100%" }}
          />
        </div>
        <div style={{ padding: "14px 20px", display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onCancel} disabled={busy}>
            {t("common.cancel")}
          </button>
          <button type="button" className="btn btn-primary btn-sm" onClick={handleSubmit} disabled={busy}>
            {busy ? t("common.working") : submitLabel || t("common.confirm")}
          </button>
        </div>
      </div>
    </div>
  );
}
