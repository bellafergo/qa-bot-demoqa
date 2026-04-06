// src/components/ConfirmDialog.jsx
import React from "react";
import { useLang } from "../i18n/LangContext.jsx";

export default function ConfirmDialog({
  open,
  title,
  description,
  confirmLabel,
  cancelLabel,
  onConfirm,
  onCancel,
  busy = false,
  danger = false,
}) {
  const { t } = useLang();
  if (!open) return null;
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
      aria-labelledby="confirm-dialog-title"
    >
      <div className="card" style={{ width: "min(520px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="confirm-dialog-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {title}
          </div>
          {description ? (
            <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginTop: 8 }}>{description}</div>
          ) : null}
        </div>
        <div style={{ padding: "14px 20px", display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onCancel} disabled={busy}>
            {cancelLabel || t("common.cancel")}
          </button>
          <button
            type="button"
            className={`btn btn-sm ${danger ? "btn-primary" : "btn-primary"}`}
            style={
              danger
                ? { background: "var(--red)", borderColor: "var(--red)", color: "#fff" }
                : undefined
            }
            onClick={onConfirm}
            disabled={busy}
          >
            {busy ? t("common.working") : confirmLabel || t("common.confirm")}
          </button>
        </div>
      </div>
    </div>
  );
}
