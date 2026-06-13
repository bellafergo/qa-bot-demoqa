import React from "react";
import { useLang } from "../../i18n/LangContext";

export default function ConfirmModal({
  open,
  title,
  description,
  busy = false,
  error = "",
  confirmLabel,
  cancelLabel,
  onConfirm,
  onCancel,
  zIndex = 9999,
}) {
  const { t } = useLang();
  if (!open) return null;
  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
    >
      <div className="card" style={{ width: "min(680px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
          <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>{title}</div>
          <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>{description}</div>
        </div>
        <div style={{ padding: "14px 20px" }}>
          {error && (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
              {error}
            </div>
          )}
          <div style={{ display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
            <button
              className="btn btn-secondary btn-sm"
              onClick={onCancel}
              disabled={busy}
            >
              {cancelLabel || t("common.cancel")}
            </button>
            <button className="btn btn-primary btn-sm" onClick={onConfirm} disabled={busy}>
              {busy ? t("common.working") : confirmLabel || t("common.confirm")}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
