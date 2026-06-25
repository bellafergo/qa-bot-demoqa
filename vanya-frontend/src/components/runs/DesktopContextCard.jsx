import React from "react";
import { useLang } from "../../i18n/LangContext";
import { inferRunType, inferBackend } from "../../utils/runHelpers.js";
import BackendBadge from "./BackendBadge.jsx";

export default function DesktopContextCard({ run }) {
  const { t } = useLang();
  if (!run) return null;
  const runType = inferRunType(run);
  if (runType !== "desktop") return null;

  const meta       = run.meta || {};
  const appPath    = meta.app_path    || run.app_path;
  const winLogin   = meta.win_login   || run.win_login;
  const winMain    = meta.win_main    || run.win_main;
  const window_    = winLogin || winMain;
  const backend    = inferBackend(run);
  const evidenceId = run.evidence_id;

  if (!appPath && !window_ && !backend && !evidenceId) return null;

  const rows = [
    appPath    && { label: t("runs.desktop.app_path"), value: appPath },
    window_    && { label: t("runs.desktop.window"),   value: window_ },
    backend    && { label: t("runs.desktop.backend"),  value: <BackendBadge backend={backend} /> },
    evidenceId && { label: t("planner.run.evidence_id"), value: <code style={{ fontSize: 11, color: "var(--text-2)" }} title={evidenceId}>{String(evidenceId).slice(0, 16)}…</code> },
  ].filter(Boolean);

  return (
    <div className="card" style={{ borderLeft: "3px solid var(--accent)" }}>
      <div className="section-title" style={{ marginBottom: 10 }}>⊞ {t("runs.desktop.context")}</div>
      <div style={{ display: "grid", gridTemplateColumns: "90px 1fr", gap: "6px 12px", alignItems: "center" }}>
        {rows.map(({ label, value }) => (
          <React.Fragment key={label}>
            <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{label}</div>
            <div style={{ fontSize: 12, color: "var(--text-2)", wordBreak: "break-all" }}>{value}</div>
          </React.Fragment>
        ))}
      </div>
    </div>
  );
}
