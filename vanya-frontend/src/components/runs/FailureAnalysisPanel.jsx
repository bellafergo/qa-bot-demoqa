import React from "react";
import { useLang } from "../../i18n/LangContext";

export default function FailureAnalysisPanel({ fa, style }) {
  const { t } = useLang();
  if (!fa) return null;
  const typeCls = (() => {
    const v = String(fa.failure_type || "").toLowerCase();
    if (v === "navigation_failed") return "badge badge-red";
    if (v === "unknown")           return "badge badge-gray";
    return "badge badge-orange";
  })();
  const confCls = (() => {
    const c = String(fa.confidence || "").toLowerCase();
    if (c === "high")   return "badge badge-green";
    if (c === "medium") return "badge badge-orange";
    return "badge badge-gray";
  })();
  return (
    <div className="card" style={style}>
      <div className="section-title" style={{ marginBottom: 12 }}>{t("runs.failure_analysis.title")}</div>
      <div style={{ display: "grid", gridTemplateColumns: "90px 1fr", gap: "8px 12px", alignItems: "center" }}>
        <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.type")}</div>
        <div><span className={typeCls}>{fa.failure_type || "—"}</span></div>
        <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.layer")}</div>
        <div style={{ fontSize: 12, color: "var(--text-2)" }}>{fa.layer || "—"}</div>
        {fa.target && (
          <>
            <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.target")}</div>
            <div><code style={{ fontSize: 11, color: "var(--text-1)" }}>{fa.target}</code></div>
          </>
        )}
        <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.confidence")}</div>
        <div><span className={confCls}>{fa.confidence || "—"}</span></div>
      </div>
    </div>
  );
}
