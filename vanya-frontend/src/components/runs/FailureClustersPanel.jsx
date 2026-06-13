import React from "react";
import { useLang } from "../../i18n/LangContext";

export default function FailureClustersPanel({ clusters, loading }) {
  const { t } = useLang();
  if (loading) {
    return (
      <div className="card" style={{ marginBottom: 24 }}>
        <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.clusters.title")}</div>
        <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("runs.clusters.loading")}</div>
      </div>
    );
  }
  return (
    <div className="card" style={{ marginBottom: 24, padding: 0, overflow: "hidden" }}>
      <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", gap: 10 }}>
        <div className="section-title" style={{ margin: 0 }}>{t("runs.clusters.title")}</div>
        {clusters.length > 0 && <span className="badge badge-red">{clusters.length} cluster{clusters.length !== 1 ? "s" : ""}</span>}
      </div>
      {clusters.length === 0 ? (
        <div style={{ padding: "16px 20px", fontSize: 13, color: "var(--text-3)" }}>{t("runs.clusters.none")}</div>
      ) : (
        <table className="data-table">
          <thead>
            <tr>
              <th>{t("runs.clusters.col.type")}</th>
              <th>{t("runs.clusters.col.target")}</th>
              <th style={{ width: 80, textAlign: "right" }}>{t("runs.clusters.col.count")}</th>
            </tr>
          </thead>
          <tbody>
            {clusters.map((c) => {
              const typeCls = (() => {
                const v = String(c.failure_type || "").toLowerCase();
                if (v === "navigation_failed") return "badge badge-red";
                if (v === "unknown")           return "badge badge-gray";
                return "badge badge-orange";
              })();
              return (
                <tr key={c.cluster_id}>
                  <td><span className={typeCls}>{c.failure_type || "—"}</span></td>
                  <td style={{ fontSize: 12, color: "var(--text-2)", fontFamily: "monospace" }}>
                    {c.target || <span style={{ color: "var(--text-3)" }}>—</span>}
                  </td>
                  <td style={{ textAlign: "right", fontWeight: 600, fontSize: 14, color: "var(--text-1)" }}>{c.count}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      )}
    </div>
  );
}
