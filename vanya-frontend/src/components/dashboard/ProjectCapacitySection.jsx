import React from "react";
import { buildProjectCapacityRows, projectCapacityAccent } from "../../utils/dashboardHelpers.js";

export default function ProjectCapacitySection({ execStatus, execError, projects, t }) {
  const rows = buildProjectCapacityRows(execStatus);
  if (rows.length === 0) {
    if (execError) {
      return (
        <div className="card" style={{ padding: "16px 20px", marginBottom: 28, fontSize: 13, color: "var(--red-text)" }}>
          {execError}
        </div>
      );
    }
    return null;
  }

  return (
    <div className="card" style={{ padding: "20px 24px", marginBottom: 28 }}>
      <div className="section-title" style={{ marginBottom: 16 }}>{t("dashboard.project_capacity")}</div>
      <div style={{
        display: "grid",
        gridTemplateColumns: "repeat(auto-fill, minmax(220px, 1fr))",
        gap: 16,
      }}>
        {rows.map((row) => {
          const name = Array.isArray(projects)
            ? (projects.find((p) => p && p.id === row.projectId)?.name || "")
            : "";
          const accent = projectCapacityAccent(row.reserved, row.max);
          const pct = Math.min(100, row.max > 0 ? (row.reserved / row.max) * 100 : 0);
          return (
            <div
              key={row.projectId}
              style={{
                borderRadius: "var(--r-sm)",
                border: `1px solid ${accent.border}`,
                background: "var(--surface)",
                padding: "14px 16px",
                boxSizing: "border-box",
              }}
            >
              <div style={{ marginBottom: 10 }}>
                <div style={{ fontFamily: "monospace", fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>
                  {row.projectId}
                </div>
                {name && name !== row.projectId && (
                  <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 3 }}>{name}</div>
                )}
              </div>

              <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 6 }}>
                <strong style={{ color: "var(--text-3)", fontWeight: 500 }}>{t("dashboard.running")}:</strong>{" "}
                <span style={{ fontWeight: 600, color: accent.label }}>{row.reserved}</span>
                {" / "}
                <span style={{ color: "var(--text-3)" }}>{row.max}</span>
                <span style={{ color: "var(--text-3)", fontSize: 10, marginLeft: 4 }}>({t("dashboard.max")})</span>
              </div>
              <div style={{ height: 6, borderRadius: 3, background: "var(--border)", overflow: "hidden", marginBottom: 10 }}>
                <div style={{
                  width: `${pct}%`,
                  height: "100%",
                  borderRadius: 3,
                  background: accent.bar,
                  transition: "width 0.35s ease",
                }} />
              </div>
              <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 4 }}>
                <strong style={{ color: "var(--text-3)", fontWeight: 500 }}>{t("dashboard.queued")}:</strong>{" "}
                <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{row.queued}</span>
              </div>
              <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                <strong style={{ color: "var(--text-3)", fontWeight: 500 }}>{t("dashboard.reserved")}:</strong>{" "}
                <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{row.reserved}</span>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}
