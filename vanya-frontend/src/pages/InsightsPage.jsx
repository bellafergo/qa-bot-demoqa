// src/pages/InsightsPage.jsx
/**
 * Insights — single analytics area grouping all intelligence views.
 *
 * Tabs:
 *   Failures         → FailureIntelligencePage  (embedded, no own header)
 *   Coverage         → CoveragePage             (module coverage metrics)
 *   Risk             → RiskSelectionPage        (embedded, no own header)
 *   AI Recommendations → placeholder with strategic orientation
 *
 * Legacy routes /failure-intel, /coverage, /risk-selection remain active
 * in App.jsx for backward compatibility.
 */
import React, { useState } from "react";
import { useLocation } from "react-router-dom";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import FailureIntelligencePage from "./FailureIntelligencePage";
import CoveragePage            from "./CoveragePage";
import RiskSelectionPage       from "./RiskSelectionPage";

const TABS = [
  { key: "insights.tab.failures", icon: "⚠" },
  { key: "insights.tab.coverage", icon: "◐" },
  { key: "insights.tab.risk",     icon: "◈" },
];

const RISK_TAB_INDEX = 2;

export default function InsightsPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const location = useLocation();
  const navTab = location.state?.tab === "risk" ? RISK_TAB_INDEX : null;
  const [userTab, setUserTab] = useState(null);
  const tab = userTab ?? navTab ?? 0;

  return (
    <div>
      {/* ── Header + tab bar ─────────────────────────────────────────────── */}
      <div style={{ padding: "24px 24px 0", background: "var(--bg)" }}>
        <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>
          {t("insights.title")}
        </h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 16px" }}>
          {t("insights.subtitle")}
        </p>
        {currentProject && (
          <div style={{ fontSize: 12, color: "var(--text-2)", margin: "-8px 0 12px", display: "flex", alignItems: "center", gap: 8 }}>
            <span aria-hidden style={{ width: 8, height: 8, borderRadius: "50%", background: currentProject.color || "var(--accent)" }} />
            <span>{t("insights.page_scope", { name: currentProject.name })}</span>
          </div>
        )}

        <div style={{ display: "flex", gap: 4, flexWrap: "wrap" }}>
          {TABS.map((tb, i) => (
            <button
              key={tb.key}
              onClick={() => setUserTab(i)}
              style={{
                padding: "7px 16px",
                borderRadius: 8,
                border: tab === i ? "1px solid var(--accent-border)" : "1px solid var(--border)",
                cursor: "pointer",
                fontSize: 13,
                fontWeight: 500,
                background: tab === i ? "var(--accent-light)" : "var(--surface)",
                color: tab === i ? "var(--accent)" : "var(--text-3)",
                boxShadow: tab === i ? "var(--shadow-1)" : "none",
                transition: "all 0.15s",
                display: "flex",
                alignItems: "center",
                gap: 6,
              }}
            >
              <span style={{ fontSize: 11 }}>{tb.icon}</span>
              {t(tb.key)}
            </button>
          ))}
        </div>

        <div style={{ height: 1, background: "var(--border)", margin: "12px -24px 0" }} />
      </div>

      {/* ── Tab content ──────────────────────────────────────────────────── */}
      {tab === 0 && <FailureIntelligencePage embedded />}
      {tab === 1 && <CoveragePage />}
      {tab === 2 && <RiskSelectionPage embedded />}
    </div>
  );
}
