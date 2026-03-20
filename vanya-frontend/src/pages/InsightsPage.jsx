// src/pages/InsightsPage.jsx
/**
 * Insights — container page grouping all intelligence and analytics views.
 *
 * Tabs:
 *   Failures         → FailureIntelligencePage  (flaky tests, regressions, clusters)
 *   Coverage         → CoveragePage             (module coverage metrics)
 *   Risk             → RiskSelectionPage        (risk-based test selection)
 *   AI Recommendations → placeholder (phase 2)
 *
 * Legacy routes /failure-intel, /coverage, /risk-selection remain active
 * for backward compatibility but are no longer in the sidebar.
 */
import React, { useState } from "react";
import { useLang } from "../i18n/LangContext";
import FailureIntelligencePage from "./FailureIntelligencePage";
import CoveragePage            from "./CoveragePage";
import RiskSelectionPage       from "./RiskSelectionPage";

const TABS = [
  { key: "insights.tab.failures", icon: "⚠" },
  { key: "insights.tab.coverage", icon: "◐" },
  { key: "insights.tab.risk",     icon: "◈" },
  { key: "insights.tab.ai",       icon: "✦" },
];

export default function InsightsPage() {
  const { t } = useLang();
  const [tab, setTab] = useState(0);

  return (
    <div>
      {/* ── Header + tab bar ─────────────────────────────────────────────── */}
      <div style={{ padding: "24px 24px 0", background: "var(--bg)" }}>
        <h1 style={{ fontSize: 20, fontWeight: 800, margin: 0, color: "var(--text-1)" }}>
          {t("insights.title")}
        </h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 16px" }}>
          {t("insights.subtitle")}
        </p>

        <div style={{ display: "flex", gap: 4, flexWrap: "wrap" }}>
          {TABS.map((tb, i) => (
            <button
              key={tb.key}
              onClick={() => setTab(i)}
              style={{
                padding: "7px 16px",
                borderRadius: 8,
                border: "none",
                cursor: "pointer",
                fontSize: 13,
                fontWeight: tab === i ? 700 : 500,
                background: tab === i ? "var(--accent)" : "var(--surface)",
                color: tab === i ? "#fff" : "var(--text-2)",
                boxShadow: tab === i ? "0 2px 8px rgba(79,107,255,0.25)" : "none",
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

      {/* ── Tab content — embedded pages handle their own padding ─────────── */}
      {tab === 0 && <FailureIntelligencePage />}
      {tab === 1 && <CoveragePage />}
      {tab === 2 && <RiskSelectionPage />}
      {tab === 3 && <AIRecommendationsPlaceholder t={t} />}
    </div>
  );
}

// ── AI Recommendations placeholder ───────────────────────────────────────────

function AIRecommendationsPlaceholder({ t }) {
  return (
    <div className="page-wrap">
      <div className="card" style={{ textAlign: "center", padding: "56px 24px" }}>
        <div style={{
          width: 52, height: 52, borderRadius: 14,
          background: "rgba(79,107,255,0.1)",
          display: "flex", alignItems: "center", justifyContent: "center",
          fontSize: 24, margin: "0 auto 16px",
        }}>
          ✦
        </div>
        <div style={{ fontSize: 16, fontWeight: 700, color: "var(--text-1)", marginBottom: 8 }}>
          {t("insights.ai.title")}
        </div>
        <div style={{ fontSize: 13, color: "var(--text-3)", maxWidth: 380, margin: "0 auto", lineHeight: 1.6 }}>
          {t("insights.ai.desc")}
        </div>
        <div className="badge badge-gray" style={{ marginTop: 20, display: "inline-block" }}>
          Coming soon
        </div>
      </div>
    </div>
  );
}
