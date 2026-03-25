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
import { useNavigate } from "react-router-dom";
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
        <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>
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
      {tab === 3 && <AIRecommendationsTab />}
    </div>
  );
}

// ── AI Recommendations tab ────────────────────────────────────────────────────

const REC_CARDS = [
  {
    icon: "◈",
    titleKey: "insights.ai.rec1.title",
    descKey:  "insights.ai.rec1.desc",
    accentColor: "rgba(79,107,255,0.12)",
  },
  {
    icon: "◐",
    titleKey: "insights.ai.rec2.title",
    descKey:  "insights.ai.rec2.desc",
    accentColor: "rgba(16,185,129,0.10)",
  },
  {
    icon: "⚠",
    titleKey: "insights.ai.rec3.title",
    descKey:  "insights.ai.rec3.desc",
    accentColor: "rgba(245,158,11,0.10)",
  },
];

function AIRecommendationsTab() {
  const { t } = useLang();
  const navigate = useNavigate();

  return (
    <div className="page-wrap">

      {/* Header */}
      <div style={{ display: "flex", alignItems: "flex-start", gap: 16, marginBottom: 28, flexWrap: "wrap" }}>
        <div style={{
          width: 48, height: 48, borderRadius: 14, flexShrink: 0,
          background: "rgba(79,107,255,0.1)",
          display: "flex", alignItems: "center", justifyContent: "center", fontSize: 22,
        }}>
          ✦
        </div>
        <div>
          <div style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)", marginBottom: 4 }}>
            {t("insights.ai.title")}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-3)", lineHeight: 1.6, maxWidth: 520 }}>
            {t("insights.ai.desc")}
          </div>
          <span className="badge badge-gray" style={{ marginTop: 10, display: "inline-block", fontSize: 10 }}>
            {t("insights.ai.badge")}
          </span>
        </div>
      </div>

      {/* Strategic recommendation cards */}
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))", gap: 16, marginBottom: 28 }}>
        {REC_CARDS.map((card, i) => (
          <div
            key={i}
            className="card"
            style={{ background: card.accentColor, borderColor: "transparent" }}
          >
            <div style={{ fontSize: 20, marginBottom: 10 }}>{card.icon}</div>
            <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>
              {t(card.titleKey)}
            </div>
            <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
              {t(card.descKey)}
            </div>
          </div>
        ))}
      </div>

      {/* CTAs */}
      <div className="card" style={{ display: "flex", alignItems: "center", gap: 16, flexWrap: "wrap" }}>
        <div style={{ flex: 1, minWidth: 200 }}>
          <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text-1)", marginBottom: 4 }}>
            {t("insights.ai.cta_heading")}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)" }}>
            {t("insights.ai.cta_desc")}
          </div>
        </div>
        <div style={{ display: "flex", gap: 8, flexShrink: 0 }}>
          <button className="btn btn-primary" onClick={() => navigate("/generate")}>
            {t("insights.ai.cta_generate")}
          </button>
          <button className="btn btn-secondary" onClick={() => navigate("/catalog")}>
            {t("insights.ai.cta_catalog")}
          </button>
        </div>
      </div>

    </div>
  );
}
