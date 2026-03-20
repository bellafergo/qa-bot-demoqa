// src/pages/GeneratePage.jsx
/**
 * Generate Tests — container page grouping all test-generation sources.
 *
 * Tabs (phase 1 — existing pages embedded; phase 2 will integrate inline):
 *   From Description  → PlannerPage    (natural-language → steps)
 *   From URL          → DraftsPage     (app explorer / URL crawl)
 *   From PR           → PRAnalysisPage (PR diff → test suggestions)
 *   From API          → ApiTestingPage (OpenAPI spec → tests)
 *
 * Legacy routes /planner, /drafts, /pr-analysis, /api-testing remain
 * active for backward compatibility but are no longer in the sidebar.
 */
import React, { useState } from "react";
import { useLang } from "../i18n/LangContext";
import PlannerPage    from "./PlannerPage";
import DraftsPage     from "./DraftsPage";
import PRAnalysisPage from "./PRAnalysisPage";
import ApiTestingPage from "./ApiTestingPage";

const TABS = [
  { key: "gen.tab.description", icon: "⚡" },
  { key: "gen.tab.url",         icon: "⊞" },
  { key: "gen.tab.pr",          icon: "◎" },
  { key: "gen.tab.api",         icon: "⌥" },
];

export default function GeneratePage() {
  const { t } = useLang();
  const [tab, setTab] = useState(0);

  return (
    <div>
      {/* ── Header + tab bar ─────────────────────────────────────────────── */}
      <div style={{ padding: "24px 24px 0", background: "var(--bg)" }}>
        <h1 style={{ fontSize: 20, fontWeight: 800, margin: 0, color: "var(--text-1)" }}>
          {t("gen.title")}
        </h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 16px" }}>
          {t("gen.subtitle")}
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
      {tab === 0 && <PlannerPage />}
      {tab === 1 && <DraftsPage />}
      {tab === 2 && <PRAnalysisPage />}
      {tab === 3 && <ApiTestingPage />}
    </div>
  );
}
