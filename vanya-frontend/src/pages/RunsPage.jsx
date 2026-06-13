// src/pages/RunsPage.jsx
/**
 * Runs & RCA — Evidence Lookup + Run History with root cause and business risk analysis.
 * GET /runs/{run_id} | GET /test-runs | POST /rca/analyze | POST /business-risk/analyze
 */
import React, { useState, useEffect } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { useLang } from "../i18n/LangContext";
import RunsAnalyticsKpi from "../components/runs/RunsAnalyticsKpi.jsx";
import RunHistoryTab from "./runs/RunHistoryTab.jsx";
import EvidenceLookupTab from "./runs/EvidenceLookupTab.jsx";

// Tab identifiers — rendered via t() in the component
const TAB_KEYS = ["runs.tab.history", "runs.tab.lookup"];

export default function RunsPage() {
  const { t } = useLang();
  const location = useLocation();
  const navigate  = useNavigate();
  const navState  = location.state || {};
  const [activeTab, setActiveTab] = useState(navState.tab ?? 0);
  const [initialRunId] = useState(navState.run_id || null);

  // Clear navigation state from history so back/forward doesn't re-trigger auto-open
  useEffect(() => {
    if (navState.run_id || navState.tab != null) {
      navigate(location.pathname, { replace: true, state: null });
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <div className="page-wrap">

      {/* Page header */}
      <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", marginBottom: 20, flexWrap: "wrap", gap: 12 }}>
        <div>
          <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>{t("runs.page.title")}</h1>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 0" }}>{t("runs.page.subtitle")}</p>
        </div>
        <button
          className="btn btn-secondary btn-sm"
          onClick={() => navigate("/batch")}
          title={t("runs.page.batch_tip")}
          style={{ alignSelf: "flex-start" }}
        >
          {t("runs.page.batch_link")}
        </button>
      </div>

      <RunsAnalyticsKpi />

      {/* Tab bar */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "2px solid var(--border)", paddingBottom: 0 }}>
        {TAB_KEYS.map((key, i) => (
          <button
            key={i}
            onClick={() => setActiveTab(i)}
            style={{
              padding: "8px 16px",
              background: activeTab === i ? "var(--accent-light)" : "transparent",
              border: "none",
              borderBottom: activeTab === i ? "2px solid var(--accent-border)" : "2px solid transparent",
              marginBottom: -2,
              fontWeight: activeTab === i ? 500 : 400,
              fontSize: 13,
              color: activeTab === i ? "var(--accent)" : "var(--text-3)",
              cursor: "pointer",
              borderRadius: "8px 8px 0 0",
              transition: "background 0.15s, color 0.15s",
            }}
          >
            {t(key)}
          </button>
        ))}
      </div>

      {activeTab === 0 && <RunHistoryTab initialRunId={initialRunId} />}
      {activeTab === 1 && <EvidenceLookupTab />}
    </div>
  );
}
