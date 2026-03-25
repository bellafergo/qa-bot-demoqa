// src/App.jsx
import React from "react";
import { Routes, Route, Navigate } from "react-router-dom";
import "./App.css";

import Layout         from "./components/Layout";
import DashboardPage  from "./pages/DashboardPage";
import ChatPage       from "./pages/ChatPage";
import PlannerPage    from "./pages/PlannerPage";
import DocumentsPage  from "./pages/DocumentsPage";
import RunsPage       from "./pages/RunsPage";
import SettingsPage   from "./pages/SettingsPage";
import CatalogPage    from "./pages/CatalogPage";
import DraftsPage     from "./pages/DraftsPage";
import ExecutionPage  from "./pages/ExecutionPage";
import PRAnalysisPage from "./pages/PRAnalysisPage";
import ApiTestingPage from "./pages/ApiTestingPage";
import CoveragePage        from "./pages/CoveragePage";
import IntegrationsPage         from "./pages/IntegrationsPage";
import FailureIntelligencePage  from "./pages/FailureIntelligencePage";
import RiskSelectionPage        from "./pages/RiskSelectionPage";
import EvidencePage            from "./pages/EvidencePage";
import GeneratePage            from "./pages/GeneratePage";
import InsightsPage            from "./pages/InsightsPage";

class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, err: null };
  }
  static getDerivedStateFromError(error) { return { hasError: true, err: error }; }
  componentDidCatch(error, info) { console.error("UI crashed:", error, info); }
  render() {
    if (this.state.hasError) {
      const msg = String(this.state.err?.message || this.state.err || "Error");
      return (
        <div style={{ padding: 32, maxWidth: 600, margin: "0 auto" }}>
          <div className="card" style={{ borderColor: "var(--red-border)" }}>
            <div style={{ fontWeight: 600, fontSize: 16, marginBottom: 8, color: "var(--red-text)" }}>UI Crashed</div>
            <div style={{ fontSize: 13, color: "var(--text-2)", whiteSpace: "pre-wrap", marginBottom: 12 }}>{msg}</div>
            <button className="btn btn-secondary btn-sm" onClick={() => window.location.reload()}>Reload Page</button>
          </div>
        </div>
      );
    }
    return this.props.children;
  }
}

export default function App() {
  return (
    <ErrorBoundary>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<Navigate to="/dashboard" replace />} />

          {/* ── Primary nav routes ───────────────────────────────────── */}
          <Route path="dashboard"  element={<DashboardPage />} />
          <Route path="catalog"    element={<CatalogPage />} />
          <Route path="runs"       element={<RunsPage />} />
          <Route path="evidence"   element={<EvidencePage />} />
          <Route path="generate"   element={<GeneratePage />} />
          <Route path="insights"   element={<InsightsPage />} />
          <Route path="chat"       element={<ChatPage />} />
          <Route path="integrations" element={<IntegrationsPage />} />
          <Route path="documents"  element={<DocumentsPage />} />
          <Route path="settings"   element={<SettingsPage />} />

          {/* ── Legacy routes — active but not in sidebar ────────────── */}
          <Route path="execution"      element={<ExecutionPage />} />
          <Route path="pr-analysis"    element={<PRAnalysisPage />} />
          <Route path="coverage"       element={<CoveragePage />} />
          <Route path="drafts"         element={<DraftsPage />} />
          <Route path="failure-intel"  element={<FailureIntelligencePage />} />
          <Route path="risk-selection" element={<RiskSelectionPage />} />
          <Route path="api-testing"    element={<ApiTestingPage />} />
          <Route path="planner"        element={<PlannerPage />} />

          <Route path="*" element={<Navigate to="/dashboard" replace />} />
        </Route>
      </Routes>
    </ErrorBoundary>
  );
}
