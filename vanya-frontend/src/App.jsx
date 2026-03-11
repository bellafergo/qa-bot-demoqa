// src/App.jsx
/**
 * App - Router shell with Layout
 *
 * Routes:
 * - /           → /dashboard (default)
 * - /dashboard  → DashboardPage (QA Intelligence home)
 * - /chat       → ChatPage
 * - /planner    → PlannerPage
 * - /documents  → DocumentsPage
 * - /runs       → RunsPage
 * - /settings   → SettingsPage
 */
import React from "react";
import { Routes, Route, Navigate } from "react-router-dom";
import "./App.css";

import Layout from "./components/Layout";
import DashboardPage from "./pages/DashboardPage";
import ChatPage from "./pages/ChatPage";
import PlannerPage from "./pages/PlannerPage";
import DocumentsPage from "./pages/DocumentsPage";
import RunsPage from "./pages/RunsPage";
import SettingsPage from "./pages/SettingsPage";

/**
 * ErrorBoundary - prevents white screen on crash
 */
class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, err: null };
  }
  static getDerivedStateFromError(error) {
    return { hasError: true, err: error };
  }
  componentDidCatch(error, info) {
    // eslint-disable-next-line no-console
    console.error("UI crashed:", error, info);
  }
  render() {
    if (this.state.hasError) {
      const msg = String(this.state.err?.message || this.state.err || "Error");
      return (
        <div style={{ padding: 32, maxWidth: 600, margin: "0 auto" }}>
          <div className="card" style={{ borderColor: "var(--red-border)" }}>
            <div style={{ fontWeight: 800, fontSize: 16, marginBottom: 8, color: "var(--red-text)" }}>
              UI Crashed
            </div>
            <div style={{ fontSize: 13, color: "var(--text-2)", whiteSpace: "pre-wrap", marginBottom: 12 }}>
              {msg}
            </div>
            <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 14 }}>
              Open DevTools → Console for the full stack trace.
            </div>
            <button
              className="btn btn-secondary btn-sm"
              onClick={() => window.location.reload()}
            >
              Reload Page
            </button>
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
        {/* Layout wrapper with nested routes */}
        <Route path="/" element={<Layout />}>
          {/* Default → dashboard */}
          <Route index element={<Navigate to="/dashboard" replace />} />

          {/* Main routes */}
          <Route path="dashboard" element={<DashboardPage />} />
          <Route path="chat"      element={<ChatPage />} />
          <Route path="planner"   element={<PlannerPage />} />
          <Route path="documents" element={<DocumentsPage />} />
          <Route path="runs"      element={<RunsPage />} />
          <Route path="settings"  element={<SettingsPage />} />

          {/* Catch-all */}
          <Route path="*" element={<Navigate to="/dashboard" replace />} />
        </Route>
      </Routes>
    </ErrorBoundary>
  );
}
