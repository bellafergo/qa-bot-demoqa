// src/App.jsx
/**
 * App - Router shell with Layout
 *
 * Routes:
 * - /chat      → ChatPage (existing chat functionality)
 * - /planner   → PlannerPage (NL test planner)
 * - /documents → DocumentsPage (document upload/query)
 * - /runs      → RunsPage (execution evidence)
 * - /settings  → SettingsPage (placeholder)
 */
import React from "react";
import { Routes, Route, Navigate } from "react-router-dom";
import "./App.css";

import Layout from "./components/Layout";
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
        <div style={{ padding: 18, color: "white" }}>
          <div
            style={{
              padding: "10px 12px",
              borderRadius: 12,
              background: "rgba(255,0,0,0.10)",
              border: "1px solid rgba(255,0,0,0.25)",
              maxWidth: 900,
            }}
          >
            <div style={{ fontWeight: 900, marginBottom: 6 }}>UI Crashed</div>
            <div style={{ opacity: 0.9, whiteSpace: "pre-wrap" }}>{msg}</div>
            <div style={{ marginTop: 10, fontSize: 12, opacity: 0.75 }}>
              Open DevTools → Console to see the stack trace.
            </div>
            <button
              onClick={() => window.location.reload()}
              style={{
                marginTop: 12,
                padding: "8px 16px",
                borderRadius: 8,
                border: "1px solid rgba(255,255,255,0.2)",
                background: "rgba(255,255,255,0.1)",
                color: "white",
                cursor: "pointer",
              }}
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
          {/* Default redirect to /chat */}
          <Route index element={<Navigate to="/chat" replace />} />

          {/* Main routes */}
          <Route path="chat" element={<ChatPage />} />
          <Route path="planner" element={<PlannerPage />} />
          <Route path="documents" element={<DocumentsPage />} />
          <Route path="runs" element={<RunsPage />} />
          <Route path="settings" element={<SettingsPage />} />

          {/* Catch-all redirect */}
          <Route path="*" element={<Navigate to="/chat" replace />} />
        </Route>
      </Routes>
    </ErrorBoundary>
  );
}
