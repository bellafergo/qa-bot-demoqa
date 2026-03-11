// src/components/Layout.jsx
import React from "react";
import { Outlet, useLocation } from "react-router-dom";
import NavSidebar from "./NavSidebar";

const ROUTE_META = {
  // dashboard has its own hero header — suppress the generic bar
  "/dashboard": null,
  "/chat":      { title: "AI Chat",        subtitle: "Intelligent QA assistant" },
  "/planner":   { title: "Test Planner",   subtitle: "Natural-language test generation" },
  "/documents": { title: "Documents",      subtitle: "Upload and query test documents" },
  "/runs":      { title: "Execution Runs", subtitle: "Review evidence and test results" },
  "/settings":  { title: "Settings",       subtitle: "Configuration and preferences" },
};

export default function Layout() {
  const { pathname } = useLocation();
  // null → dashboard (or other pages with their own hero) — no header bar
  const meta = pathname in ROUTE_META ? ROUTE_META[pathname] : { title: "Vanya", subtitle: "" };
  const showHeader = meta !== null;

  return (
    <div style={{ display: "flex", height: "100vh", overflow: "hidden" }}>
      {/* ── Left navigation rail ────────────────────────── */}
      <NavSidebar />

      {/* ── Right: optional header + page content ─────────── */}
      <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden", minWidth: 0 }}>

        {/* Top header — hidden on pages that manage their own title (e.g. Dashboard) */}
        {showHeader && (
          <header style={{
            height: "var(--header-h)",
            minHeight: "var(--header-h)",
            background: "var(--surface)",
            borderBottom: "1px solid var(--border)",
            display: "flex",
            alignItems: "center",
            padding: "0 28px",
            gap: 16,
            flexShrink: 0,
            boxShadow: "0 1px 0 var(--border)",
          }}>
            <div style={{ flex: 1 }}>
              <div style={{
                fontSize: 15,
                fontWeight: 800,
                color: "var(--text)",
                letterSpacing: "-0.02em",
                lineHeight: 1.2,
              }}>
                {meta.title}
              </div>
              {meta.subtitle && (
                <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>
                  {meta.subtitle}
                </div>
              )}
            </div>

            {/* Live status indicator */}
            <div style={{
              display: "flex",
              alignItems: "center",
              gap: 6,
              fontSize: 11,
              color: "var(--text-3)",
              fontWeight: 500,
            }}>
              <span style={{
                width: 7,
                height: 7,
                borderRadius: "50%",
                background: "var(--green)",
                display: "inline-block",
                boxShadow: "0 0 0 2px var(--green-bg)",
                flexShrink: 0,
              }} />
              Live
            </div>
          </header>
        )}

        {/* Scrollable page content */}
        <main style={{ flex: 1, overflow: "auto", background: "var(--bg)" }}>
          <Outlet />
        </main>
      </div>
    </div>
  );
}
