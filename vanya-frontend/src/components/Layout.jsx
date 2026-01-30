// src/components/Layout.jsx
import React from "react";
import { Outlet, useLocation } from "react-router-dom";
import NavSidebar from "./NavSidebar";

const ROUTE_TITLES = {
  "/chat": "Chat",
  "/planner": "Test Planner",
  "/documents": "Documents",
  "/runs": "Runs",
  "/settings": "Settings",
};

export default function Layout() {
  const location = useLocation();
  const pageTitle = ROUTE_TITLES[location.pathname] || "Vanya";

  return (
    <div style={{ display: "flex", height: "100vh", width: "100%" }}>
      {/* Navigation Sidebar */}
      <NavSidebar />

      {/* Main content area */}
      <div
        style={{
          flex: 1,
          display: "flex",
          flexDirection: "column",
          minWidth: 0,
        }}
      >
        {/* Header */}
        <header
          style={{
            height: 56,
            padding: "0 20px",
            borderBottom: "1px solid rgba(255,255,255,0.08)",
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            background: "rgba(0,0,0,0.15)",
          }}
        >
          <div style={{ display: "flex", alignItems: "center", gap: 12 }}>
            <span style={{ fontWeight: 800, fontSize: 18, color: "white" }}>
              Vanya
            </span>
            <span
              style={{
                fontWeight: 400,
                fontSize: 14,
                opacity: 0.7,
                color: "white",
              }}
            >
              | {pageTitle}
            </span>
          </div>

          <div style={{ fontSize: 12, opacity: 0.6, color: "white" }}>
            QA Intelligence Agent
          </div>
        </header>

        {/* Page content via Outlet */}
        <main style={{ flex: 1, overflow: "auto" }}>
          <Outlet />
        </main>
      </div>
    </div>
  );
}
