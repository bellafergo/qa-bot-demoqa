// src/components/Layout.jsx
import React from "react";
import { Outlet, useLocation } from "react-router-dom";
import NavSidebar from "./NavSidebar";
import { useLang } from "../i18n/LangContext";

// null → page manages its own hero header (e.g. Dashboard)
const ROUTE_META = {
  "/dashboard":    null,
  "/chat":         { titleKey: "layout.chat.title",          subtitleKey: "layout.chat.subtitle"          },
  "/planner":      { titleKey: "layout.planner.title",       subtitleKey: "layout.planner.subtitle"       },
  "/documents":    { titleKey: "layout.documents.title",     subtitleKey: "layout.documents.subtitle"     },
  "/runs":         { titleKey: "layout.runs.title",          subtitleKey: "layout.runs.subtitle"          },
  "/settings":     { titleKey: "layout.settings.title",      subtitleKey: "layout.settings.subtitle"      },
  "/catalog":      { titleKey: "layout.catalog.title",       subtitleKey: "layout.catalog.subtitle"       },
  "/drafts":       { titleKey: "layout.drafts.title",        subtitleKey: "layout.drafts.subtitle"        },
  "/execution":    { titleKey: "layout.execution.title",     subtitleKey: "layout.execution.subtitle"     },
  "/pr-analysis":  { titleKey: "layout.pr_analysis.title",   subtitleKey: "layout.pr_analysis.subtitle"   },
  "/api-testing":  { titleKey: "layout.api_testing.title",   subtitleKey: "layout.api_testing.subtitle"   },
  "/coverage":     { titleKey: "layout.coverage.title",      subtitleKey: "layout.coverage.subtitle"      },
  "/integrations": { titleKey: "layout.integrations.title",  subtitleKey: "layout.integrations.subtitle"  },
};

export default function Layout() {
  const { pathname } = useLocation();
  const { t } = useLang();

  const meta = pathname in ROUTE_META ? ROUTE_META[pathname] : { titleKey: "nav.dashboard", subtitleKey: "" };
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
                {t(meta.titleKey)}
              </div>
              {meta.subtitleKey && (
                <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>
                  {t(meta.subtitleKey)}
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
              {t("common.live")}
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
