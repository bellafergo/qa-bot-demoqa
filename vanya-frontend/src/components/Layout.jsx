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
  "/batch":        { titleKey: "layout.batch.title",         subtitleKey: "layout.batch.subtitle"         },
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
          <header className="layout-header">
            <div style={{ flex: 1 }}>
              <div className="layout-header-title">
                {t(meta.titleKey)}
              </div>
              {meta.subtitleKey && (
                <div className="layout-header-sub">
                  {t(meta.subtitleKey)}
                </div>
              )}
            </div>

            <div className="layout-live-pill" role="status">
              <span className="layout-live-dot" aria-hidden />
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
