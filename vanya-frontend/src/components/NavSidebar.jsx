// src/components/NavSidebar.jsx
import React from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { useLang } from "../i18n/LangContext";
import { useAuth } from "../auth/AuthContext.jsx";
import { useRbac } from "../auth/RbacContext.jsx";
import ProjectSwitcher from "./ProjectSwitcher";
import { SidebarBrand } from "../ui";

const NAV_SECTIONS_DEF = [
  {
    labelKey: "nav.operation",
    items: [
      { to: "/dashboard", icon: "⊞", labelKey: "nav.dashboard", permission: "VIEW_DASHBOARD" },
      { to: "/catalog",   icon: "☰", labelKey: "nav.catalog", permission: "VIEW_DASHBOARD" },
      { to: "/runs",      icon: "◈", labelKey: "nav.runs", permission: "VIEW_DASHBOARD" },
      { to: "/batch",     icon: "▶", labelKey: "nav.batch", permission: "VIEW_DASHBOARD" },
      { to: "/evidence",  icon: "⊟", labelKey: "nav.evidence", permission: "VIEW_DASHBOARD" },
    ],
  },
  {
    labelKey: "nav.intelligence",
    items: [
      { to: "/insights", icon: "◐", labelKey: "nav.insights", permission: "VIEW_DASHBOARD" },
      { to: "/incidents", icon: "⚡", labelKey: "nav.incidents", permission: "VIEW_INCIDENTS" },
      { to: "/pr-analysis", icon: "⎇", labelKey: "nav.pr_analysis", permission: "VIEW_DASHBOARD" },
      { to: "/knowledge", icon: "◫", labelKey: "nav.knowledge", permission: "VIEW_DASHBOARD" },
    ],
  },
  {
    labelKey: "nav.automation",
    items: [
      { to: "/browser-watch", icon: "◎", labelKey: "nav.browser_watch", permission: "VIEW_DASHBOARD" },
      { to: "/local-agents", icon: "⎔", labelKey: "nav.local_agents", permission: "VIEW_DASHBOARD" },
    ],
  },
  {
    labelKey: "nav.platform",
    items: [
      { to: "/projects",     icon: "▤", labelKey: "nav.projects", permission: "VIEW_DASHBOARD" },
      { to: "/integrations", icon: "◇", labelKey: "nav.integrations", permission: "VIEW_DASHBOARD" },
      { to: "/chat",         icon: "◉", labelKey: "nav.chat", permission: "VIEW_DASHBOARD" },
      { to: "/settings",     icon: "⊛", labelKey: "nav.settings", permission: "VIEW_DASHBOARD" },
    ],
  },
];

const BOTTOM_ITEMS_DEF = [];

function NavItem({ to, icon, label }) {
  return (
    <NavLink
      to={to}
      className={({ isActive }) =>
        `nav-sidebar-link${isActive ? " nav-sidebar-link--active" : ""}`
      }
    >
      <span className="nav-sidebar-link-icon">{icon}</span>
      <span className="nav-sidebar-link-text">{label}</span>
    </NavLink>
  );
}

export default function NavSidebar() {
  const { lang, setLang, t } = useLang();
  const { signOut, user } = useAuth();
  const { hasPermission } = useRbac();
  const navigate = useNavigate();

  const visibleSections = NAV_SECTIONS_DEF.map((section) => ({
    ...section,
    items: section.items.filter((item) => hasPermission(item.permission)),
  })).filter((section) => section.items.length > 0);

  return (
    <nav style={{
      width: "var(--nav-w)", minWidth: "var(--nav-w)",
      height: "100vh",
      background: "var(--nav-bg)",
      borderRight: "1px solid var(--nav-border)",
      display: "flex", flexDirection: "column",
      flexShrink: 0, overflow: "hidden",
    }}>
      {/* Brand — logos: /public/logo/zuperio-*.svg */}
      <div style={{
        height: "var(--header-h)", minHeight: "var(--header-h)",
        display: "flex", alignItems: "center",
        padding: "0 16px 0 18px", borderBottom: "1px solid var(--nav-border)",
        gap: 12, flexShrink: 0,
      }}>
        <SidebarBrand productName="Vanya" productTagline="QA Intelligence" />
      </div>

      {/* Current project scope */}
      <div style={{ padding: "10px 14px", borderBottom: "1px solid var(--nav-border)", flexShrink: 0 }}>
        <ProjectSwitcher />
      </div>

      {/* Quick action */}
      <div style={{ padding: "12px 14px", borderBottom: "1px solid var(--nav-border)", flexShrink: 0 }}>
        <button
          type="button"
          className="nav-sidebar-cta nav-sidebar-cta--primary"
          onClick={() => navigate("/generate")}
        >
          <span style={{ fontSize: 14, lineHeight: 1 }}>⊕</span>
          {t("nav.quick_generate")}
        </button>
      </div>

      {/* Sections */}
      <div style={{ flex: 1, padding: "10px 10px", overflow: "auto" }}>
        {visibleSections.map(section => (
          <div key={section.labelKey} style={{ marginBottom: 14 }}>
            <div className="nav-sidebar-section-label">
              {t(section.labelKey)}
            </div>
            {section.items.map(item => (
              <NavItem key={item.to} to={item.to} icon={item.icon} label={t(item.labelKey)} />
            ))}
          </div>
        ))}
      </div>

      {/* Bottom — language toggle */}
      <div style={{ padding: "8px 8px 14px", borderTop: "1px solid var(--nav-border)" }}>
        {BOTTOM_ITEMS_DEF.map(item => (
          <NavItem key={item.to} to={item.to} icon={item.icon} label={t(item.labelKey)} />
        ))}

        <div style={{
          display: "flex", alignItems: "center", gap: 4,
          padding: "6px 10px", marginTop: 4,
        }}>
          <span style={{ fontSize: 10, color: "rgba(148, 163, 184, 0.55)", flexShrink: 0 }}>
            {t("lang.label")}:
          </span>
          {["en", "es"].map(l => (
            <button
              key={l}
              type="button"
              onClick={() => setLang(l)}
              style={{
                fontSize: 10,
                fontWeight: lang === l ? 500 : 400,
                color: lang === l ? "var(--nav-text-active)" : "rgba(148, 163, 184, 0.65)",
                background: lang === l ? "rgba(255,255,255,0.06)" : "transparent",
                border: "none", cursor: "pointer",
                padding: "2px 7px", borderRadius: 6,
                transition: "background 0.15s, color 0.15s",
              }}
            >
              {l.toUpperCase()}
            </button>
          ))}
        </div>

        <button
          type="button"
          className="btn btn-secondary btn-sm"
          style={{ width: "calc(100% - 20px)", margin: "10px 10px 6px", justifyContent: "center" }}
          onClick={() => signOut()}
        >
          {t("auth.logout")}
        </button>
        {user?.email ? (
          <div style={{ padding: "0 12px 6px", fontSize: 10, color: "rgba(148, 163, 184, 0.55)", wordBreak: "break-all" }}>
            {user.email}
          </div>
        ) : null}
        <div style={{ marginTop: 2, padding: "0 10px", fontSize: 10, color: "rgba(148, 163, 184, 0.4)" }}>
          {t("nav.footer_tagline")}
        </div>
      </div>
    </nav>
  );
}
