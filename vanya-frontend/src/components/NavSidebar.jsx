// src/components/NavSidebar.jsx
import React from "react";
import { NavLink } from "react-router-dom";
import { useLang } from "../i18n/LangContext";

// Route → translation key for label
const NAV_SECTIONS_DEF = [
  {
    labelKey: "nav.core",
    items: [
      { to: "/dashboard",  icon: "⊞", labelKey: "nav.dashboard"  },
      { to: "/catalog",    icon: "☰", labelKey: "nav.catalog"    },
      { to: "/execution",  icon: "⚙", labelKey: "nav.execution"  },
      { to: "/runs",       icon: "◈", labelKey: "nav.runs"       },
    ],
  },
  {
    labelKey: "nav.intelligence",
    items: [
      { to: "/pr-analysis",   icon: "◎", labelKey: "nav.pr_analysis"  },
      { to: "/coverage",      icon: "◐", labelKey: "nav.coverage"     },
      { to: "/drafts",        icon: "⊕", labelKey: "nav.drafts"       },
      { to: "/failure-intel", icon: "⚠", labelKey: "nav.failure_intel" },
    ],
  },
  {
    labelKey: "nav.tools",
    items: [
      { to: "/chat",        icon: "✦", labelKey: "nav.chat"        },
      { to: "/api-testing", icon: "⌥", labelKey: "nav.api_testing" },
      { to: "/planner",     icon: "⚡", labelKey: "nav.planner"    },
      { to: "/documents",   icon: "⊟", labelKey: "nav.documents"   },
    ],
  },
  {
    labelKey: "nav.platform",
    items: [
      { to: "/integrations", icon: "⊕", labelKey: "nav.integrations" },
    ],
  },
];

const BOTTOM_ITEMS_DEF = [
  { to: "/settings", icon: "⊛", labelKey: "nav.settings" },
];

function NavItem({ to, icon, label }) {
  return (
    <NavLink
      to={to}
      style={({ isActive }) => ({
        display: "flex",
        alignItems: "center",
        gap: 10,
        padding: "8px 10px",
        borderRadius: 8,
        marginBottom: 1,
        textDecoration: "none",
        background: isActive ? "var(--nav-item-active)" : "transparent",
        transition: "background 0.15s",
      })}
      onMouseEnter={e => { e.currentTarget.style.background = e.currentTarget.getAttribute("aria-current") ? "var(--nav-item-active)" : "var(--nav-item-hover)"; }}
      onMouseLeave={e => { e.currentTarget.style.background = e.currentTarget.getAttribute("aria-current") ? "var(--nav-item-active)" : "transparent"; }}
    >
      {({ isActive }) => (
        <>
          <span style={{
            width: 26, height: 26,
            display: "flex", alignItems: "center", justifyContent: "center",
            borderRadius: 6,
            background: isActive ? "rgba(79,107,255,0.25)" : "rgba(255,255,255,0.05)",
            fontSize: 13,
            flexShrink: 0,
            transition: "background 0.15s",
          }}>
            {icon}
          </span>
          <span style={{
            fontSize: 12,
            fontWeight: isActive ? 700 : 500,
            color: isActive ? "var(--nav-text-active)" : "var(--nav-text)",
            transition: "color 0.15s",
          }}>
            {label}
          </span>
          {isActive && (
            <div style={{
              marginLeft: "auto", width: 4, height: 4,
              borderRadius: "50%", background: "var(--nav-accent)", flexShrink: 0,
            }} />
          )}
        </>
      )}
    </NavLink>
  );
}

export default function NavSidebar() {
  const { lang, setLang, t } = useLang();

  return (
    <nav style={{
      width: "var(--nav-w)", minWidth: "var(--nav-w)",
      height: "100vh",
      background: "var(--nav-bg)",
      borderRight: "1px solid var(--nav-border)",
      display: "flex", flexDirection: "column",
      flexShrink: 0, overflow: "hidden",
    }}>
      {/* Brand */}
      <div style={{
        height: "var(--header-h)", minHeight: "var(--header-h)",
        display: "flex", alignItems: "center",
        padding: "0 18px", borderBottom: "1px solid var(--nav-border)",
        gap: 10, flexShrink: 0,
      }}>
        <div style={{
          width: 30, height: 30, borderRadius: 8,
          background: "var(--nav-accent)",
          display: "flex", alignItems: "center", justifyContent: "center",
          fontSize: 15, fontWeight: 900, color: "#fff",
          letterSpacing: "-0.03em", flexShrink: 0,
          boxShadow: "0 2px 8px rgba(79,107,255,0.45)",
        }}>V</div>
        <div>
          <div style={{ fontSize: 14, fontWeight: 800, color: "#fff", letterSpacing: "-0.02em", lineHeight: 1.2 }}>Vanya</div>
          <div style={{ fontSize: 10, color: "var(--nav-text)", lineHeight: 1.2, marginTop: 1 }}>QA Intelligence</div>
        </div>
      </div>

      {/* Sections */}
      <div style={{ flex: 1, padding: "8px 8px", overflow: "auto" }}>
        {NAV_SECTIONS_DEF.map(section => (
          <div key={section.labelKey} style={{ marginBottom: 8 }}>
            <div style={{
              fontSize: 9, fontWeight: 800, textTransform: "uppercase",
              letterSpacing: "0.12em", color: "var(--nav-text)",
              padding: "5px 10px 3px",
            }}>
              {t(section.labelKey)}
            </div>
            {section.items.map(item => (
              <NavItem key={item.to} to={item.to} icon={item.icon} label={t(item.labelKey)} />
            ))}
          </div>
        ))}
      </div>

      {/* Bottom — settings + language toggle */}
      <div style={{ padding: "8px 8px 12px", borderTop: "1px solid var(--nav-border)" }}>
        {BOTTOM_ITEMS_DEF.map(item => (
          <NavItem key={item.to} to={item.to} icon={item.icon} label={t(item.labelKey)} />
        ))}

        {/* Language toggle */}
        <div style={{
          display: "flex", alignItems: "center", gap: 4,
          padding: "6px 10px", marginTop: 4,
        }}>
          <span style={{ fontSize: 10, color: "var(--nav-text)", opacity: 0.7, flexShrink: 0 }}>
            {t("lang.label")}:
          </span>
          {["en", "es"].map(l => (
            <button
              key={l}
              onClick={() => setLang(l)}
              style={{
                fontSize: 10,
                fontWeight: lang === l ? 800 : 500,
                color: lang === l ? "var(--nav-text-active)" : "var(--nav-text)",
                background: lang === l ? "rgba(79,107,255,0.2)" : "transparent",
                border: "none", cursor: "pointer",
                padding: "2px 6px", borderRadius: 4,
                transition: "background 0.15s, color 0.15s",
              }}
            >
              {l.toUpperCase()}
            </button>
          ))}
        </div>

        <div style={{ marginTop: 2, padding: "0 10px", fontSize: 10, color: "var(--nav-text)", opacity: 0.6 }}>
          v1.0 · QA Platform
        </div>
      </div>
    </nav>
  );
}
