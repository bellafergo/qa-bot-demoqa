// src/components/NavSidebar.jsx
import React from "react";
import { NavLink } from "react-router-dom";

const NAV_SECTIONS = [
  {
    label: "Core",
    items: [
      { to: "/dashboard",  icon: "⊞", label: "Dashboard"  },
      { to: "/catalog",    icon: "☰", label: "Catalog"    },
      { to: "/execution",  icon: "⚙", label: "Execution"  },
      { to: "/runs",       icon: "◈", label: "Runs & RCA" },
    ],
  },
  {
    label: "Intelligence",
    items: [
      { to: "/pr-analysis", icon: "◎", label: "PR Analysis" },
      { to: "/coverage",    icon: "◐", label: "Coverage"    },
      { to: "/drafts",      icon: "⊕", label: "Drafts"      },
    ],
  },
  {
    label: "Tools",
    items: [
      { to: "/chat",        icon: "✦", label: "AI Chat"     },
      { to: "/api-testing", icon: "⌥", label: "API Testing" },
      { to: "/planner",     icon: "⚡", label: "Planner"    },
      { to: "/documents",   icon: "⊟", label: "Documents"   },
    ],
  },
];

const BOTTOM_ITEMS = [
  { to: "/settings", icon: "⊛", label: "Settings" },
];

function NavItem({ to, icon, label }) {
  return (
    <NavLink
      key={to}
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
        {NAV_SECTIONS.map(section => (
          <div key={section.label} style={{ marginBottom: 8 }}>
            <div style={{
              fontSize: 9, fontWeight: 800, textTransform: "uppercase",
              letterSpacing: "0.12em", color: "var(--nav-text)",
              padding: "5px 10px 3px",
            }}>
              {section.label}
            </div>
            {section.items.map(item => <NavItem key={item.to} {...item} />)}
          </div>
        ))}
      </div>

      {/* Bottom */}
      <div style={{ padding: "8px 8px 12px", borderTop: "1px solid var(--nav-border)" }}>
        {BOTTOM_ITEMS.map(item => <NavItem key={item.to} {...item} />)}
        <div style={{ marginTop: 8, padding: "0 10px", fontSize: 10, color: "var(--nav-text)", opacity: 0.6 }}>
          v1.0 · QA Platform
        </div>
      </div>
    </nav>
  );
}
