// src/components/NavSidebar.jsx
import React from "react";
import { NavLink } from "react-router-dom";

const NAV_ITEMS = [
  { to: "/chat",      icon: "✦",  label: "AI Chat"     },
  { to: "/planner",   icon: "⚡", label: "Planner"     },
  { to: "/runs",      icon: "◈",  label: "Runs"        },
  { to: "/documents", icon: "⊟",  label: "Documents"   },
  { to: "/settings",  icon: "⊛",  label: "Settings", bottom: true },
];

const mainItems   = NAV_ITEMS.filter(i => !i.bottom);
const bottomItems = NAV_ITEMS.filter(i =>  i.bottom);

export default function NavSidebar() {
  return (
    <nav style={{
      width: "var(--nav-w)",
      minWidth: "var(--nav-w)",
      height: "100vh",
      background: "var(--nav-bg)",
      borderRight: "1px solid var(--nav-border)",
      display: "flex",
      flexDirection: "column",
      flexShrink: 0,
      overflow: "hidden",
    }}>
      {/* ── Brand ──────────────────────────────────────── */}
      <div style={{
        height: "var(--header-h)",
        minHeight: "var(--header-h)",
        display: "flex",
        alignItems: "center",
        padding: "0 18px",
        borderBottom: "1px solid var(--nav-border)",
        gap: 10,
        flexShrink: 0,
      }}>
        {/* Logo mark */}
        <div style={{
          width: 30,
          height: 30,
          borderRadius: 8,
          background: "var(--nav-accent)",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          fontSize: 15,
          fontWeight: 900,
          color: "#fff",
          letterSpacing: "-0.03em",
          flexShrink: 0,
          boxShadow: "0 2px 8px rgba(79,107,255,0.45)",
        }}>
          V
        </div>
        <div>
          <div style={{
            fontSize: 14,
            fontWeight: 800,
            color: "#fff",
            letterSpacing: "-0.02em",
            lineHeight: 1.2,
          }}>
            Vanya
          </div>
          <div style={{ fontSize: 10, color: "var(--nav-text)", lineHeight: 1.2, marginTop: 1 }}>
            QA Intelligence
          </div>
        </div>
      </div>

      {/* ── Main navigation ────────────────────────────── */}
      <div style={{ flex: 1, padding: "10px 8px", overflow: "auto" }}>
        <div style={{ marginBottom: 4 }}>
          <div style={{
            fontSize: 9,
            fontWeight: 800,
            textTransform: "uppercase",
            letterSpacing: "0.12em",
            color: "var(--nav-text)",
            padding: "6px 10px 4px",
          }}>
            Navigation
          </div>
        </div>

        {mainItems.map(({ to, icon, label }) => (
          <NavLink
            key={to}
            to={to}
            style={({ isActive }) => ({
              display: "flex",
              alignItems: "center",
              gap: 10,
              padding: "9px 10px",
              borderRadius: 8,
              marginBottom: 2,
              textDecoration: "none",
              background: isActive ? "var(--nav-item-active)" : "transparent",
              transition: "background 0.15s",
            })}
            onMouseEnter={e => {
              if (!e.currentTarget.dataset.active) {
                e.currentTarget.style.background = "var(--nav-item-hover)";
              }
            }}
            onMouseLeave={e => {
              if (!e.currentTarget.dataset.active) {
                e.currentTarget.style.background = e.currentTarget.getAttribute("aria-current")
                  ? "var(--nav-item-active)" : "transparent";
              }
            }}
          >
            {({ isActive }) => (
              <>
                <span style={{
                  width: 28,
                  height: 28,
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                  borderRadius: 7,
                  background: isActive ? "rgba(79,107,255,0.25)" : "rgba(255,255,255,0.05)",
                  fontSize: 14,
                  flexShrink: 0,
                  transition: "background 0.15s",
                }}>
                  {icon}
                </span>
                <span style={{
                  fontSize: 13,
                  fontWeight: isActive ? 700 : 500,
                  color: isActive ? "var(--nav-text-active)" : "var(--nav-text)",
                  transition: "color 0.15s",
                }}>
                  {label}
                </span>
                {isActive && (
                  <div style={{
                    marginLeft: "auto",
                    width: 4,
                    height: 4,
                    borderRadius: "50%",
                    background: "var(--nav-accent)",
                    flexShrink: 0,
                  }} />
                )}
              </>
            )}
          </NavLink>
        ))}
      </div>

      {/* ── Bottom items ────────────────────────────────── */}
      <div style={{ padding: "8px 8px 12px", borderTop: "1px solid var(--nav-border)" }}>
        {bottomItems.map(({ to, icon, label }) => (
          <NavLink
            key={to}
            to={to}
            style={({ isActive }) => ({
              display: "flex",
              alignItems: "center",
              gap: 10,
              padding: "9px 10px",
              borderRadius: 8,
              marginBottom: 2,
              textDecoration: "none",
              background: isActive ? "var(--nav-item-active)" : "transparent",
            })}
          >
            {({ isActive }) => (
              <>
                <span style={{
                  width: 28,
                  height: 28,
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                  borderRadius: 7,
                  background: isActive ? "rgba(79,107,255,0.25)" : "rgba(255,255,255,0.05)",
                  fontSize: 14,
                  flexShrink: 0,
                }}>
                  {icon}
                </span>
                <span style={{
                  fontSize: 13,
                  fontWeight: isActive ? 700 : 500,
                  color: isActive ? "var(--nav-text-active)" : "var(--nav-text)",
                }}>
                  {label}
                </span>
              </>
            )}
          </NavLink>
        ))}

        {/* Version tag */}
        <div style={{
          marginTop: 8,
          padding: "0 10px",
          fontSize: 10,
          color: "var(--nav-text)",
          opacity: 0.6,
        }}>
          v1.0 · QA Platform
        </div>
      </div>
    </nav>
  );
}
