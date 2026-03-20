// src/components/NavSidebar.jsx
import React from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { useLang } from "../i18n/LangContext";

const NAV_SECTIONS_DEF = [
  {
    labelKey: "nav.core",
    items: [
      { to: "/dashboard", icon: "⊞", labelKey: "nav.dashboard" },
      { to: "/catalog",   icon: "☰", labelKey: "nav.catalog"   },
      { to: "/runs",      icon: "◈", labelKey: "nav.runs"      },
      { to: "/evidence",  icon: "⊟", labelKey: "nav.evidence"  },
    ],
  },
  {
    labelKey: "nav.intelligence",
    items: [
      { to: "/insights", icon: "◐", labelKey: "nav.insights" },
    ],
  },
  {
    labelKey: "nav.tools",
    items: [
      { to: "/chat", icon: "✦", labelKey: "nav.chat" },
    ],
  },
  {
    labelKey: "nav.platform",
    items: [
      { to: "/integrations", icon: "◇", labelKey: "nav.integrations" },
      { to: "/documents",    icon: "⊟", labelKey: "nav.documents"    },
      { to: "/settings",     icon: "⊛", labelKey: "nav.settings"     },
    ],
  },
];

const BOTTOM_ITEMS_DEF = [];

function NavItem({ to, icon, label }) {
  return (
    <NavLink
      to={to}
      style={({ isActive }) => ({
        display: "flex",
        alignItems: "center",
        gap: 10,
        padding: "7px 10px",
        borderRadius: 10,
        marginBottom: 2,
        textDecoration: "none",
        background: isActive
          ? "linear-gradient(135deg, rgba(79,107,255,0.22) 0%, rgba(17,197,245,0.10) 100%)"
          : "transparent",
        boxShadow: isActive ? "0 2px 8px rgba(79,107,255,0.12)" : "none",
        transition: "all 0.18s ease",
      })}
      onMouseEnter={e => {
        const isCurrent = !!e.currentTarget.getAttribute("aria-current");
        if (!isCurrent) e.currentTarget.style.background = "rgba(255,255,255,0.055)";
      }}
      onMouseLeave={e => {
        const isCurrent = !!e.currentTarget.getAttribute("aria-current");
        if (!isCurrent) e.currentTarget.style.background = "transparent";
      }}
    >
      {({ isActive }) => (
        <>
          <span style={{
            width: 28, height: 28,
            display: "flex", alignItems: "center", justifyContent: "center",
            borderRadius: 8,
            background: isActive
              ? "linear-gradient(135deg, rgba(79,107,255,0.35), rgba(17,197,245,0.20))"
              : "rgba(255,255,255,0.06)",
            fontSize: 13,
            flexShrink: 0,
            transition: "background 0.18s",
            boxShadow: isActive ? "0 1px 6px rgba(79,107,255,0.20)" : "none",
          }}>
            {icon}
          </span>
          <span style={{
            fontSize: 12,
            fontWeight: isActive ? 700 : 400,
            color: isActive ? "var(--nav-text-active)" : "var(--nav-text)",
            transition: "color 0.18s",
            letterSpacing: isActive ? "0.005em" : "0",
          }}>
            {label}
          </span>
          {isActive && (
            <div style={{
              marginLeft: "auto",
              width: 5, height: 5,
              borderRadius: "50%",
              background: "linear-gradient(135deg, #4f6bff, #11c5f5)",
              flexShrink: 0,
              boxShadow: "0 0 6px rgba(79,107,255,0.55)",
            }} />
          )}
        </>
      )}
    </NavLink>
  );
}

export default function NavSidebar() {
  const { lang, setLang, t } = useLang();
  const navigate = useNavigate();

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
          width: 30, height: 30, borderRadius: 9,
          background: "linear-gradient(135deg, #4f6bff 0%, #11c5f5 100%)",
          display: "flex", alignItems: "center", justifyContent: "center",
          fontSize: 15, fontWeight: 900, color: "#fff",
          letterSpacing: "-0.03em", flexShrink: 0,
          boxShadow: "0 3px 10px rgba(79,107,255,0.50)",
        }}>V</div>
        <div>
          <div style={{ fontSize: 14, fontWeight: 800, color: "#fff", letterSpacing: "-0.02em", lineHeight: 1.2 }}>Vanya</div>
          <div style={{ fontSize: 10, color: "rgba(180,195,220,0.45)", lineHeight: 1.2, marginTop: 1 }}>QA Intelligence</div>
        </div>
      </div>

      {/* Quick action — hero CTA */}
      <div style={{ padding: "10px 12px", borderBottom: "1px solid var(--nav-border)", flexShrink: 0 }}>
        <button
          onClick={() => navigate("/generate")}
          style={{
            display: "flex", alignItems: "center", justifyContent: "center", gap: 7,
            width: "100%", padding: "10px 14px",
            borderRadius: 10, border: "none", cursor: "pointer",
            background: "linear-gradient(135deg, #4f6bff 0%, #11c5f5 100%)",
            color: "#fff",
            fontSize: 12, fontWeight: 700,
            letterSpacing: "0.01em",
            boxShadow: "0 4px 14px rgba(79,107,255,0.40)",
            transition: "transform 0.18s ease, box-shadow 0.18s ease",
          }}
          onMouseEnter={e => {
            e.currentTarget.style.transform = "translateY(-1px)";
            e.currentTarget.style.boxShadow = "0 6px 20px rgba(79,107,255,0.52)";
          }}
          onMouseLeave={e => {
            e.currentTarget.style.transform = "translateY(0)";
            e.currentTarget.style.boxShadow = "0 4px 14px rgba(79,107,255,0.40)";
          }}
        >
          <span style={{ fontSize: 14, lineHeight: 1 }}>⊕</span>
          {t("nav.quick_generate")}
        </button>
      </div>

      {/* Sections */}
      <div style={{ flex: 1, padding: "8px 8px", overflow: "auto" }}>
        {NAV_SECTIONS_DEF.map(section => (
          <div key={section.labelKey} style={{ marginBottom: 10 }}>
            <div style={{
              fontSize: 9, fontWeight: 800, textTransform: "uppercase",
              letterSpacing: "0.14em", color: "rgba(180,195,220,0.35)",
              padding: "6px 10px 3px",
            }}>
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
          <span style={{ fontSize: 10, color: "rgba(180,195,220,0.40)", flexShrink: 0 }}>
            {t("lang.label")}:
          </span>
          {["en", "es"].map(l => (
            <button
              key={l}
              onClick={() => setLang(l)}
              style={{
                fontSize: 10,
                fontWeight: lang === l ? 800 : 500,
                color: lang === l ? "#fff" : "rgba(180,195,220,0.45)",
                background: lang === l ? "rgba(79,107,255,0.25)" : "transparent",
                border: "none", cursor: "pointer",
                padding: "2px 7px", borderRadius: 5,
                transition: "background 0.15s, color 0.15s",
              }}
            >
              {l.toUpperCase()}
            </button>
          ))}
        </div>

        <div style={{ marginTop: 2, padding: "0 10px", fontSize: 10, color: "rgba(180,195,220,0.28)" }}>
          v1.0 · QA Platform
        </div>
      </div>
    </nav>
  );
}
