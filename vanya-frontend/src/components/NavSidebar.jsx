// src/components/NavSidebar.jsx
import React from "react";
import { NavLink } from "react-router-dom";

const NAV_ITEMS = [
  { to: "/chat", label: "Chat", icon: "💬" },
  { to: "/planner", label: "Planner", icon: "📝" },
  { to: "/documents", label: "Documents", icon: "📄" },
  { to: "/runs", label: "Runs", icon: "▶️" },
  { to: "/settings", label: "Settings", icon: "⚙️" },
];

export default function NavSidebar() {
  return (
    <nav
      style={{
        width: 64,
        minWidth: 64,
        background: "rgba(0,0,0,0.25)",
        borderRight: "1px solid rgba(255,255,255,0.08)",
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        paddingTop: 12,
        gap: 6,
      }}
    >
      {/* Logo */}
      <div
        style={{
          width: 40,
          height: 40,
          borderRadius: 10,
          background: "linear-gradient(135deg, #4e6bff 0%, #7b5cff 100%)",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          fontWeight: 900,
          fontSize: 18,
          color: "white",
          marginBottom: 12,
        }}
        title="Vanya QA"
      >
        V
      </div>

      {/* Nav items */}
      {NAV_ITEMS.map((item) => (
        <NavLink
          key={item.to}
          to={item.to}
          style={({ isActive }) => ({
            width: 44,
            height: 44,
            borderRadius: 10,
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            fontSize: 18,
            textDecoration: "none",
            color: "white",
            background: isActive ? "rgba(78,107,255,0.25)" : "transparent",
            border: isActive
              ? "1px solid rgba(78,107,255,0.5)"
              : "1px solid transparent",
            transition: "all 150ms ease",
          })}
          title={item.label}
        >
          {item.icon}
        </NavLink>
      ))}
    </nav>
  );
}
