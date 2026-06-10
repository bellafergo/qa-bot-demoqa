import React from "react";

export default function RoleCard({ role }) {
  if (!role) return null;

  return (
    <li
      style={{
        marginBottom: 8,
        padding: "10px 12px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
      }}
    >
      <div style={{ fontWeight: 600, color: "var(--text-1)" }}>{role.displayName || role.role_name}</div>
      {role.description ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4 }}>{role.description}</div>
      ) : null}
    </li>
  );
}
