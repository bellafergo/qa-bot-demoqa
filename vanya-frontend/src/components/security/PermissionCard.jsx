import React from "react";

export default function PermissionCard({ permission }) {
  if (!permission) return null;

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
      <div style={{ fontWeight: 600, color: "var(--text-1)" }}>{permission.permission_name}</div>
      <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 2 }}>{permission.permission_id}</div>
      {permission.description ? (
        <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 4 }}>{permission.description}</div>
      ) : null}
    </li>
  );
}
