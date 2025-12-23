// src/components/Sidebar.jsx
import React from "react";

export default function Sidebar({
  threads = [],
  activeId = null,
  onNew,
  onSelect,
  isLoading = false,
}) {
  return (
    <div style={{ width: 260, borderRight: "1px solid #eee", padding: 12 }}>
      <button onClick={onNew} disabled={isLoading}>
        + New chat
      </button>

      <div style={{ marginTop: 12 }}>
        {threads.map((t) => (
          <div
            key={t.id}
            onClick={() => onSelect?.(t.id)}
            style={{
              marginTop: 8,
              padding: 8,
              cursor: "pointer",
              background: t.id === activeId ? "#f2f2f2" : "transparent",
              borderRadius: 8,
            }}
          >
            {t.title || "New chat"}
          </div>
        ))}
      </div>
    </div>
  );
}