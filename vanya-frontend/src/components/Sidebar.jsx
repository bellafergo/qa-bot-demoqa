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
      <div style={{ fontWeight: 800, marginBottom: 10 }}>Historial</div>

      <button onClick={onNew} disabled={isLoading} style={{ width: "100%" }}>
        + New chat
      </button>

      <div style={{ marginTop: 12 }}>
        {threads.map((t, idx) => {
          const id = t?.id || t?.thread_id || String(idx);
          const title = t?.title || "New chat";

          return (
            <div
              key={id}
              onClick={() => onSelect?.(id)}
              style={{
                marginTop: 8,
                padding: 8,
                cursor: "pointer",
                background: id === activeId ? "#f2f2f2" : "transparent",
                borderRadius: 8,
                border: id === activeId ? "1px solid #ddd" : "1px solid transparent",
              }}
              title={title}
            >
              {title}
            </div>
          );
        })}
      </div>
    </div>
  );
}