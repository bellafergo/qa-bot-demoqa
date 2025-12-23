// src/components/Sidebar.jsx
import React from "react";

export default function Sidebar({
  threads = [],
  activeId = null,
  onNew,
  onSelect,
  onDelete,          // ✅ nuevo
  isLoading = false,
}) {
  return (
    <div style={{ width: 260, borderRight: "1px solid rgba(255,255,255,0.08)", padding: 12 }}>
      <div style={{ fontWeight: 800, marginBottom: 10, opacity: 0.9 }}>Historial</div>

      <button
        onClick={onNew}
        disabled={isLoading}
        style={{ width: "100%", padding: "10px 12px", borderRadius: 10 }}
      >
        + New chat
      </button>

      <div style={{ marginTop: 12, display: "flex", flexDirection: "column", gap: 8 }}>
        {threads.length === 0 && (
          <div style={{ fontSize: 12, opacity: 0.65, padding: "8px 4px" }}>
            Aún no hay chats guardados.
          </div>
        )}

        {threads.map((t, idx) => {
          const id = t?.id || t?.thread_id || `fallback-${idx}`;
          const isActive = String(id) === String(activeId);

          // ✅ Usa title del backend; si sigue "New chat", al menos muestra id corto
          const titleRaw = (t?.title || "New chat").trim();
          const title =
            titleRaw !== "New chat"
              ? titleRaw
              : `Chat ${String(id).slice(0, 6)}…`;

          return (
            <div
              key={id}
              style={{
                display: "flex",
                gap: 8,
                alignItems: "stretch",
              }}
            >
              <button
                onClick={() => onSelect?.(id)}
                type="button"
                style={{
                  flex: 1,
                  textAlign: "left",
                  padding: "10px 10px",
                  borderRadius: 10,
                  cursor: "pointer",
                  border: isActive
                    ? "1px solid rgba(255,255,255,0.22)"
                    : "1px solid rgba(255,255,255,0.08)",
                  background: isActive ? "rgba(255,255,255,0.08)" : "transparent",
                  color: "rgba(255,255,255,0.92)",
                }}
                title={id}
              >
                <div style={{ fontWeight: 700, fontSize: 13, lineHeight: 1.2 }}>
                  {title}
                </div>
                <div style={{ fontSize: 11, opacity: 0.6, marginTop: 4 }}>
                  id: {String(id).slice(0, 8)}…
                </div>
              </button>

              <button
                type="button"
                onClick={() => onDelete?.(id)}
                disabled={isLoading}
                title="Borrar chat"
                style={{
                  width: 38,
                  borderRadius: 10,
                  border: "1px solid rgba(255,255,255,0.08)",
                  background: "transparent",
                  cursor: isLoading ? "not-allowed" : "pointer",
                  color: "rgba(255,255,255,0.8)",
                }}
              >
                🗑️
              </button>
            </div>
          );
        })}
      </div>
    </div>
  );
}