// src/components/Sidebar.jsx
import React, { useMemo, useState } from "react";

function fmtDate(iso) {
  if (!iso) return "";
  try {
    const d = new Date(iso);
    if (Number.isNaN(d.getTime())) return "";
    return d.toLocaleString();
  } catch {
    return "";
  }
}

function buildFallbackTitle(t) {
  const title = (t?.title || "").trim();
  if (title && title.toLowerCase() !== "new chat") return title;

  // Si backend manda un preview (ideal)
  const preview = (t?.preview || "").trim();
  if (preview) return preview.length > 48 ? preview.slice(0, 48) + "…" : preview;

  // fallback final: id corto
  const id = String(t?.id || t?.thread_id || "").trim();
  if (id) return `Chat ${id.slice(0, 6)}…`;

  return "Chat";
}

export default function Sidebar({
  threads = [],
  activeId = null,
  onNew,
  onSelect,
  onDelete,
  isLoading = false,
}) {
  const [filter, setFilter] = useState("");

  const filtered = useMemo(() => {
    const q = filter.trim().toLowerCase();
    if (!q) return threads;
    return threads.filter((t) => {
      const title = (t?.title || "").toLowerCase();
      const preview = (t?.preview || "").toLowerCase();
      const id = String(t?.id || t?.thread_id || "").toLowerCase();
      return title.includes(q) || preview.includes(q) || id.includes(q);
    });
  }, [threads, filter]);

  return (
    <div
      style={{
        width: 320,
        borderRight: "1px solid rgba(255,255,255,0.08)",
        padding: 12,
        background: "rgba(0,0,0,0.15)",
        height: "100%",
        boxSizing: "border-box",
      }}
    >
      <div style={{ fontWeight: 800, marginBottom: 10, opacity: 0.9 }}>
        Historial
      </div>

      <button
        onClick={onNew}
        disabled={isLoading}
        style={{
          width: "100%",
          padding: "10px 12px",
          borderRadius: 10,
          border: "1px solid rgba(255,255,255,0.12)",
          background: "#4e6bff",
          color: "white",
          cursor: isLoading ? "not-allowed" : "pointer",
          fontWeight: 700,
        }}
      >
        + New chat
      </button>

      <input
        value={filter}
        onChange={(e) => setFilter(e.target.value)}
        placeholder="Buscar…"
        style={{
          width: "100%",
          marginTop: 10,
          padding: "10px 12px",
          borderRadius: 10,
          border: "1px solid rgba(255,255,255,0.12)",
          background: "rgba(0,0,0,0.25)",
          color: "white",
          outline: "none",
          boxSizing: "border-box",
        }}
      />

      <div style={{ marginTop: 12, overflowY: "auto", height: "calc(100vh - 160px)" }}>
        {filtered.map((t, idx) => {
          const id = t?.id || t?.thread_id || String(idx);
          const title = buildFallbackTitle(t);
          const subtitle =
            (t?.preview || "").trim() ||
            (t?.updated_at ? fmtDate(t.updated_at) : "");

          const isActive = String(id) === String(activeId);

          return (
            <div
              key={id}
              onClick={() => onSelect?.(id)}
              style={{
                marginTop: 10,
                padding: 10,
                cursor: "pointer",
                background: isActive ? "rgba(78,107,255,0.18)" : "rgba(0,0,0,0.18)",
                borderRadius: 12,
                border: isActive
                  ? "1px solid rgba(78,107,255,0.55)"
                  : "1px solid rgba(255,255,255,0.10)",
                display: "flex",
                alignItems: "center",
                justifyContent: "space-between",
                gap: 10,
              }}
              title={title}
            >
              <div style={{ minWidth: 0 }}>
                <div style={{ fontWeight: 750, fontSize: 13, color: "white" }}>
                  {title}
                </div>
                {subtitle ? (
                  <div style={{ fontSize: 11, opacity: 0.7, marginTop: 3, color: "white" }}>
                    {subtitle}
                  </div>
                ) : null}
              </div>

              <button
                onClick={(e) => {
                  e.preventDefault();
                  e.stopPropagation(); // IMPORTANTÍSIMO: que no seleccione el chat
                  onDelete?.(id);
                }}
                disabled={isLoading}
                title="Eliminar chat"
                style={{
                  width: 34,
                  height: 34,
                  borderRadius: 10,
                  border: "1px solid rgba(255,255,255,0.12)",
                  background: "rgba(0,0,0,0.35)",
                  color: "white",
                  cursor: isLoading ? "not-allowed" : "pointer",
                  opacity: 0.9,
                }}
              >
                🗑️
              </button>
            </div>
          );
        })}

        {!filtered.length && (
          <div style={{ marginTop: 14, opacity: 0.7, fontSize: 12 }}>
            No hay chats.
          </div>
        )}
      </div>
    </div>
  );
}