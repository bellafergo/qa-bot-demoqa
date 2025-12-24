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

function getThreadId(t) {
  // Normaliza SIEMPRE a string y evita ids falsos
  const raw = t?.id ?? t?.thread_id ?? "";
  const id = String(raw || "").trim();
  return id || null;
}

function buildTitle(t) {
  const title = String(t?.title || "").trim();
  if (title && title.toLowerCase() !== "new chat") return title;

  const preview = String(t?.preview || "").trim();
  if (preview) return preview.length > 48 ? preview.slice(0, 48) + "…" : preview;

  const id = getThreadId(t);
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

  const normalized = useMemo(() => {
    // Limpia threads inválidos y normaliza ids
    return (Array.isArray(threads) ? threads : [])
      .map((t) => {
        const id = getThreadId(t);
        if (!id) return null;
        return { ...t, __id: id };
      })
      .filter(Boolean);
  }, [threads]);

  const filtered = useMemo(() => {
    const q = filter.trim().toLowerCase();
    if (!q) return normalized;

    return normalized.filter((t) => {
      const title = String(t?.title || "").toLowerCase();
      const preview = String(t?.preview || "").toLowerCase();
      const id = String(t?.__id || "").toLowerCase();
      return title.includes(q) || preview.includes(q) || id.includes(q);
    });
  }, [normalized, filter]);

  const activeStr = String(activeId || "").trim();

  return (
    <div
      style={{
        width: 320,
        borderRight: "1px solid rgba(255,255,255,0.08)",
        padding: 12,
        background: "rgba(0,0,0,0.15)",
        height: "100%",
        boxSizing: "border-box",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <div style={{ fontWeight: 800, marginBottom: 10, opacity: 0.9 }}>
        Historial
      </div>

      <button
        onClick={() => onNew?.()}
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

      <div
        style={{
          marginTop: 12,
          overflowY: "auto",
          flex: 1,
          paddingRight: 2,
        }}
      >
        {filtered.map((t) => {
          const id = t.__id; // ya normalizado
          const title = buildTitle(t);
          const subtitle =
            String(t?.preview || "").trim() ||
            (t?.updated_at ? fmtDate(t.updated_at) : "");

          const isActive = String(id) === activeStr;

          return (
            <div
              key={id}
              data-thread-id={id} // 👈 para debug rápido en DevTools
              onClick={() => onSelect?.(id)}
              role="button"
              tabIndex={0}
              onKeyDown={(e) => {
                if (e.key === "Enter" || e.key === " ") onSelect?.(id);
              }}
              style={{
                marginTop: 10,
                padding: 10,
                cursor: "pointer",
                background: isActive
                  ? "rgba(78,107,255,0.18)"
                  : "rgba(0,0,0,0.18)",
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
                <div
                  style={{
                    fontWeight: 750,
                    fontSize: 13,
                    color: "white",
                    overflow: "hidden",
                    textOverflow: "ellipsis",
                    whiteSpace: "nowrap",
                    maxWidth: 220,
                  }}
                >
                  {title}
                </div>

                {subtitle ? (
                  <div
                    style={{
                      fontSize: 11,
                      opacity: 0.7,
                      marginTop: 3,
                      color: "white",
                      overflow: "hidden",
                      textOverflow: "ellipsis",
                      whiteSpace: "nowrap",
                      maxWidth: 220,
                    }}
                  >
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
                  flexShrink: 0,
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