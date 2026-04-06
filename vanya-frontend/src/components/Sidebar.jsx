// src/components/Sidebar.jsx
import React, { useEffect, useMemo, useRef, useState } from "react";
import { useLang } from "../i18n/LangContext";
import { apiFetch } from "../api.js";

/**
 * Thread history sidebar — light panel, dark-chat compatible.
 * Auto-fetches from /threads when no threads prop is supplied.
 */

async function fetchThreads(limit = 80, signal) {
  const res = await apiFetch(`/threads?limit=${limit}`, {
    method: "GET",
    headers: { "Content-Type": "application/json" },
    signal,
  });
  if (!res.ok) throw new Error(`Threads error: ${res.status}`);
  const data = await res.json();
  return Array.isArray(data) ? data : [];
}

function fmtDate(iso) {
  if (!iso) return "";
  try {
    const d = new Date(iso);
    if (Number.isNaN(d.getTime())) return "";
    return d.toLocaleDateString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
  } catch { return ""; }
}

function getThreadId(t) {
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
  threads: threadsProp = [],
  activeId = null,
  onNew,
  onSelect,
  onDelete,
  isLoading = false,
}) {
  const { t } = useLang();
  const [filter, setFilter] = useState("");
  const [threadsRemote, setThreadsRemote] = useState([]);
  const [isFetching, setIsFetching] = useState(false);
  const [fetchErr, setFetchErr] = useState("");
  const abortRef = useRef(null);

  const usingRemote = !Array.isArray(threadsProp) || threadsProp.length === 0;
  const threads = usingRemote ? threadsRemote : threadsProp;

  const load = async () => {
    if (!usingRemote) return;
    setIsFetching(true);
    setFetchErr("");
    try {
      abortRef.current?.abort?.();
      const controller = new AbortController();
      abortRef.current = controller;
      const data = await fetchThreads(120, controller.signal);
      setThreadsRemote(data);
    } catch (e) {
      if (e?.name !== "AbortError") setFetchErr(e?.message || "Error loading history");
    } finally { setIsFetching(false); }
  };

  useEffect(() => {
    load();
    return () => abortRef.current?.abort?.();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [usingRemote]);

  const normalized = useMemo(() =>
    (Array.isArray(threads) ? threads : [])
      .map(t => { const id = getThreadId(t); return id ? { ...t, __id: id } : null; })
      .filter(Boolean),
  [threads]);

  const filtered = useMemo(() => {
    const q = filter.trim().toLowerCase();
    if (!q) return normalized;
    return normalized.filter(t => {
      const title   = String(t?.title   || "").toLowerCase();
      const preview = String(t?.preview || "").toLowerCase();
      const id      = String(t?.__id    || "").toLowerCase();
      return title.includes(q) || preview.includes(q) || id.includes(q);
    });
  }, [normalized, filter]);

  const activeStr = String(activeId || "").trim();
  const busy = Boolean(isLoading || isFetching);

  return (
    <div style={{
      width: "var(--thread-w)",
      minWidth: "var(--thread-w)",
      height: "100%",
      background: "var(--surface)",
      borderRight: "1px solid var(--border)",
      display: "flex",
      flexDirection: "column",
      overflow: "hidden",
    }}>
      {/* ── Header ──────────────────────────────────────── */}
      <div style={{
        padding: "14px 14px 10px",
        borderBottom: "1px solid var(--border)",
        flexShrink: 0,
      }}>
        <div style={{
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          marginBottom: 10,
        }}>
          <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.07em" }}>
            {t("chat.sidebar.title")}
          </span>
          <button
            onClick={load}
            disabled={!usingRemote || busy}
            title={t("chat.sidebar.refresh")}
            style={{
              width: 28, height: 28,
              borderRadius: 6,
              border: "1px solid var(--border)",
              background: "var(--surface)",
              color: "var(--text-2)",
              cursor: !usingRemote || busy ? "not-allowed" : "pointer",
              opacity: usingRemote ? 1 : 0.35,
              display: "flex", alignItems: "center", justifyContent: "center",
              fontSize: 14,
              transition: "background 0.15s",
            }}
          >
            ↻
          </button>
        </div>

        {/* New chat button */}
        <button
          onClick={() => { onNew?.(); if (usingRemote) setTimeout(load, 350); }}
          disabled={busy}
          style={{
            width: "100%",
            padding: "8px 12px",
            borderRadius: 7,
            border: "1px solid var(--accent-border)",
            background: "var(--accent-light)",
            color: "var(--accent)",
            cursor: busy ? "not-allowed" : "pointer",
            fontWeight: 600,
            fontSize: 13,
            fontFamily: "inherit",
            opacity: busy ? 0.7 : 1,
            transition: "background 0.15s",
          }}
        >
          {t("chat.sidebar.new")}
        </button>

        {/* Search */}
        <div style={{ position: "relative", marginTop: 8 }}>
          <span style={{
            position: "absolute", left: 9, top: "50%", transform: "translateY(-50%)",
            fontSize: 12, color: "var(--text-3)", pointerEvents: "none",
          }}>🔍</span>
          <input
            value={filter}
            onChange={e => setFilter(e.target.value)}
            placeholder={t("chat.sidebar.search")}
            style={{
              width: "100%",
              padding: "7px 10px 7px 28px",
              border: "1px solid var(--border)",
              borderRadius: 7,
              background: "var(--surface-2)",
              color: "var(--text-1)",
              fontSize: 12,
              fontFamily: "inherit",
              outline: "none",
              boxSizing: "border-box",
            }}
          />
        </div>
      </div>

      {/* ── Error ───────────────────────────────────────── */}
      {fetchErr && (
        <div style={{ padding: "8px 14px", fontSize: 12, color: "var(--red-text)", background: "var(--red-bg)" }}>
          {fetchErr}
        </div>
      )}

      {/* ── Thread list ─────────────────────────────────── */}
      <div style={{ flex: 1, overflowY: "auto", padding: "6px 8px" }}>
        {filtered.map(th => {
          const id = th.__id;
          const title = buildTitle(th);
          const subtitle = String(th?.preview || "").trim() || (th?.updated_at ? fmtDate(th.updated_at) : "");
          const isActive = id === activeStr;

          return (
            <div
              key={id}
              data-thread-id={id}
              onClick={() => onSelect?.(id)}
              role="button"
              tabIndex={0}
              onKeyDown={e => { if (e.key === "Enter" || e.key === " ") onSelect?.(id); }}
              style={{
                padding: "9px 10px",
                borderRadius: 8,
                marginBottom: 2,
                cursor: "pointer",
                background: isActive ? "var(--accent-light)" : "transparent",
                border: isActive ? "1px solid var(--accent-border)" : "1px solid transparent",
                display: "flex",
                alignItems: "flex-start",
                justifyContent: "space-between",
                gap: 8,
                opacity: busy ? 0.85 : 1,
                transition: "background 0.12s, border-color 0.12s",
              }}
              onMouseEnter={e => { if (!isActive) e.currentTarget.style.background = "var(--surface-2)"; }}
              onMouseLeave={e => { if (!isActive) e.currentTarget.style.background = "transparent"; }}
              title={title}
            >
              <div style={{ minWidth: 0, flex: 1 }}>
                <div style={{
                  fontWeight: isActive ? 500 : 400,
                  fontSize: 13,
                  color: isActive ? "var(--accent)" : "var(--text-2)",
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  whiteSpace: "nowrap",
                }}>
                  {title}
                </div>
                {subtitle && (
                  <div style={{
                    fontSize: 11,
                    fontWeight: 400,
                    color: "var(--text-4)",
                    marginTop: 2,
                    overflow: "hidden",
                    textOverflow: "ellipsis",
                    whiteSpace: "nowrap",
                  }}>
                    {subtitle}
                  </div>
                )}
              </div>

              <button
                onClick={e => {
                  e.preventDefault();
                  e.stopPropagation();
                  onDelete?.(id);
                  if (usingRemote) setTimeout(load, 350);
                }}
                disabled={busy}
                title={t("chat.sidebar.delete")}
                style={{
                  width: 26, height: 26, flexShrink: 0,
                  borderRadius: 5,
                  border: "1px solid var(--border)",
                  background: "var(--surface)",
                  color: "var(--text-3)",
                  cursor: busy ? "not-allowed" : "pointer",
                  display: "flex", alignItems: "center", justifyContent: "center",
                  fontSize: 11,
                  opacity: 0,
                  transition: "opacity 0.12s, color 0.12s",
                }}
                onMouseEnter={e => { e.currentTarget.style.opacity = "1"; e.currentTarget.style.color = "var(--red)"; }}
                onMouseLeave={e => { e.currentTarget.style.opacity = "0"; e.currentTarget.style.color = "var(--text-3)"; }}
              >
                ✕
              </button>
            </div>
          );
        })}

        {busy && !filtered.length && (
          <div style={{ padding: "16px 10px", color: "var(--text-3)", fontSize: 12 }}>
            {t("chat.sidebar.loading")}
          </div>
        )}
        {!busy && !filtered.length && (
          <div style={{ padding: "16px 10px", color: "var(--text-3)", fontSize: 12 }}>
            {t("chat.sidebar.none")}
          </div>
        )}
      </div>
    </div>
  );
}
