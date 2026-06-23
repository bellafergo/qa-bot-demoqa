import React, { useMemo } from "react";
import { buildWatchListItemViewModel } from "../../utils/browserWatchViewUtils.js";

function WatchRow({ item, selected, labels, onSelect }) {
  return (
    <button
      type="button"
      onClick={() => onSelect(item.watchId)}
      className="card"
      style={{
        width: "100%",
        textAlign: "left",
        padding: "14px 16px",
        marginBottom: 10,
        cursor: "pointer",
        border: selected ? "1px solid var(--accent)" : "1px solid var(--border)",
        background: selected ? "var(--accent-light)" : undefined,
        boxShadow: item.needsAttention && !selected ? "inset 3px 0 0 #f59e0b" : undefined,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 10, alignItems: "flex-start", marginBottom: 10 }}>
        <div style={{ fontSize: 14, fontWeight: 700, color: "var(--text-1)", lineHeight: 1.3 }}>{item.name}</div>
        <span className={`badge ${item.statusBadgeClass}`} style={{ fontSize: 10, flexShrink: 0 }}>
          {item.statusLabel}
        </span>
      </div>

      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(120px, 1fr))", gap: "8px 14px", fontSize: 12 }}>
        <div>
          <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", marginBottom: 3 }}>{labels.url}</div>
          <div
            title={item.url || undefined}
            style={{
              color: "var(--text-2)",
              overflow: "hidden",
              textOverflow: "ellipsis",
              whiteSpace: "nowrap",
            }}
          >
            {item.urlDisplay}
          </div>
        </div>
        <div>
          <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", marginBottom: 3 }}>{labels.execution}</div>
          <div style={{ color: "var(--text-1)", fontWeight: 600 }}>{item.executionLabel}</div>
        </div>
        <div>
          <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", marginBottom: 3 }}>{labels.compare}</div>
          <div style={{ color: "var(--text-1)" }}>{item.compareLabel}</div>
        </div>
        <div>
          <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", marginBottom: 3 }}>{labels.lastRun}</div>
          <div style={{ color: "var(--text-1)" }}>{item.lastRunText}</div>
        </div>
        <div>
          <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", marginBottom: 3 }}>{labels.lastChange}</div>
          <div style={{ color: "var(--text-1)" }}>{item.lastChangeText}</div>
        </div>
      </div>
    </button>
  );
}

export default function BrowserWatchList({ watches, selectedId, t, onSelect }) {
  const labels = useMemo(
    () => ({
      url: t("watch.enterprise.list.url"),
      execution: t("watch.enterprise.list.execution"),
      compare: t("watch.enterprise.list.compare"),
      lastRun: t("watch.enterprise.list.last_run"),
      lastChange: t("watch.enterprise.list.last_change"),
    }),
    [t],
  );

  const items = useMemo(() => {
    const priority = { critical: 0, warning: 1, never_run: 2, healthy: 3, disabled: 4 };
    return (watches || [])
      .map((w) => buildWatchListItemViewModel(w, t))
      .sort((a, b) => (priority[a.statusBucket] ?? 9) - (priority[b.statusBucket] ?? 9));
  }, [watches, t]);

  return (
    <div>
      {items.map((item) => (
        <WatchRow
          key={item.watchId}
          item={item}
          selected={selectedId === item.watchId}
          labels={labels}
          onSelect={onSelect}
        />
      ))}
    </div>
  );
}
