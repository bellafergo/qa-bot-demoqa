import React from "react";

export function Skeleton({ width = "100%", height = 14, style, className = "" }) {
  return (
    <span
      className={`ui-skeleton ${className}`.trim()}
      style={{ display: "block", width, height, borderRadius: 6, ...style }}
      aria-hidden
    />
  );
}

export function SkeletonKpi() {
  return (
    <div className="kpi-card" aria-hidden>
      <Skeleton width="55%" height={12} style={{ marginBottom: 14 }} />
      <Skeleton width="42%" height={28} style={{ marginBottom: 10 }} />
      <Skeleton width="70%" height={11} />
    </div>
  );
}

export function SkeletonCard({ lines = 3 }) {
  return (
    <div className="card" style={{ padding: "20px 24px" }} aria-hidden>
      <Skeleton width="38%" height={12} style={{ marginBottom: 16 }} />
      {Array.from({ length: lines }).map((_, i) => (
        <Skeleton key={i} width={i === lines - 1 ? "62%" : "92%"} height={12} style={{ marginBottom: 10 }} />
      ))}
    </div>
  );
}

export function SkeletonTable({ rows = 5, cols = 4 }) {
  return (
    <div className="card" style={{ overflow: "hidden", padding: 0 }} aria-hidden>
      <div style={{ padding: "12px 16px", borderBottom: "1px solid var(--border)" }}>
        <div style={{ display: "grid", gridTemplateColumns: `repeat(${cols}, 1fr)`, gap: 12 }}>
          {Array.from({ length: cols }).map((_, i) => (
            <Skeleton key={`h-${i}`} height={10} />
          ))}
        </div>
      </div>
      {Array.from({ length: rows }).map((_, row) => (
        <div key={row} style={{ padding: "12px 16px", borderBottom: row < rows - 1 ? "1px solid var(--border)" : undefined }}>
          <div style={{ display: "grid", gridTemplateColumns: `repeat(${cols}, 1fr)`, gap: 12 }}>
            {Array.from({ length: cols }).map((_, col) => (
              <Skeleton key={`${row}-${col}`} height={12} width={col === 0 ? "80%" : "60%"} />
            ))}
          </div>
        </div>
      ))}
    </div>
  );
}

export function SkeletonKpiGrid({ count = 6 }) {
  return (
    <div className="kpi-grid" style={{ marginBottom: 28 }} aria-hidden>
      {Array.from({ length: count }).map((_, i) => (
        <SkeletonKpi key={i} />
      ))}
    </div>
  );
}
