import React from "react";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";
import { SkeletonCard } from "../ui/Skeleton.jsx";
import {
  resolveHistoryCapabilityState,
  CAPABILITY_STATE_I18N_KEYS,
  DEFAULT_MIN_RUNS_FOR_TRENDS,
} from "../../utils/capabilityStateViewUtils.js";
import { isPassStatus } from "../../utils/dashboardHelpers.js";

export default function PassRateTrendChart({ runs, loading, t }) {
  const data = [...(runs || [])].reverse().slice(-10);

  // Guard: must have at least 2 points to draw the chart.
  // When loading, show skeleton. When done but insufficient data, show no-data state.
  if (data.length < 2) {
    if (loading) {
      return <SkeletonCard lines={5} />;
    }
    return (
      <CapabilityStateCard
        state={resolveHistoryCapabilityState({
          runCount: data.length,
          minRuns: DEFAULT_MIN_RUNS_FOR_TRENDS,
          title: t(CAPABILITY_STATE_I18N_KEYS.trendsTitle),
          t,
        })}
      />
    );
  }

  // QA FINAL — extra right padding so "Meta 80%" does not collide with the last plot point
  const W = 400, H = 110, PAD_L = 32, PAD_R = 40, PAD_T = 10, PAD_B = 24;
  const innerW = W - PAD_L - PAD_R;
  const innerH = H - PAD_T - PAD_B;

  // Sliding 3-run window pass rate
  const rates = data.map((_, i) => {
    const window = data.slice(Math.max(0, i - 2), i + 1);
    const passed = window.filter(r => isPassStatus(r.status)).length;
    return passed / window.length;
  });

  const avgRate = rates.reduce((a, b) => a + b, 0) / Math.max(rates.length, 1);

  const pts = rates.map((rate, i) => {
    const x = PAD_L + (data.length <= 1 ? innerW / 2 : (i * innerW) / (data.length - 1));
    const y = PAD_T + innerH * (1 - rate);
    return [x, y];
  });

  // Safety net: should never be empty here, but guard anyway
  if (pts.length < 2) return null;

  const lineStr = pts.map(([x, y]) => `${x.toFixed(1)},${y.toFixed(1)}`).join(" ");

  const areaPath =
    `M ${pts[0][0].toFixed(1)},${(PAD_T + innerH).toFixed(1)} ` +
    `L ${pts[0][0].toFixed(1)},${pts[0][1].toFixed(1)} ` +
    pts.slice(1).map(([x, y]) => `L ${x.toFixed(1)},${y.toFixed(1)}`).join(" ") +
    ` L ${pts[pts.length - 1][0].toFixed(1)},${(PAD_T + innerH).toFixed(1)} Z`;

  const avgY = (PAD_T + innerH * (1 - avgRate)).toFixed(1);

  // MEJORA #7 — reference band 80–100% and horizontal line at 80%
  const yAt = (rate) => PAD_T + innerH * (1 - rate);
  const y80 = yAt(0.8);
  const y100 = yAt(1);
  const bandH = y80 - y100;

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} style={{ width: "100%", height: 110, display: "block" }}>

        {/* Grid lines */}
        {[0, 0.5, 1].map(pct => {
          const gy = (PAD_T + innerH * (1 - pct)).toFixed(1);
          return (
            <line key={pct} x1={PAD_L} y1={gy} x2={W - PAD_R} y2={gy}
              stroke="var(--border)" strokeWidth="0.5" strokeDasharray="2,4" />
          );
        })}

        {/* MEJORA #7 — success band between 80% and 100% (behind series) */}
        <rect
          x={PAD_L}
          y={y100}
          width={innerW}
          height={Math.max(0, bandH)}
          fill="var(--green)"
          opacity={0.08}
        />
        <line
          x1={PAD_L}
          y1={y80}
          x2={W - PAD_R}
          y2={y80}
          stroke="var(--green)"
          strokeWidth="1"
          strokeDasharray="4,3"
          opacity={0.85}
        />
        <text
          x={PAD_L + 4}
          y={Math.max(PAD_T + 11, y80 - 4)}
          textAnchor="start"
          fontSize="9"
          fill="var(--green)"
          fontWeight={500}
        >
          {t("dash.trends.meta_80")}
        </text>

        {/* Area fill */}
        <path d={areaPath} fill="var(--chart-trend-fill)" />

        {/* Avg line */}
        <line x1={PAD_L} y1={avgY} x2={W - PAD_R} y2={avgY}
          stroke="var(--text-3)" strokeWidth="1" strokeDasharray="3,3" />

        {/* Trend line */}
        <polyline points={lineStr} fill="none" stroke="var(--chart-line)"
          strokeWidth="1.75" strokeLinejoin="round" strokeLinecap="round" />

        {/* Data points */}
        {pts.map(([x, y], i) => (
          <circle key={i} cx={x.toFixed(1)} cy={y.toFixed(1)} r="3.5"
            fill={isPassStatus(data[i]?.status) ? "var(--green)" : "var(--red)"}
            stroke="var(--surface)" strokeWidth="1.5" />
        ))}

        {/* Y-axis labels */}
        <text x={PAD_L - 4} y={PAD_T + 4}           textAnchor="end" fontSize="9" fill="var(--text-3)">100%</text>
        <text x={PAD_L - 4} y={PAD_T + innerH / 2 + 3} textAnchor="end" fontSize="9" fill="var(--text-3)">50%</text>
        <text x={PAD_L - 4} y={PAD_T + innerH + 4}  textAnchor="end" fontSize="9" fill="var(--text-3)">0%</text>

        {/* X-axis labels — show first, middle, last */}
        {pts.length > 0 && [0, Math.floor((pts.length - 1) / 2), pts.length - 1]
          .filter((v, i, arr) => arr.indexOf(v) === i)
          .map(i => (
            <text key={i} x={pts[i][0].toFixed(1)} y={H - 5}
              textAnchor="middle" fontSize="8" fill="var(--text-3)">
              {t("dash.trends.run")} {i + 1}
            </text>
          ))}
      </svg>

      <div style={{ display: "flex", gap: 12, marginTop: 6, fontSize: 11, color: "var(--text-3)" }}>
        <span>{t("dash.trends.subtitle")}: <strong style={{ color: "var(--text-2)" }}>{data.length}</strong></span>
        <span>· {t("dash.trends.avg")}: <strong style={{ color: "var(--chart-line)" }}>{(avgRate * 100).toFixed(0)}%</strong></span>
      </div>
    </div>
  );
}
