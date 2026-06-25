import React from "react";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";
import { SkeletonCard } from "../ui/Skeleton.jsx";
import {
  resolveHistoryCapabilityState,
  CAPABILITY_STATE_I18N_KEYS,
} from "../../utils/capabilityStateViewUtils.js";

export default function CoverageDonutChart({ summary, loading, t }) {
  const ui    = summary?.total_ui_tests  ?? 0;
  const api   = summary?.total_api_tests ?? 0;
  const total = ui + api;

  if (loading) {
    return <SkeletonCard lines={4} />;
  }

  if (!loading && total === 0) {
    return (
      <CapabilityStateCard
        state={resolveHistoryCapabilityState({
          runCount: 0,
          minRuns: 1,
          title: t(CAPABILITY_STATE_I18N_KEYS.coverageDistributionTitle),
          t,
        })}
      />
    );
  }

  const CX = 56, CY = 56, R = 38, STROKE = 13;
  const CIRC = 2 * Math.PI * R;
  const uiArc  = total > 0 ? (ui  / total) * CIRC : 0;
  const apiArc = total > 0 ? (api / total) * CIRC : 0;
  const uiDeg  = total > 0 ? (ui  / total) * 360  : 0;

  return (
    <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
      <svg viewBox="0 0 112 112" style={{ width: 112, height: 112, flexShrink: 0 }}>
        {/* Track */}
        <circle cx={CX} cy={CY} r={R} fill="none" stroke="var(--border)" strokeWidth={STROKE} />

        {/* UI segment */}
        {ui > 0 && (
          <circle cx={CX} cy={CY} r={R} fill="none"
            stroke="var(--chart-donut-ui)" strokeWidth={STROKE}
            strokeDasharray={`${uiArc.toFixed(2)} ${(CIRC - uiArc).toFixed(2)}`}
            style={{ transform: `rotate(-90deg)`, transformOrigin: `${CX}px ${CY}px` }}
          />
        )}

        {/* API segment */}
        {api > 0 && (
          <circle cx={CX} cy={CY} r={R} fill="none"
            stroke="var(--chart-donut-api)" strokeWidth={STROKE}
            strokeDasharray={`${apiArc.toFixed(2)} ${(CIRC - apiArc).toFixed(2)}`}
            style={{ transform: `rotate(${(-90 + uiDeg).toFixed(2)}deg)`, transformOrigin: `${CX}px ${CY}px` }}
          />
        )}

        {/* Center */}
        <text x={CX} y={CY - 7} textAnchor="middle" fontSize="18" fontWeight="600" fill="var(--text-1)">
          {total}
        </text>
        <text x={CX} y={CY + 9} textAnchor="middle" fontSize="9" fill="var(--text-3)">
          {t("dash.coverage.total")}
        </text>
      </svg>

      <div style={{ display: "flex", flexDirection: "column", gap: 10, fontSize: 12, flex: 1 }}>
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <span style={{ width: 9, height: 9, borderRadius: "50%", background: "var(--chart-donut-ui)", flexShrink: 0 }} />
          <span style={{ color: "var(--text-2)", flex: 1 }}>{t("dash.coverage.ui")}</span>
          <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{ui}</span>
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <span style={{ width: 9, height: 9, borderRadius: "50%", background: "var(--chart-donut-api)", flexShrink: 0 }} />
          <span style={{ color: "var(--text-2)", flex: 1 }}>{t("dash.coverage.api")}</span>
          <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{api}</span>
        </div>
        {total > 0 && (
          <div style={{ fontSize: 10, color: "var(--text-3)", borderTop: "1px solid var(--border)", paddingTop: 6 }}>
            UI {ui > 0 ? ((ui / total) * 100).toFixed(0) : 0}%
            {" · "}
            API {api > 0 ? ((api / total) * 100).toFixed(0) : 0}%
          </div>
        )}
      </div>
    </div>
  );
}
