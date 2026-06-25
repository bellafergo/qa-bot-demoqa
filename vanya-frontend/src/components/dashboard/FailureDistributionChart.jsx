import React from "react";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";
import { SkeletonCard } from "../ui/Skeleton.jsx";
import {
  resolveHistoryCapabilityState,
  CAPABILITY_STATE_I18N_KEYS,
  DEFAULT_MIN_RUNS_FOR_FAILURE_INTEL,
} from "../../utils/capabilityStateViewUtils.js";

export default function FailureDistributionChart({ fi, loading, t }) {
  if (loading) {
    return <SkeletonCard lines={4} />;
  }

  if (!fi && !loading) {
    return (
      <CapabilityStateCard
        state={resolveHistoryCapabilityState({
          runCount: 0,
          minRuns: DEFAULT_MIN_RUNS_FOR_FAILURE_INTEL,
          title: t(CAPABILITY_STATE_I18N_KEYS.failuresTitle),
          t,
        })}
      />
    );
  }

  const flaky       = fi?.flaky_tests_count             ?? 0;
  const clusters    = fi?.total_clusters                ?? 0;
  const regressions = fi?.recurrent_regressions_count   ?? 0;
  const maxVal      = Math.max(flaky, clusters, regressions, 1);
  const allZero     = flaky === 0 && clusters === 0 && regressions === 0;

  const bars = [
    { key: "dash.failures.flaky",       val: flaky,       color: "var(--orange)" },
    { key: "dash.failures.clusters",    val: clusters,    color: "var(--blue)"  },
    { key: "dash.failures.regressions", val: regressions, color: "var(--red)"     },
  ];

  if (allZero && !loading) {
    return (
      <div style={{ padding: "14px 0", fontSize: 12, color: "var(--green)", fontWeight: 500 }}>
        ✓ {t("dash.failures.all_clear")}
      </div>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
      {bars.map(({ key, val, color }) => (
        <div key={key}>
          <div style={{ display: "flex", justifyContent: "space-between", marginBottom: 5, fontSize: 12 }}>
            <span style={{ color: "var(--text-2)" }}>{t(key)}</span>
            <span style={{ fontWeight: 600, color: val > 0 ? color : "var(--text-3)" }}>
              {val}
            </span>
          </div>
          <div style={{ height: 7, borderRadius: 4, background: "var(--border)", overflow: "hidden" }}>
            <div style={{
              height: "100%",
              borderRadius: 4,
              background: color,
              width: loading ? "0%" : `${((val / maxVal) * 100).toFixed(1)}%`,
              transition: "width 0.5s ease",
              opacity: val === 0 ? 0.18 : 1,
            }} />
          </div>
        </div>
      ))}
    </div>
  );
}
