import React from "react";
import { SkeletonCard } from "../ui/Skeleton.jsx";
import { formatTestDisplayNameWithMeta } from "../../utils/humanizeTestNameUtils.js";

export default function TopProblemCard({
  analytics,
  loading,
  t,
  onRerun,
  onView,
  busy,
  error,
}) {
  const tf = analytics?.top_failures?.[0];
  if (loading) {
    return (
      <div className="card dash-top-problem" style={{ padding: "18px 22px", marginBottom: 28 }}>
        <div className="section-title" style={{ margin: 0, marginBottom: 10 }}>{t("dash.top_problem.title")}</div>
        <SkeletonCard lines={3} />
      </div>
    );
  }
  if (!tf) return null;

  const testMeta = formatTestDisplayNameWithMeta({
    testId: tf.test_case_id,
    testName: tf.test_name,
  }, t);
  const displayName = testMeta.display;

  return (
    <div className="card dash-top-problem" style={{ padding: "18px 22px", marginBottom: 28 }}>
      <div className="section-title" style={{ margin: 0, marginBottom: 12 }}>{t("dash.top_problem.title")}</div>
      <div style={{ marginBottom: 10 }}>
        <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)", marginBottom: 4 }} title={testMeta.showTechnicalId ? testMeta.technicalId : undefined}>
          {displayName}
        </div>
        {testMeta.showTechnicalId ? (
          <div style={{ fontFamily: "ui-monospace, monospace", fontSize: 11, color: "var(--text-3)" }}>{testMeta.technicalId}</div>
        ) : null}
      </div>
      <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 14, lineHeight: 1.5 }}>
        <span>{t("dash.top_problem.impact_failed", { n: tf.failed_runs ?? 0 })}</span>
        {" · "}
        <span>{t("dash.top_problem.impact_runs", { n: tf.total_runs ?? 0 })}</span>
        {" · "}
        <span>
          {t("dash.top_problem.impact_rate", {
            pct:
              tf.pass_rate != null && !Number.isNaN(Number(tf.pass_rate))
                ? Number(tf.pass_rate).toFixed(1)
                : "—",
          })}
        </span>
      </div>
      {error ? (
        <div style={{ fontSize: 12, color: "var(--red)", marginBottom: 10 }}>{error}</div>
      ) : null}
      <div style={{ display: "flex", flexWrap: "wrap", gap: 10, alignItems: "center" }}>
        <button
          type="button"
          className="btn btn-secondary btn-sm"
          disabled={busy || !tf.test_case_id}
          aria-busy={busy ? true : undefined}
          onClick={() => onRerun(tf.test_case_id)}
        >
          {busy ? t("dash.rerun.running") : t("dash.top_problem.rerun")}
        </button>
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => onView(tf)}>
          {t("dash.top_problem.view")}
        </button>
      </div>
    </div>
  );
}
