import React, { useEffect, useState } from "react";
import { apiErrorMessage, diffBrowserInspections } from "../../api";
import { buildDiffHighlights, canCompareBaseline, shortInspectionId } from "../../utils/baselineExplorerViewUtils.js";

export default function BaselineComparisonModal({
  open,
  onClose,
  watch,
  t,
  showToast,
}) {
  const [diff, setDiff] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState("");

  const comparable = canCompareBaseline(watch);
  const baselineId = String(watch?.baseline_inspection_id || "").trim();
  const latestId = String(watch?.last_inspection_id || "").trim();

  useEffect(() => {
    if (!open) {
      setDiff(null);
      setError("");
      return;
    }
    if (!comparable) {
      setDiff(null);
      setError(t("watch.baseline.compare.unavailable"));
      return;
    }
    let cancelled = false;
    (async () => {
      setLoading(true);
      setError("");
      setDiff(null);
      try {
        const result = await diffBrowserInspections({
          base_inspection_id: baselineId,
          target_inspection_id: latestId,
          project_id: watch?.project_id || undefined,
        });
        if (!cancelled) setDiff(result);
      } catch (e) {
        if (!cancelled) {
          const msg = apiErrorMessage(e);
          setError(msg);
          showToast?.(msg, "error");
        }
      } finally {
        if (!cancelled) setLoading(false);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [open, comparable, baselineId, latestId, watch?.project_id, t, showToast]);

  if (!open) return null;

  const highlights = buildDiffHighlights(diff, t);
  const regressionSignals = Array.isArray(diff?.regression_signals) ? diff.regression_signals : [];
  const improvementSignals = Array.isArray(diff?.improvement_signals) ? diff.improvement_signals : [];

  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex: 9999,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
      onClick={onClose}
    >
      <div
        className="card"
        style={{ width: "min(720px, 100%)", padding: 0, overflow: "hidden", maxHeight: "90vh", display: "flex", flexDirection: "column" }}
        onClick={(e) => e.stopPropagation()}
      >
        <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
          <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>
            {t("watch.baseline.compare.title")}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
            {t("watch.baseline.compare.subtitle", {
              baseline: shortInspectionId(baselineId, 12) || "—",
              latest: shortInspectionId(latestId, 12) || "—",
            })}
          </div>
        </div>

        <div style={{ padding: "14px 20px", overflowY: "auto" }}>
          {loading && <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("watch.baseline.compare.loading")}</div>}
          {error && !loading && (
            <div className="alert alert-error" style={{ fontSize: 12 }}>
              {error}
            </div>
          )}
          {!loading && !error && diff && (
            <>
              <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 14 }}>
                <span className="badge badge-gray" style={{ fontSize: 10 }}>
                  {t("watch.baseline.compare.level")}: {diff.change_level || "—"}
                </span>
                {diff.visual_change_detected ? (
                  <span className="badge badge-orange" style={{ fontSize: 10 }}>
                    {t("watch.baseline.compare.visual_detected")}
                  </span>
                ) : null}
              </div>
              {diff.summary ? (
                <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.5, margin: "0 0 14px" }}>{diff.summary}</p>
              ) : null}
              {highlights.length > 0 ? (
                <dl style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: "8px 12px", margin: "0 0 14px", fontSize: 13 }}>
                  {highlights.map((h) => (
                    <React.Fragment key={h.label}>
                      <dt style={{ color: "var(--text-3)" }}>{h.label}</dt>
                      <dd style={{ margin: 0, fontWeight: 600 }}>{h.value}</dd>
                    </React.Fragment>
                  ))}
                </dl>
              ) : (
                <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 14 }}>{t("watch.baseline.compare.no_changes")}</div>
              )}
              {regressionSignals.length > 0 ? (
                <div style={{ marginBottom: 12 }}>
                  <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                    {t("watch.baseline.compare.regressions")}
                  </div>
                  <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, color: "var(--text-2)" }}>
                    {regressionSignals.map((s, i) => (
                      <li key={`r-${i}`}>{s}</li>
                    ))}
                  </ul>
                </div>
              ) : null}
              {improvementSignals.length > 0 ? (
                <div>
                  <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                    {t("watch.baseline.compare.improvements")}
                  </div>
                  <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, color: "var(--text-2)" }}>
                    {improvementSignals.map((s, i) => (
                      <li key={`i-${i}`}>{s}</li>
                    ))}
                  </ul>
                </div>
              ) : null}
            </>
          )}
        </div>

        <div style={{ padding: "14px 20px", borderTop: "1px solid var(--border)", display: "flex", justifyContent: "flex-end" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onClose} disabled={loading}>
            {t("common.close")}
          </button>
        </div>
      </div>
    </div>
  );
}
