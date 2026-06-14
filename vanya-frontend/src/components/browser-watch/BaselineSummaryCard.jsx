import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { apiErrorMessage, getBrowserInspection } from "../../api";
import { compareModeLabel } from "../../utils/browserWatchViewUtils.js";
import {
  canCompareBaseline,
  formatBaselineDate,
  resolveBaselineUpdatedByLabel,
  shortInspectionId,
} from "../../utils/baselineExplorerViewUtils.js";
import BaselineHealthBadge from "./BaselineHealthBadge.jsx";

export default function BaselineSummaryCard({
  watch,
  busy,
  t,
  showToast,
  onChangeBaseline,
  onCompare,
}) {
  const baselineId = String(watch?.baseline_inspection_id || "").trim();
  const hasBaseline = Boolean(baselineId && baselineId !== "_");

  const [inspection, setInspection] = useState(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (!hasBaseline) {
      setInspection(null);
      return;
    }
    let cancelled = false;
    (async () => {
      setLoading(true);
      try {
        const detail = await getBrowserInspection(baselineId);
        if (!cancelled) setInspection(detail);
      } catch (e) {
        if (!cancelled) {
          setInspection(null);
          showToast?.(apiErrorMessage(e), "error");
        }
      } finally {
        if (!cancelled) setLoading(false);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [baselineId, hasBaseline, showToast]);

  if (!hasBaseline) {
    return (
      <div className="card" style={{ padding: 16, marginTop: 12 }}>
        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 8 }}>
          {t("watch.baseline.summary.title")}
        </div>
        <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 12 }}>{t("watch.baseline.summary.empty")}</div>
        <button type="button" className="btn btn-secondary btn-sm" disabled={busy} onClick={onChangeBaseline}>
          {t("watch.baseline.action.change")}
        </button>
      </div>
    );
  }

  const evidenceTo = `/evidence/run/${encodeURIComponent(baselineId)}`;
  const inspectionStatus = inspection?.status || "—";
  const bis = inspection?.browser_inspection_summary;
  const succeeded = bis?.inspection_succeeded;
  const canCompare = canCompareBaseline(watch);

  return (
    <div className="card" style={{ padding: 16, marginTop: 12 }}>
      <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap", marginBottom: 12 }}>
        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
          {t("watch.baseline.summary.title")}
        </div>
        <BaselineHealthBadge baselineSetAt={watch?.baseline_set_at} t={t} />
      </div>

      <dl style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: "8px 12px", margin: "0 0 14px", fontSize: 13 }}>
        <dt style={{ color: "var(--text-3)" }}>{t("watch.baseline.field.id")}</dt>
        <dd style={{ margin: 0, fontFamily: "monospace", fontSize: 12 }} title={baselineId}>
          {shortInspectionId(baselineId, 16)}
        </dd>
        <dt style={{ color: "var(--text-3)" }}>{t("watch.baseline.field.set_at")}</dt>
        <dd style={{ margin: 0 }}>{formatBaselineDate(watch?.baseline_set_at)}</dd>
        <dt style={{ color: "var(--text-3)" }}>{t("watch.baseline.field.updated_by")}</dt>
        <dd style={{ margin: 0 }}>{resolveBaselineUpdatedByLabel(watch?.baseline_updated_by, t)}</dd>
        <dt style={{ color: "var(--text-3)" }}>{t("watch.baseline.field.mode")}</dt>
        <dd style={{ margin: 0 }}>{compareModeLabel(watch?.compare_mode, t)}</dd>
        <dt style={{ color: "var(--text-3)" }}>{t("watch.baseline.field.status")}</dt>
        <dd style={{ margin: 0 }}>
          {loading ? t("watch.baseline.loading") : inspectionStatus}
          {!loading && succeeded === false ? ` (${t("watch.baseline.inspection_failed")})` : null}
        </dd>
      </dl>

      <div style={{ display: "flex", flexWrap: "wrap", gap: 8 }}>
        <Link to={evidenceTo} className="btn btn-secondary btn-sm">
          {t("watch.baseline.action.evidence")}
        </Link>
        <button type="button" className="btn btn-secondary btn-sm" disabled={busy || !canCompare} onClick={onCompare}>
          {t("watch.baseline.action.compare")}
        </button>
        <button type="button" className="btn btn-secondary btn-sm" disabled={busy} onClick={onChangeBaseline}>
          {t("watch.baseline.action.change")}
        </button>
      </div>
    </div>
  );
}
