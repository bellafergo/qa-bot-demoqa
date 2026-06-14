import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import {
  getBrowserWatchMetrics,
  getBrowserWatchEventsPage,
  apiErrorMessage,
} from "../../api";
import BaselineComparisonModal from "./BaselineComparisonModal.jsx";
import BaselineHistoryPanel from "./BaselineHistoryPanel.jsx";
import BaselineSummaryCard from "./BaselineSummaryCard.jsx";
import { effectiveEventRunOrigin } from "../../lib/browserWatchUi.js";
import {
  BROWSER_WATCH_I18N_KEYS,
  compareModeLabel,
  executionModeLabel,
  formatEventTimelineLabel,
  formatRelativeTime,
  formatWatchTimestamp,
  resolveWatchHealthBucket,
  resolveWatchName,
  watchStatusBadgeClass,
  watchStatusLabelKey,
} from "../../utils/browserWatchViewUtils.js";

function shortRunId(id) {
  if (id == null || id === "" || id === "_") return "";
  const s = String(id);
  return s.length <= 10 ? s : `${s.slice(0, 8)}…`;
}

function InspectionRunRef({ id, t, showToast }) {
  if (!id || id === "_") {
    return <span style={{ color: "var(--text-3)" }}>—</span>;
  }
  const to = `/evidence/run/${encodeURIComponent(id)}`;
  const short = shortRunId(id);
  const copy = async (e) => {
    e.preventDefault();
    e.stopPropagation();
    try {
      await navigator.clipboard.writeText(String(id));
      showToast(t("watch.copy_ok"), "success");
    } catch {
      showToast(t("watch.copy_fail"), "error");
    }
  };
  return (
    <span style={{ display: "inline-flex", alignItems: "center", gap: 4, flexWrap: "wrap" }}>
      <Link to={to} style={{ fontFamily: "monospace", fontSize: 11 }} title={String(id)}>
        {short}
      </Link>
      <button
        type="button"
        className="btn btn-secondary btn-sm"
        style={{ padding: "1px 6px", fontSize: 10, minHeight: 22 }}
        onClick={copy}
        title={t("watch.copy_id")}
      >
        ⧉
      </button>
    </span>
  );
}

export default function BrowserWatchDetailPanel({
  watchId,
  detailVersion,
  selectedWatch,
  busy,
  t,
  showToast,
  onRunNow,
  onEdit,
  onBaseline,
  onToggleEnabled,
}) {
  const [metrics, setMetrics] = useState(null);
  const [mLoading, setMLoading] = useState(false);
  const [events, setEvents] = useState([]);
  const [nextCursor, setNextCursor] = useState(null);
  const [eLoading, setELoading] = useState(false);
  const [eMoreLoading, setEMoreLoading] = useState(false);
  const [compareOpen, setCompareOpen] = useState(false);

  useEffect(() => {
    if (!watchId) {
      setMetrics(null);
      setEvents([]);
      setNextCursor(null);
      return;
    }
    let cancelled = false;
    (async () => {
      setMLoading(true);
      setELoading(true);
      setEvents([]);
      setNextCursor(null);
      try {
        const [m, evp] = await Promise.all([
          getBrowserWatchMetrics(watchId),
          getBrowserWatchEventsPage(watchId, { limit: 25 }),
        ]);
        if (cancelled) return;
        setMetrics(m);
        setEvents(Array.isArray(evp?.items) ? evp.items : []);
        setNextCursor(evp?.next_cursor ?? null);
      } catch (e) {
        if (!cancelled) {
          setMetrics(null);
          setEvents([]);
          showToast(apiErrorMessage(e), "error");
        }
      } finally {
        if (!cancelled) {
          setMLoading(false);
          setELoading(false);
        }
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [watchId, detailVersion, showToast]);

  const loadMore = async () => {
    if (!watchId || !nextCursor) return;
    setEMoreLoading(true);
    try {
      const evp = await getBrowserWatchEventsPage(watchId, { limit: 25, cursor: nextCursor });
      const more = Array.isArray(evp?.items) ? evp.items : [];
      setEvents((prev) => [...prev, ...more]);
      setNextCursor(evp?.next_cursor ?? null);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setEMoreLoading(false);
    }
  };

  if (!watchId) {
    return (
      <div
        className="browser-watch-layout__detail"
        style={{
          border: "1px dashed var(--border)",
          borderRadius: 8,
          padding: 20,
          color: "var(--text-3)",
          fontSize: 13,
        }}
      >
        {t(BROWSER_WATCH_I18N_KEYS.detailSelect)}
      </div>
    );
  }

  const bucket = resolveWatchHealthBucket(selectedWatch);
  const isBusy = busy?.has?.(watchId);

  return (
    <div className="browser-watch-layout__detail">
      <div className="card" style={{ padding: 16 }}>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap", marginBottom: 14 }}>
          <div>
            <div style={{ fontSize: 16, fontWeight: 700, color: "var(--text-1)" }}>{resolveWatchName(selectedWatch)}</div>
            <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4, wordBreak: "break-all" }} title={selectedWatch?.url}>
              {selectedWatch?.url || "—"}
            </div>
          </div>
          <span className={`badge ${watchStatusBadgeClass(bucket)}`} style={{ fontSize: 10 }}>
            {t(watchStatusLabelKey(bucket))}
          </span>
        </div>

        <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginBottom: 16 }}>
          <button type="button" className="btn btn-primary btn-sm" disabled={isBusy} onClick={() => onRunNow?.(selectedWatch)}>
            {t(BROWSER_WATCH_I18N_KEYS.actionRunNow)}
          </button>
          <button type="button" className="btn btn-secondary btn-sm" disabled={isBusy} onClick={() => onEdit?.(selectedWatch)}>
            {t(BROWSER_WATCH_I18N_KEYS.actionEdit)}
          </button>
          <button type="button" className="btn btn-secondary btn-sm" disabled={isBusy} onClick={() => onBaseline?.(watchId)}>
            {t(BROWSER_WATCH_I18N_KEYS.actionBaseline)}
          </button>
          <button type="button" className="btn btn-secondary btn-sm" disabled={isBusy} onClick={() => onToggleEnabled?.(selectedWatch)}>
            {selectedWatch?.enabled ? t(BROWSER_WATCH_I18N_KEYS.actionDisable) : t(BROWSER_WATCH_I18N_KEYS.actionEnable)}
          </button>
        </div>

        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 10 }}>
          {t(BROWSER_WATCH_I18N_KEYS.detailInfo)}
        </div>
        <dl style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: "8px 12px", margin: "0 0 16px", fontSize: 13 }}>
          <dt style={{ color: "var(--text-3)" }}>{t("watch.detail.execution")}</dt>
          <dd style={{ margin: 0, fontWeight: 600 }}>{executionModeLabel(selectedWatch?.execution_mode, t)}</dd>
          <dt style={{ color: "var(--text-3)" }}>{t(BROWSER_WATCH_I18N_KEYS.detailBaselineMode)}</dt>
          <dd style={{ margin: 0 }}>{compareModeLabel(selectedWatch?.compare_mode, t)}</dd>
          <dt style={{ color: "var(--text-3)" }}>{t("watch.col.last_run")}</dt>
          <dd style={{ margin: 0 }}>{formatRelativeTime(metrics?.last_run_at || selectedWatch?.last_run_at)}</dd>
          <dt style={{ color: "var(--text-3)" }}>{t("watch.col.alert")}</dt>
          <dd style={{ margin: 0 }}>{formatRelativeTime(metrics?.last_alert_at || selectedWatch?.last_alert_at)}</dd>
          <dt style={{ color: "var(--text-3)" }}>{t(BROWSER_WATCH_I18N_KEYS.detailCurrentStatus)}</dt>
          <dd style={{ margin: 0 }}>{metrics?.current_status || selectedWatch?.current_status || selectedWatch?.last_status || "—"}</dd>
        </dl>

        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 10 }}>
          {t("watch.detail.metrics")}
        </div>
        {mLoading ? (
          <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("watch.detail.loading_m")}</div>
        ) : (
          <div style={{ display: "grid", gridTemplateColumns: "repeat(3, 1fr)", gap: 10 }}>
            {[
              { label: t(BROWSER_WATCH_I18N_KEYS.detailTotalRuns), value: metrics?.total_runs ?? 0 },
              { label: t(BROWSER_WATCH_I18N_KEYS.detailDiffsFound), value: metrics?.total_diffs ?? 0 },
              { label: t(BROWSER_WATCH_I18N_KEYS.detailAlertsTriggered), value: metrics?.alerts_triggered ?? 0 },
            ].map((m) => (
              <div
                key={m.label}
                style={{
                  padding: "10px 12px",
                  borderRadius: 8,
                  border: "1px solid var(--border)",
                  background: "var(--bg-2)",
                }}
              >
                <div style={{ fontSize: 10, color: "var(--text-3)", marginBottom: 4 }}>{m.label}</div>
                <div style={{ fontSize: 18, fontWeight: 800, color: "var(--text-1)" }}>{m.value}</div>
              </div>
            ))}
          </div>
        )}

        {selectedWatch?.last_inspection_id ? (
          <div style={{ marginTop: 12, fontSize: 11, color: "var(--text-3)", display: "flex", flexWrap: "wrap", gap: 12 }}>
            <span>
              {t("watch.col.last_insp")}: <InspectionRunRef id={selectedWatch.last_inspection_id} t={t} showToast={showToast} />
            </span>
          </div>
        ) : null}
      </div>

      <BaselineSummaryCard
        watch={selectedWatch}
        busy={isBusy}
        t={t}
        showToast={showToast}
        onChangeBaseline={() => onBaseline?.(watchId)}
        onCompare={() => setCompareOpen(true)}
      />
      <BaselineHistoryPanel watchId={watchId} detailVersion={detailVersion} t={t} showToast={showToast} />
      <BaselineComparisonModal
        open={compareOpen}
        onClose={() => setCompareOpen(false)}
        watch={selectedWatch}
        t={t}
        showToast={showToast}
      />

      <div className="card" style={{ padding: 16, marginTop: 12 }}>
        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 12 }}>
          {t(BROWSER_WATCH_I18N_KEYS.detailTimeline)}
        </div>
        {eLoading && <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("watch.detail.loading_e")}</div>}
        {!eLoading && events.length === 0 && (
          <div style={{ fontSize: 13, color: "var(--text-2)" }}>{t("watch.detail.no_events")}</div>
        )}
        {!eLoading && events.length > 0 && (
          <ul style={{ listStyle: "none", margin: 0, padding: 0, maxHeight: 400, overflowY: "auto" }}>
            {events.map((e, idx) => (
              <li
                key={e.event_id}
                style={{
                  display: "grid",
                  gridTemplateColumns: "72px 1fr",
                  gap: 12,
                  padding: "10px 0",
                  borderBottom: idx < events.length - 1 ? "1px solid var(--border)" : undefined,
                }}
              >
                <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", whiteSpace: "nowrap" }}>
                  {formatWatchTimestamp(e.created_at)}
                </div>
                <div>
                  <div style={{ fontSize: 13, color: "var(--text-1)", lineHeight: 1.45 }}>
                    {formatEventTimelineLabel(e, t)}
                  </div>
                  {(() => {
                    const ro = effectiveEventRunOrigin(e, selectedWatch);
                    if (!ro) return null;
                    const isLocal = ro === "local_agent";
                    return (
                      <span
                        className={`badge ${isLocal ? "badge-orange" : "badge-gray"}`}
                        style={{ fontSize: 9, marginTop: 6, display: "inline-block" }}
                      >
                        {isLocal ? t("watch.ev.badge_local") : t("watch.ev.badge_cloud")}
                      </span>
                    );
                  })()}
                </div>
              </li>
            ))}
          </ul>
        )}
        {!eLoading && nextCursor ? (
          <button
            type="button"
            className="btn btn-secondary btn-sm"
            style={{ marginTop: 10 }}
            disabled={eMoreLoading}
            onClick={loadMore}
          >
            {eMoreLoading ? t("common.working") : t("watch.detail.load_more")}
          </button>
        ) : null}
      </div>
    </div>
  );
}
