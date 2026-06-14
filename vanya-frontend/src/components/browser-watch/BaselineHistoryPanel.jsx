import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { apiErrorMessage, getBrowserWatchEventsPage } from "../../api";
import {
  buildBaselineHistoryRow,
  filterBaselineSetEvents,
  formatBaselineDate,
  shortInspectionId,
} from "../../utils/baselineExplorerViewUtils.js";

export default function BaselineHistoryPanel({ watchId, detailVersion, t, showToast }) {
  const [rows, setRows] = useState([]);
  const [loading, setLoading] = useState(false);
  const [nextCursor, setNextCursor] = useState(null);
  const [moreLoading, setMoreLoading] = useState(false);

  useEffect(() => {
    if (!watchId) {
      setRows([]);
      setNextCursor(null);
      return;
    }
    let cancelled = false;
    (async () => {
      setLoading(true);
      setRows([]);
      setNextCursor(null);
      try {
        const page = await getBrowserWatchEventsPage(watchId, { limit: 50 });
        if (cancelled) return;
        const baselineEvents = filterBaselineSetEvents(page?.items).map(buildBaselineHistoryRow);
        setRows(baselineEvents);
        setNextCursor(page?.next_cursor ?? null);
      } catch (e) {
        if (!cancelled) showToast?.(apiErrorMessage(e), "error");
      } finally {
        if (!cancelled) setLoading(false);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [watchId, detailVersion, showToast]);

  const loadMore = async () => {
    if (!watchId || !nextCursor) return;
    setMoreLoading(true);
    try {
      const page = await getBrowserWatchEventsPage(watchId, { limit: 50, cursor: nextCursor });
      const more = filterBaselineSetEvents(page?.items).map(buildBaselineHistoryRow);
      setRows((prev) => [...prev, ...more]);
      setNextCursor(page?.next_cursor ?? null);
    } catch (e) {
      showToast?.(apiErrorMessage(e), "error");
    } finally {
      setMoreLoading(false);
    }
  };

  return (
    <div className="card" style={{ padding: 16, marginTop: 12 }}>
      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 12 }}>
        {t("watch.baseline.history.title")}
      </div>

      {loading && <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("watch.baseline.history.loading")}</div>}
      {!loading && rows.length === 0 && (
        <div style={{ fontSize: 13, color: "var(--text-2)" }}>{t("watch.baseline.history.empty")}</div>
      )}

      {!loading && rows.length > 0 && (
        <ul style={{ listStyle: "none", margin: 0, padding: 0 }}>
          {rows.map((row, idx) => {
            const inspId = row.inspectionId;
            const hasId = inspId && inspId !== "_";
            return (
              <li
                key={row.eventId || `${row.createdAt}-${idx}`}
                style={{
                  display: "grid",
                  gridTemplateColumns: "1fr auto",
                  gap: 8,
                  alignItems: "center",
                  padding: "10px 0",
                  borderBottom: idx < rows.length - 1 ? "1px solid var(--border)" : undefined,
                }}
              >
                <div>
                  <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>
                    {formatBaselineDate(row.createdAt)}
                  </div>
                  <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 2 }}>
                    {t("watch.baseline.history.row_label")}{" "}
                    <span style={{ fontFamily: "monospace" }} title={hasId ? String(inspId) : undefined}>
                      {hasId ? shortInspectionId(inspId, 12) : "—"}
                    </span>
                  </div>
                </div>
                {hasId ? (
                  <Link
                    to={`/evidence/run/${encodeURIComponent(inspId)}`}
                    className="btn btn-secondary btn-sm"
                    style={{ whiteSpace: "nowrap" }}
                  >
                    {t("watch.baseline.action.evidence")}
                  </Link>
                ) : null}
              </li>
            );
          })}
        </ul>
      )}

      {!loading && nextCursor ? (
        <button
          type="button"
          className="btn btn-secondary btn-sm"
          style={{ marginTop: 10 }}
          disabled={moreLoading}
          onClick={loadMore}
        >
          {moreLoading ? t("common.working") : t("watch.detail.load_more")}
        </button>
      ) : null}
    </div>
  );
}
