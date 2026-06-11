import React, { useCallback, useEffect, useMemo, useState } from "react";
import { getAuditEvents, getAuditSummary, apiErrorMessage } from "../../api";
import { useLang } from "../../i18n/LangContext";
import { buildAuditTrailViewModel } from "../../utils/auditViewUtils.js";
import AuditEventsView from "./AuditEventsView.jsx";

export default function AuditTrailSection() {
  const { t } = useLang();
  const [events, setEvents] = useState(null);
  const [summary, setSummary] = useState(null);
  const [selectedEventType, setSelectedEventType] = useState("");
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const load = useCallback(async (eventType = "") => {
    setLoading(true);
    setError("");
    try {
      const params = { limit: 50 };
      if (eventType) params.event_type = eventType;
      const [eventsData, summaryData] = await Promise.all([
        getAuditEvents(params),
        getAuditSummary(),
      ]);
      setEvents(eventsData);
      setSummary(summaryData);
    } catch (err) {
      setError(apiErrorMessage(err));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    load(selectedEventType);
  }, [load, selectedEventType]);

  const vm = useMemo(
    () => buildAuditTrailViewModel({ events, summary, selectedEventType, t }),
    [events, summary, selectedEventType, t],
  );

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{vm.title}</div>
      <p style={{ margin: "0 0 14px", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
        {vm.subtitle}
      </p>

      {loading ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>…</div>
      ) : error ? (
        <div className="alert alert-error" style={{ fontSize: 12 }}>{error}</div>
      ) : (
        <AuditEventsView vm={vm} onFilterChange={setSelectedEventType} />
      )}

      <p style={{ margin: "12px 0 0", fontSize: 11, color: "var(--text-3)", fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
