import React, { useCallback, useEffect, useMemo, useState } from "react";
import { getAuditEvents, getAuditSummary, apiErrorMessage } from "../../api";
import { useLang } from "../../i18n/LangContext";
import { useRbac } from "../../auth/RbacContext.jsx";
import { buildAuditTrailViewModel } from "../../utils/auditViewUtils.js";
import { PERMISSION_I18N_KEYS } from "../../utils/permissionViewUtils.js";
import AuditEventsView from "./AuditEventsView.jsx";

export default function AuditTrailSection() {
  const { t } = useLang();
  const { hasPermission, loading: rbacLoading } = useRbac();
  const canViewAudit = hasPermission("MANAGE_SECURITY");

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
    if (rbacLoading) return;
    if (!canViewAudit) {
      setLoading(false);
      setError("");
      setEvents(null);
      setSummary(null);
      return;
    }
    load(selectedEventType);
  }, [load, selectedEventType, canViewAudit, rbacLoading]);

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

      {rbacLoading ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>…</div>
      ) : !canViewAudit ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
          {t(PERMISSION_I18N_KEYS.deniedManageSecurity)}
        </p>
      ) : loading ? (
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
