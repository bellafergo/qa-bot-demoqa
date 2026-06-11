import React, { useCallback, useEffect, useState } from "react";
import {
  getServiceNowStatus,
  listServiceNowChanges,
  listServiceNowCMDB,
  listServiceNowIncidents,
  listServiceNowServices,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import ServiceNowConnectionCard from "./ServiceNowConnectionCard.jsx";
import ServiceNowIncidentCard from "./ServiceNowIncidentCard.jsx";
import ServiceNowChangeCard from "./ServiceNowChangeCard.jsx";
import { buildServiceNowIntegrationViewModel } from "../../utils/servicenowViewUtils.js";

function ServiceNowDiscoveryListSection({ section }) {
  if (!section) return null;
  return (
    <div style={{ marginBottom: 12 }}>
      <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
        {section.label}
      </div>
      {section.showEmpty ? (
        <p style={{ fontSize: 12, color: "var(--text-2)", margin: 0 }}>{section.emptyText}</p>
      ) : (
        <div style={{ display: "grid", gap: 6 }}>
          {section.items.map((item) => (
            <div
              key={item.key}
              style={{
                padding: "8px 10px",
                borderRadius: 6,
                border: "1px solid var(--border)",
                background: "var(--bg-2)",
                fontSize: 12,
                display: "flex",
                justifyContent: "space-between",
                gap: 8,
                flexWrap: "wrap",
              }}
            >
              <span style={{ fontFamily: "monospace", fontWeight: 600 }}>{item.key}</span>
              <span style={{ color: "var(--text-2)" }}>{item.label}</span>
              {item.meta ? <span style={{ color: "var(--text-3)", fontSize: 11 }}>{item.meta}</span> : null}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default function ServiceNowIntegrationPanel({ refreshToken = 0 }) {
  const { t } = useLang();
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [status, setStatus] = useState(null);
  const [incidents, setIncidents] = useState([]);
  const [changes, setChanges] = useState([]);
  const [services, setServices] = useState([]);
  const [cmdbItems, setCmdbItems] = useState([]);

  const load = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const [st, inc, ch, svc, cmdb] = await Promise.all([
        getServiceNowStatus(),
        listServiceNowIncidents(),
        listServiceNowChanges(),
        listServiceNowServices(),
        listServiceNowCMDB(),
      ]);
      setStatus(st);
      setIncidents(Array.isArray(inc?.incidents) ? inc.incidents : []);
      setChanges(Array.isArray(ch?.changes) ? ch.changes : []);
      setServices(Array.isArray(svc?.services) ? svc.services : []);
      setCmdbItems(Array.isArray(cmdb?.items) ? cmdb.items : []);
    } catch (e) {
      setError(e.message || t("integrations.servicenow.load_error"));
      setStatus(null);
      setIncidents([]);
      setChanges([]);
      setServices([]);
      setCmdbItems([]);
    } finally {
      setLoading(false);
    }
  }, [t]);

  useEffect(() => {
    load();
  }, [load, refreshToken]);

  const vm = buildServiceNowIntegrationViewModel({
    status,
    incidents,
    changes,
    services,
    cmdbItems,
    t,
  });

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 10 }}>
        <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)" }}>
          {t("integrations.servicenow.discovery_title")}
        </div>
        <button type="button" className="btn btn-sm" onClick={load} disabled={loading}>
          {loading ? "…" : vm.refreshLabel}
        </button>
      </div>

      {error ? (
        <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>{error}</div>
      ) : null}

      {loading && !status ? (
        <div style={{ fontSize: 12, color: "var(--text-2)", padding: "8px 0" }}>{vm.loadingLabel}</div>
      ) : (
        <ServiceNowConnectionCard vm={vm.connection} />
      )}

      {!loading && vm.connection.connected ? (
        <>
          <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
            {vm.incidentsLabel}
          </div>
          {vm.showEmptyIncidents ? (
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 12px" }}>{vm.emptyIncidentsText}</p>
          ) : (
            <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
              {vm.incidents.map((inc) => (
                <ServiceNowIncidentCard key={inc.number} vm={inc} />
              ))}
            </div>
          )}

          <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
            {vm.changesLabel}
          </div>
          {vm.showEmptyChanges ? (
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 12px" }}>{vm.emptyChangesText}</p>
          ) : (
            <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
              {vm.changes.map((ch) => (
                <ServiceNowChangeCard key={ch.number} vm={ch} />
              ))}
            </div>
          )}

          <ServiceNowDiscoveryListSection section={vm.services} />
          <ServiceNowDiscoveryListSection section={vm.cmdb} />
        </>
      ) : null}
    </div>
  );
}
