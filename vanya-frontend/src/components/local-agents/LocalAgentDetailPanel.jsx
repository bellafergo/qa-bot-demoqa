import React from "react";

export default function LocalAgentDetailPanel({
  vm,
  loading,
  busy,
  t,
  onRefresh,
  onDisable,
  selectLabel,
}) {
  if (!vm && !loading) {
    return (
      <div
        className="local-agents-layout__detail"
        style={{
          border: "1px dashed var(--border)",
          borderRadius: 8,
          padding: 20,
          color: "var(--text-3)",
          fontSize: 13,
        }}
      >
        {selectLabel}
      </div>
    );
  }

  if (loading) {
    return (
      <div className="local-agents-layout__detail card" style={{ padding: 16, fontSize: 13, color: "var(--text-3)" }}>
        {t("localAgents.loading")}
      </div>
    );
  }

  if (!vm) return null;

  return (
    <div className="local-agents-layout__detail card" style={{ padding: 16 }}>
      <div style={{ display: "flex", justifyContent: "space-between", gap: 12, alignItems: "flex-start", marginBottom: 14 }}>
        <div>
          <div style={{ fontSize: 16, fontWeight: 700, color: "var(--text-1)" }}>{vm.name}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4, wordBreak: "break-all" }}>{vm.agentId}</div>
        </div>
        <span className={vm.status.badgeClass} style={{ fontSize: 10 }}>{vm.statusLabel}</span>
      </div>

      <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginBottom: 16 }}>
        <button type="button" className="btn btn-secondary btn-sm" disabled={busy} onClick={onRefresh}>
          {t("localAgents.refresh")}
        </button>
        <button
          type="button"
          className="btn btn-secondary btn-sm"
          style={{ color: "var(--red)" }}
          disabled={busy}
          onClick={onDisable}
        >
          {t("localAgents.action.disable")}
        </button>
      </div>

      <dl style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: "8px 12px", margin: "0 0 16px", fontSize: 13 }}>
        <dt style={{ color: "var(--text-3)" }}>{vm.environmentLabel}</dt>
        <dd style={{ margin: 0, fontWeight: 600 }}>{vm.environment}</dd>
        <dt style={{ color: "var(--text-3)" }}>{vm.versionLabel}</dt>
        <dd style={{ margin: 0 }}>{vm.version}</dd>
        <dt style={{ color: "var(--text-3)" }}>{vm.lastHeartbeatLabel}</dt>
        <dd style={{ margin: 0 }}>{vm.lastHeartbeat}</dd>
      </dl>

      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 8 }}>
        {vm.capabilitiesLabel}
      </div>
      <ul style={{ margin: "0 0 16px", paddingLeft: 18, fontSize: 13, color: "var(--text-2)" }}>
        {vm.capabilities.map((label) => (
          <li key={label}>{label}</li>
        ))}
      </ul>

      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 8 }}>
        {vm.inventorySummaryLabel}
      </div>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(3, 1fr)", gap: 10, marginBottom: 16 }}>
        {[
          { label: vm.inventorySummary.dbConnectionsLabel, value: vm.inventorySummary.dbConnections },
          { label: vm.inventorySummary.recentValidationsLabel, value: vm.inventorySummary.recentValidations },
          { label: vm.inventorySummary.systemsRegisteredLabel, value: vm.inventorySummary.systemsRegistered },
        ].map((item) => (
          <div key={item.label} style={{ padding: "10px 12px", borderRadius: 8, border: "1px solid var(--border)", background: "var(--bg-2)" }}>
            <div style={{ fontSize: 10, color: "var(--text-3)" }}>{item.label}</div>
            <div style={{ fontSize: 18, fontWeight: 800, marginTop: 4 }}>{item.value}</div>
          </div>
        ))}
      </div>

      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 8 }}>
        {vm.metadataTitle}
      </div>
      <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginBottom: 16 }}>
        {vm.metadataTags.map((tag) => (
          <span key={tag.label} className="badge badge-gray" style={{ fontSize: 11 }}>
            {tag.label}: {tag.value}
          </span>
        ))}
      </div>

      {vm.recentJobs.length > 0 ? (
        <>
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("localAgents.detail.jobs")}</div>
          <ul style={{ listStyle: "none", margin: 0, padding: 0, maxHeight: 160, overflowY: "auto" }}>
            {vm.recentJobs.map((j) => (
              <li key={j.job_id} style={{ borderBottom: "1px solid var(--border)", padding: "6px 0", fontSize: 11 }}>
                <span className="badge badge-gray" style={{ fontSize: 9 }}>{j.status}</span>{" "}
                <span style={{ color: "var(--text-2)" }}>{j.target_url?.slice(0, 60) || j.job_id}</span>
              </li>
            ))}
          </ul>
        </>
      ) : null}
    </div>
  );
}
