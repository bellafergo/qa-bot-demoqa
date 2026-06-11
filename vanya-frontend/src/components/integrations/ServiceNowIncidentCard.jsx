import React from "react";
import { useLang } from "../../i18n/LangContext";
import { SERVICENOW_I18N_KEYS } from "../../utils/servicenowViewUtils.js";

export default function ServiceNowIncidentCard({ vm }) {
  const { t } = useLang();
  if (!vm) return null;

  return (
    <div
      style={{
        padding: "10px 12px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-2)",
        fontSize: 12,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap" }}>
        <span style={{ fontFamily: "monospace", fontWeight: 600 }}>{vm.number}</span>
        <span className="badge badge-blue">{vm.state}</span>
      </div>
      <div style={{ marginTop: 6, fontWeight: 500 }}>{vm.shortDescription}</div>
      <div style={{ marginTop: 8, color: "var(--text-3)", display: "flex", gap: 12, flexWrap: "wrap" }}>
        <span>{t(SERVICENOW_I18N_KEYS.priority)}: {vm.priority}</span>
        <span>{t(SERVICENOW_I18N_KEYS.assignmentGroup)}: {vm.assignmentGroup}</span>
        {vm.openedAt ? <span>{t(SERVICENOW_I18N_KEYS.openedAt)}: {vm.openedAt}</span> : null}
      </div>
    </div>
  );
}
