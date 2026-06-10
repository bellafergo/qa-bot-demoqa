import React, { useCallback, useState } from "react";
import { useLang } from "../../i18n/LangContext.jsx";
import { DATABASE_CONNECTOR_I18N_KEYS } from "../../utils/databaseConnectorViewUtils.js";

export default function DatabaseValidationExecuteModal({
  open,
  payload,
  onClose,
  onSimulateApprove,
  onExecute,
  busy,
}) {
  const { t } = useLang();
  const [result, setResult] = useState(null);

  const handleExecute = useCallback(async () => {
    if (!onExecute) return;
    const execution = await onExecute();
    if (execution) setResult(execution);
  }, [onExecute]);

  if (!open || !payload) return null;

  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex: 10050,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
      aria-labelledby="database-validation-execute-title"
    >
      <div className="card" style={{ width: "min(640px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="database-validation-execute-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {payload.title}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>{payload.readOnlyNote}</div>
        </div>
        <div style={{ padding: "16px 20px" }}>
          <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 10 }}>
            <strong>{t(DATABASE_CONNECTOR_I18N_KEYS.check)}:</strong> {payload.checkName}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 10 }}>
            <strong>{t(DATABASE_CONNECTOR_I18N_KEYS.name)}:</strong> {payload.connectionName} ({payload.connectionLabel})
          </div>
          {payload.approvalRequired ? (
            <div className="alert alert-warning" style={{ fontSize: 13, marginBottom: 12 }}>
              {payload.approvalRequiredLabel}
            </div>
          ) : null}
          {result ? (
            <div style={{ marginBottom: 12 }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {payload.resultLabel}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55 }}>{result.summary}</div>
              <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>
                {t(DATABASE_CONNECTOR_I18N_KEYS.status)}: {result.status} · {result.row_count} row(s)
              </div>
            </div>
          ) : null}
        </div>
        <div
          style={{
            padding: "12px 20px",
            borderTop: "1px solid var(--border)",
            display: "flex",
            gap: 8,
            justifyContent: "flex-end",
            flexWrap: "wrap",
          }}
        >
          <button type="button" className="btn btn-secondary btn-sm" onClick={onClose} disabled={busy}>
            Close
          </button>
          {payload.approvalRequired ? (
            <button type="button" className="btn btn-secondary btn-sm" onClick={onSimulateApprove} disabled={busy}>
              {payload.simulateApproveLabel}
            </button>
          ) : null}
          <button
            type="button"
            className="btn btn-primary btn-sm"
            onClick={handleExecute}
            disabled={busy || !payload.canExecute || Boolean(result)}
          >
            {payload.executeLabel}
          </button>
        </div>
      </div>
    </div>
  );
}
