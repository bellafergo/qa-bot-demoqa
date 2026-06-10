import React, { useCallback, useEffect, useMemo, useState } from "react";
import {
  integrationsReadiness,
  previewReportDelivery,
  sendReportDelivery,
  apiErrorMessage,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import {
  REPORT_DELIVERY_CHANNELS,
  REPORT_DELIVERY_TYPES,
  buildReportDeliveryViewModel,
  canSendDelivery,
} from "../../utils/reportDeliveryViewUtils.js";

export default function ReportDeliveryCenter({ projectId }) {
  const { t } = useLang();
  const [readiness, setReadiness] = useState(null);
  const [reportType, setReportType] = useState(REPORT_DELIVERY_TYPES[0]);
  const [channel, setChannel] = useState(REPORT_DELIVERY_CHANNELS[0]);
  const [recipient, setRecipient] = useState("");
  const [preview, setPreview] = useState(null);
  const [loadingPreview, setLoadingPreview] = useState(false);
  const [sending, setSending] = useState(false);
  const [error, setError] = useState("");
  const [success, setSuccess] = useState("");

  const vm = useMemo(
    () => buildReportDeliveryViewModel({ readiness, t }),
    [readiness, t],
  );

  const loadReadiness = useCallback(async () => {
    try {
      const data = await integrationsReadiness();
      setReadiness(data);
    } catch {
      setReadiness({ connectors: {} });
    }
  }, []);

  useEffect(() => {
    loadReadiness();
  }, [loadReadiness]);

  const handlePreview = async () => {
    if (!projectId) return;
    setLoadingPreview(true);
    setError("");
    setSuccess("");
    try {
      const body = {
        report_type: reportType,
        channel,
        recipient: channel === "email" ? recipient.trim() || undefined : undefined,
      };
      const data = await previewReportDelivery(projectId, body);
      setPreview(data);
    } catch (err) {
      setPreview(null);
      setError(apiErrorMessage(err));
    } finally {
      setLoadingPreview(false);
    }
  };

  const handleSend = async () => {
    if (!projectId) return;
    setSending(true);
    setError("");
    setSuccess("");
    try {
      const body = {
        report_type: reportType,
        channel,
        recipient: channel === "email" ? recipient.trim() || undefined : undefined,
        recipients: channel === "email" && recipient.trim() ? [recipient.trim()] : undefined,
        requires_user_approval: true,
      };
      const result = await sendReportDelivery(projectId, body);
      if (result?.success) {
        setSuccess(vm.sendSuccess);
      } else {
        setError(result?.error || vm.sendFailed);
      }
    } catch (err) {
      setError(apiErrorMessage(err));
    } finally {
      setSending(false);
    }
  };

  const sendEnabled = canSendDelivery({ channel, readiness, recipient, sending });

  if (!projectId) return null;

  if (vm.empty) {
    return (
      <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {vm.emptyMessage}
      </p>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(180px, 1fr))", gap: 12 }}>
        <label style={{ display: "flex", flexDirection: "column", gap: 6, fontSize: 12 }}>
          <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{vm.reportTypeLabel}</span>
          <select
            className="input"
            value={reportType}
            onChange={(e) => setReportType(e.target.value)}
          >
            {vm.reportTypes.map((opt) => (
              <option key={opt.id} value={opt.id}>{opt.label}</option>
            ))}
          </select>
        </label>

        <label style={{ display: "flex", flexDirection: "column", gap: 6, fontSize: 12 }}>
          <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{vm.channelLabel}</span>
          <select
            className="input"
            value={channel}
            onChange={(e) => setChannel(e.target.value)}
          >
            {vm.channels.map((opt) => (
              <option key={opt.id} value={opt.id}>
                {opt.label}{opt.ready ? "" : ` (${vm.channelNotReady})`}
              </option>
            ))}
          </select>
        </label>

        {channel === "email" ? (
          <label style={{ display: "flex", flexDirection: "column", gap: 6, fontSize: 12 }}>
            <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{vm.recipientLabel}</span>
            <input
              className="input"
              type="email"
              value={recipient}
              onChange={(e) => setRecipient(e.target.value)}
              placeholder="executive@company.com"
            />
          </label>
        ) : null}
      </div>

      <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
        <button
          type="button"
          className="btn btn-secondary btn-sm"
          onClick={handlePreview}
          disabled={loadingPreview}
        >
          {loadingPreview ? "…" : vm.previewLabel}
        </button>
        <button
          type="button"
          className="btn btn-primary btn-sm"
          onClick={handleSend}
          disabled={!sendEnabled}
        >
          {sending ? "…" : vm.sendLabel}
        </button>
      </div>

      <p style={{ fontSize: 12, color: "var(--text-3)", margin: 0, fontStyle: "italic" }}>
        {vm.requiresApprovalNote}
      </p>

      {error ? (
        <div className="alert alert-error" style={{ fontSize: 13 }}>{error}</div>
      ) : null}
      {success ? (
        <div className="alert alert-success" style={{ fontSize: 13 }}>{success}</div>
      ) : null}

      {preview ? (
        <div
          style={{
            padding: "12px 14px",
            background: "var(--bg-3, rgba(255,255,255,0.03))",
            borderRadius: 8,
            fontSize: 13,
            lineHeight: 1.5,
          }}
        >
          <div style={{ fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.previewTitle}
          </div>
          <div style={{ marginBottom: 6 }}>
            <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{vm.subjectLabel}: </span>
            <span style={{ color: "var(--text-2)" }}>{preview.subject}</span>
          </div>
          <div style={{ marginBottom: 6 }}>
            <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{vm.summaryLabel}: </span>
            <span style={{ color: "var(--text-2)" }}>{preview.summary}</span>
          </div>
          <pre
            style={{
              margin: "8px 0 0",
              padding: 10,
              background: "var(--bg-2)",
              borderRadius: 6,
              fontSize: 12,
              overflow: "auto",
              whiteSpace: "pre-wrap",
            }}
          >
            {JSON.stringify(preview.payload, null, 2)}
          </pre>
        </div>
      ) : null}
    </div>
  );
}
