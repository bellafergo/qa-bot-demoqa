import React, { useEffect, useState } from "react";

export default function SSOProviderCard({
  provider,
  onValidate,
  onGenerateLoginUrl,
  validating = false,
  generating = false,
  validationMessage = "",
  loginUrl = "",
}) {
  const [clientId, setClientId] = useState(provider?.clientId || "");
  const [tenantId, setTenantId] = useState(provider?.tenantId || "");
  const [issuer, setIssuer] = useState(provider?.issuer || "");

  useEffect(() => {
    setClientId(provider?.clientId || "");
    setTenantId(provider?.tenantId || "");
    setIssuer(provider?.issuer || "");
  }, [provider?.clientId, provider?.tenantId, provider?.issuer]);

  if (!provider) return null;

  return (
    <li
      style={{
        marginBottom: 12,
        padding: "12px 14px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-2)",
        listStyle: "none",
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
        <span style={{ fontWeight: 600, fontSize: 14 }}>{provider.providerLabel}</span>
        <div style={{ display: "flex", gap: 6, flexWrap: "wrap" }}>
          <span className={provider.configuredBadgeClass}>{provider.configuredLabel}</span>
          <span className={provider.validatedBadgeClass}>{provider.validatedLabel}</span>
        </div>
      </div>

      <div style={{ display: "grid", gap: 8, marginBottom: 10 }}>
        <label style={{ fontSize: 12 }}>
          <div style={{ color: "var(--text-3)", marginBottom: 4 }}>{provider.clientIdLabel}</div>
          <input
            className="input"
            value={clientId}
            onChange={(e) => setClientId(e.target.value)}
            placeholder={provider.clientIdLabel}
          />
        </label>
        {provider.showTenantId ? (
          <label style={{ fontSize: 12 }}>
            <div style={{ color: "var(--text-3)", marginBottom: 4 }}>{provider.tenantIdLabel}</div>
            <input
              className="input"
              value={tenantId}
              onChange={(e) => setTenantId(e.target.value)}
              placeholder={provider.tenantIdLabel}
            />
          </label>
        ) : null}
        {provider.showIssuer ? (
          <label style={{ fontSize: 12 }}>
            <div style={{ color: "var(--text-3)", marginBottom: 4 }}>{provider.issuerLabel}</div>
            <input
              className="input"
              value={issuer}
              onChange={(e) => setIssuer(e.target.value)}
              placeholder={provider.issuerLabel}
            />
          </label>
        ) : null}
      </div>

      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: validationMessage || loginUrl ? 10 : 0 }}>
        <button
          type="button"
          className="btn btn-secondary btn-sm"
          disabled={validating}
          onClick={() => onValidate?.({
            provider: provider.provider,
            client_id: clientId,
            tenant_id: tenantId,
            issuer,
            enabled: true,
          })}
        >
          {provider.validateLabel}
        </button>
        <button
          type="button"
          className="btn btn-secondary btn-sm"
          disabled={generating || !provider.validated}
          onClick={() => onGenerateLoginUrl?.(provider.provider)}
        >
          {provider.generateLoginUrlLabel}
        </button>
      </div>

      {validationMessage ? (
        <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 8px", lineHeight: 1.45 }}>
          {validationMessage}
        </p>
      ) : null}

      {loginUrl ? (
        <div style={{ fontSize: 12 }}>
          <div style={{ color: "var(--text-3)", marginBottom: 4 }}>{provider.loginUrlLabel}</div>
          <code style={{ display: "block", wordBreak: "break-all", lineHeight: 1.45 }}>{loginUrl}</code>
        </div>
      ) : null}
    </li>
  );
}
