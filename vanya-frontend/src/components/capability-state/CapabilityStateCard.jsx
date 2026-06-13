import React from "react";
import { Link } from "react-router-dom";
import { useLang } from "../../i18n/LangContext";
import { CAPABILITY_STATE, CAPABILITY_STATE_I18N_KEYS } from "../../utils/capabilityStateViewUtils.js";

export default function CapabilityStateCard({ state }) {
  const { t } = useLang();
  if (!state || state.state === CAPABILITY_STATE.AVAILABLE) return null;

  const isIntegration = state.state === CAPABILITY_STATE.INTEGRATION_REQUIRED;

  return (
    <div
      className="card"
      style={{
        padding: "20px 22px",
        border: "1px solid var(--border)",
        background: "var(--bg-2)",
        opacity: isIntegration ? 0.98 : 1,
      }}
    >
      <div style={{ display: "flex", gap: 12, alignItems: "flex-start", flexWrap: "wrap" }}>
        <div style={{ fontSize: 22, lineHeight: 1 }} aria-hidden>
          {state.icon}
        </div>
        <div style={{ flex: "1 1 240px", minWidth: 0 }}>
          <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text-1)", marginBottom: 6 }}>
            {state.title}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: state.benefits?.length ? 12 : 10 }}>
            {state.description}
          </div>

          {state.benefits?.length ? (
            <div style={{ marginBottom: 14 }}>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em", marginBottom: 6 }}>
                {t(CAPABILITY_STATE_I18N_KEYS.integrationBenefits)}
              </div>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.5 }}>
                {state.benefits.map((benefit) => (
                  <li key={benefit} style={{ marginBottom: 4 }}>{benefit}</li>
                ))}
              </ul>
            </div>
          ) : null}

          {state.statusLabel ? (
            <div
              style={{
                padding: "10px 12px",
                borderRadius: 8,
                background: "var(--bg-1, rgba(0,0,0,0.12))",
                fontSize: 12,
                color: "var(--text-2)",
                marginBottom: 14,
              }}
            >
              <div style={{ fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>{state.statusLabel}</div>
              <div style={{ fontWeight: 600, color: "var(--text-1)" }}>{state.statusValue}</div>
            </div>
          ) : null}

          {state.cta ? (
            <Link
              to={state.cta.path}
              className="btn btn-primary btn-sm"
              style={{ textDecoration: "none", display: "inline-flex" }}
            >
              {state.cta.label}
            </Link>
          ) : null}
        </div>
      </div>
    </div>
  );
}
