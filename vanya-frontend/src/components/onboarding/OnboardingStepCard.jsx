import React from "react";
import { Link } from "react-router-dom";

export default function OnboardingStepCard({ step }) {
  if (!step) return null;

  return (
    <li
      style={{
        marginBottom: 8,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 10, alignItems: "flex-start" }}>
        <span style={{ fontSize: 16, lineHeight: 1.2, width: 18, textAlign: "center", color: "var(--text-2)" }}>
          {step.statusIcon}
        </span>
        <div style={{ flex: 1 }}>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 4 }}>
            <strong style={{ color: "var(--text-1)" }}>{step.title}</strong>
            {step.isOptional ? (
              <span className="badge badge-blue" style={{ fontSize: 10 }}>
                {step.optionalLabel}
              </span>
            ) : null}
            <span className={step.statusBadgeClass}>{step.statusLabel}</span>
            <span className="badge badge-gray">{step.completion_percentage}%</span>
          </div>
          {step.description ? (
            <div style={{ color: "var(--text-3)", fontSize: 12, marginBottom: step.guidanceText || step.navigation ? 8 : 0 }}>
              {step.description}
            </div>
          ) : null}
          {step.guidanceText ? (
            <div style={{ color: "var(--text-2)", fontSize: 12, lineHeight: 1.55, marginBottom: step.navigation ? 8 : 0 }}>
              {step.guidanceText}
            </div>
          ) : null}
          {step.navigation ? (
            <Link to={step.navigation.path} className="btn btn-secondary btn-sm" style={{ fontSize: 11 }}>
              {step.navigation.label}
            </Link>
          ) : null}
        </div>
      </div>
    </li>
  );
}
