import React from "react";
import { Link } from "react-router-dom";

export default function SystemStatusRibbon({ ribbon }) {
  const { variant, headline, cta } = ribbon;
  return (
    <div
      className={`dash-system-ribbon dash-system-ribbon--${variant}`}
      role="status"
      aria-live="polite"
    >
      <div
        className="dash-system-ribbon__inner"
        style={{
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          gap: 16,
          flexWrap: "wrap",
        }}
      >
        <span
          className="dash-system-ribbon__headline"
          style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", lineHeight: 1.35 }}
        >
          {headline}
        </span>
        {cta ? (
          <Link
            to={cta.to}
            className={cta.secondary ? "btn btn-ghost btn-sm" : "btn btn-secondary btn-sm"}
            style={{ flexShrink: 0 }}
          >
            {cta.label}
          </Link>
        ) : null}
      </div>
    </div>
  );
}
