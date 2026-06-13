import React from "react";
import { Link } from "react-router-dom";

export default function NextRecommendedActionCard({ vm }) {
  if (!vm) return null;

  return (
    <div className="card" style={{ padding: "18px 20px", marginBottom: 16 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8, textTransform: "uppercase", letterSpacing: "0.06em" }}>
        {vm.title}
      </div>
      <div style={{ display: "flex", justifyContent: "space-between", gap: 16, flexWrap: "wrap", alignItems: "center" }}>
        <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)", lineHeight: 1.45 }}>{vm.message}</div>
        {vm.action ? (
          <Link to={vm.action.path} className="btn btn-primary btn-sm">
            {vm.action.label}
          </Link>
        ) : vm.noActionRequired ? (
          <span className="badge badge-green" style={{ fontSize: 11 }}>
            ✓
          </span>
        ) : null}
      </div>
    </div>
  );
}
