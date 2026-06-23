import React from "react";
import { Link } from "react-router-dom";
import { useLang } from "../../i18n/LangContext";

export default function PrRiskFlowGuide({ variant = "pr" }) {
  const { t } = useLang();
  const isPr = variant === "pr";

  return (
    <div
      className="card"
      style={{
        marginBottom: 20,
        padding: "16px 18px",
        borderColor: "var(--accent-border, var(--border))",
        background: "var(--bg-2)",
      }}
    >
      <div style={{ fontSize: 13, fontWeight: 700, color: "var(--text-1)", marginBottom: 6 }}>
        {t(isPr ? "pr_risk.guide.pr_title" : "pr_risk.guide.risk_title")}
      </div>
      <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, margin: "0 0 12px" }}>
        {t(isPr ? "pr_risk.guide.pr_desc" : "pr_risk.guide.risk_desc")}
      </p>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
        <span className="badge badge-blue" style={{ fontSize: 11 }}>
          {t(isPr ? "pr_risk.guide.pr_badge" : "pr_risk.guide.risk_badge")}
        </span>
        <Link
          to={isPr ? "/insights" : "/pr-analysis"}
          className="btn btn-secondary btn-sm"
          style={{ textDecoration: "none" }}
          state={isPr ? { tab: "risk" } : undefined}
        >
          {t(isPr ? "pr_risk.guide.pr_cta" : "pr_risk.guide.risk_cta")}
        </Link>
      </div>
    </div>
  );
}
