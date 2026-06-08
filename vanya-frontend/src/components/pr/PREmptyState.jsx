// src/components/pr/PREmptyState.jsx
import React from "react";
import { Link } from "react-router-dom";
import { useLang } from "../../i18n/LangContext";

export default function PREmptyState({ gitHubReady, hasKnowledge, knowledgeLoading, hasProject }) {
  const { t } = useLang();

  return (
    <div className="card" style={{ padding: "24px 28px", marginBottom: 20, textAlign: "center" }}>
      <div style={{ fontSize: 28, opacity: 0.3, marginBottom: 12 }}>⎇</div>
      <div style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)", marginBottom: 8 }}>
        {t("pr.enterprise.empty.title")}
      </div>
      <p style={{ fontSize: 13, color: "var(--text-2)", maxWidth: 480, margin: "0 auto 16px", lineHeight: 1.55 }}>
        {!hasProject
          ? t("pr.v1.need_project")
          : !gitHubReady
            ? t("pr.enterprise.empty.connect_scm")
            : t("pr.enterprise.empty.analyze_hint")}
      </p>
      <div style={{ display: "flex", gap: 10, justifyContent: "center", flexWrap: "wrap" }}>
        {!gitHubReady && hasProject ? (
          <Link to="/integrations" className="btn btn-primary btn-sm">{t("gh.go_integrations")}</Link>
        ) : null}
        {hasProject && !knowledgeLoading && !hasKnowledge ? (
          <Link to="/knowledge" className="btn btn-secondary btn-sm">{t("pr.enterprise.knowledge.cta")}</Link>
        ) : null}
      </div>
    </div>
  );
}
