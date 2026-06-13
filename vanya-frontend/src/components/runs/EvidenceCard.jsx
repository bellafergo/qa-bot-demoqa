import React from "react";
import { runSteps, shouldShowApiHttpEvidence } from "../../utils/runEvidenceUtils.js";
import { useLang } from "../../i18n/LangContext";
import { inferRunType, getScreenshotSrc, collectHealingRows } from "../../utils/runHelpers.js";
import RunExecutionStepsTable from "./RunExecutionStepsTable.jsx";

export default function EvidenceCard({ detail, runType }) {
  const { t } = useLang();
  if (!detail) return null;
  // Support both canonical (artifacts.*) and legacy (flat fields)
  const artifacts    = detail.artifacts || {};
  const evidenceUrl  = artifacts.evidence_url  || detail.evidence_url;
  const reportUrl    = artifacts.report_url    || detail.report_url;
  const screenshotSrc = getScreenshotSrc(detail);
  const hasLinks     = evidenceUrl || reportUrl;
  const resolvedRunType = runType || inferRunType(detail);
  const showApiHttpEvidence = shouldShowApiHttpEvidence(detail, resolvedRunType);
  const hasAssetEvidence = Boolean(screenshotSrc || hasLinks);
  const hasAnything = hasAssetEvidence || showApiHttpEvidence;
  const isDesktop    = resolvedRunType === "desktop";
  const steps = runSteps(detail);

  return (
    <div className="card">
      <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.evidence.title")}</div>
      {!hasAnything ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>{t("runs.evidence.none")}</div>
      ) : (
        <>
          {screenshotSrc && (
            <>
              {isDesktop && (
                <div style={{ fontSize: 10, fontWeight: 400, color: "var(--accent)", textTransform: "uppercase", letterSpacing: "0.07em", marginBottom: 6 }}>
                  ⊞ {t("runs.desktop.screenshot")}
                </div>
              )}
              <img
                src={screenshotSrc}
                alt={t("runs.detail.screenshot")}
                style={{
                  width: "100%",
                  borderRadius: 6,
                  border: isDesktop ? "2px solid var(--accent)" : "1px solid var(--border)",
                  objectFit: "contain",
                  marginBottom: hasLinks ? 10 : 0,
                  display: "block",
                }}
              />
            </>
          )}
          {hasLinks && (
            <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
              {evidenceUrl && (
                <a href={evidenceUrl} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">
                  {t("runs.evidence.open")}
                </a>
              )}
              {reportUrl && (
                <a href={reportUrl} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">
                  {t("runs.evidence.report")}
                </a>
              )}
            </div>
          )}
          {showApiHttpEvidence && (
            <RunExecutionStepsTable
              steps={steps}
              healedIndexSet={new Set(collectHealingRows(detail).map((r) => r.stepIndex).filter((i) => typeof i === "number"))}
              t={t}
              inline
            />
          )}
        </>
      )}
    </div>
  );
}
