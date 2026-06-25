import React from "react";
import { Link } from "react-router-dom";
import QualityTrendReportView from "../incident/QualityTrendReportView.jsx";
import EarlyDegradationReportView from "../incident/EarlyDegradationReportView.jsx";
import ReleaseReadinessView from "../release-readiness/ReleaseReadinessView.jsx";
import PlatformObservabilityView from "../platform-observability/PlatformObservabilityView.jsx";
import DashboardMoreIntelligenceSection from "./DashboardMoreIntelligenceSection.jsx";
import { SkeletonCard } from "../ui/Skeleton.jsx";

function MetricTile({ label, value, badgeClass }) {
  return (
    <div
      style={{
        padding: "16px 18px",
        borderRadius: 10,
        border: "1px solid var(--border)",
        background: "var(--bg-2, rgba(255,255,255,0.02))",
        minHeight: 88,
      }}
    >
      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 8, textTransform: "uppercase", letterSpacing: "0.04em" }}>
        {label}
      </div>
      <span className={badgeClass} style={{ fontSize: 15, fontWeight: 700, padding: "6px 12px" }}>
        {value}
      </span>
    </div>
  );
}

function BulletList({ items, emptyMessage }) {
  if (!items?.length) {
    return <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>{emptyMessage}</p>;
  }
  return (
    <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.55 }}>
      {items.map((item) => (
        <li key={item}>{item}</li>
      ))}
    </ul>
  );
}

export default function ExecutiveDashboardV2({
  vm,
  exploreChildren,
  qualityTrendVm,
  earlyDegradationVm,
  releaseReadinessVm,
  platformObservabilityVm,
  platformObservabilitySection,
  onRetry,
  DashboardSectionState,
}) {
  if (!vm) return null;

  if (vm.loading) {
    return (
      <div style={{ display: "flex", flexDirection: "column", gap: 20, marginBottom: 24 }}>
        <SkeletonCard lines={4} />
        <SkeletonCard lines={3} />
        <SkeletonCard lines={2} />
      </div>
    );
  }

  const { block1, block2, block3, block4, block5 } = vm;

  const healthRows = [
    block4.summary.qualityTrend.show ? block4.summary.qualityTrend : null,
    block4.summary.earlyDegradation.show ? block4.summary.earlyDegradation : null,
    block4.summary.platform.show ? block4.summary.platform : null,
    block4.summary.release.show ? block4.summary.release : null,
  ].filter(Boolean);

  const hasDetails =
    (qualityTrendVm?.show && !qualityTrendVm?.empty)
    || (earlyDegradationVm?.show && !earlyDegradationVm?.empty)
    || releaseReadinessVm?.show
    || (platformObservabilityVm?.show && !platformObservabilityVm?.empty);

  return (
    <div className="executive-dashboard-v2" style={{ display: "flex", flexDirection: "column", gap: 20, marginBottom: 8 }}>
      {/* Block 1 — Executive Decision (first fold) */}
      <section className="card" style={{ padding: "24px 28px" }} aria-label={block1.title}>
        <h2 style={{ margin: "0 0 20px", fontSize: 13, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em" }}>
          {block1.title}
        </h2>
        <div
          style={{
            display: "grid",
            gridTemplateColumns: "repeat(auto-fit, minmax(160px, 1fr))",
            gap: 14,
            marginBottom: block1.topRecommendation ? 20 : 0,
          }}
        >
          <MetricTile label={block1.releaseDecisionLabel} value={block1.decisionLabel} badgeClass={block1.decisionBadgeClass} />
          <MetricTile label={block1.deploymentRiskLabel} value={block1.deploymentRisk.label} badgeClass={block1.deploymentRisk.badgeClass} />
          {block1.confidence.show ? (
            <MetricTile label={block1.confidenceLabel} value={block1.confidence.label} badgeClass={block1.confidence.badgeClass} />
          ) : null}
        </div>
        {block1.cautionReason ? (
          <p style={{ fontSize: 13, color: "var(--text-2)", margin: "0 0 16px", lineHeight: 1.55 }}>
            {block1.cautionReason}
          </p>
        ) : null}
        {block1.topRecommendation ? (
          <div
            style={{
              padding: "14px 16px",
              borderRadius: 8,
              border: "1px solid var(--border)",
              background: "var(--bg-3, rgba(255,255,255,0.03))",
            }}
          >
            <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
              {block1.topRecommendationLabel}
            </div>
            <Link
              to={block1.topRecommendation.path}
              style={{ fontSize: 15, fontWeight: 600, color: "var(--accent)", textDecoration: "none" }}
            >
              {block1.topRecommendation.label} →
            </Link>
            {block1.topRecommendation.context ? (
              <p style={{ fontSize: 12, color: "var(--text-3)", margin: "8px 0 0", lineHeight: 1.5 }}>
                {block1.topRecommendation.context}
              </p>
            ) : null}
          </div>
        ) : null}
      </section>

      {/* Block 2 — What Changed */}
      <section className="card" style={{ padding: "20px 24px" }} aria-label={block2.title}>
        <h2 style={{ margin: "0 0 16px", fontSize: 13, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em" }}>
          {block2.title}
        </h2>
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(200px, 1fr))", gap: 20 }}>
          <div>
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 8 }}>{block2.topRisksLabel}</div>
            <BulletList items={block2.topRisks} emptyMessage={block2.noRisks} />
          </div>
          <div>
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 8 }}>{block2.topChangesLabel}</div>
            <BulletList items={block2.topChanges} emptyMessage={block2.noChanges} />
          </div>
          <div>
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 8 }}>{block2.topModulesLabel}</div>
            <BulletList items={block2.topModules} emptyMessage={block2.noModules} />
          </div>
        </div>
      </section>

      {/* Block 3 — Recommended Actions */}
      {block3.actions.length ? (
        <section aria-label={block3.title}>
          <h2 style={{ margin: "0 0 12px", fontSize: 13, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em" }}>
            {block3.title}
          </h2>
          <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))", gap: 14 }}>
            {block3.actions.map((action) => (
              <div
                key={`${action.path}:${action.title}`}
                className="card"
                style={{ padding: "16px 18px", display: "flex", flexDirection: "column", gap: 8 }}
              >
                <Link to={action.path} style={{ fontSize: 14, fontWeight: 600, color: "var(--accent)", textDecoration: "none" }}>
                  {action.title} →
                </Link>
                <p style={{ fontSize: 12, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
                  <strong style={{ color: "var(--text-3)", fontWeight: 600 }}>{block3.whyLabel}:</strong> {action.why}
                </p>
                <p style={{ fontSize: 12, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
                  <strong style={{ color: "var(--text-3)", fontWeight: 600 }}>{block3.impactLabel}:</strong> {action.impact}
                </p>
                <p style={{ fontSize: 11, color: "var(--text-3)", margin: 0 }}>
                  {block3.timeLabel}: {action.timeEstimate}
                </p>
              </div>
            ))}
          </div>
        </section>
      ) : null}

      {/* Block 4 — System Health */}
      <section className="card" style={{ padding: "20px 24px" }} aria-label={block4.title}>
        <h2 style={{ margin: "0 0 14px", fontSize: 13, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em" }}>
          {block4.title}
        </h2>
        {healthRows.length ? (
          <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(180px, 1fr))", gap: 12, marginBottom: hasDetails ? 16 : 0 }}>
            {healthRows.map((row) => (
              <div key={row.label} style={{ padding: "10px 12px", borderRadius: 8, border: "1px solid var(--border)" }}>
                <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>{row.label}</div>
                <span className={row.badgeClass}>{row.value}</span>
              </div>
            ))}
          </div>
        ) : (
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>—</p>
        )}
        {hasDetails ? (
          <details style={{ marginTop: 4 }}>
            <summary style={{ cursor: "pointer", fontSize: 12, fontWeight: 600, color: "var(--text-2)", listStyle: "none" }}>
              ▸ {block4.moreDetailsLabel}
            </summary>
            <div style={{ marginTop: 14, display: "flex", flexDirection: "column", gap: 16 }}>
              {qualityTrendVm?.show && !qualityTrendVm?.empty ? (
                <QualityTrendReportView vm={qualityTrendVm} />
              ) : null}
              {earlyDegradationVm?.show && !earlyDegradationVm?.empty ? (
                <EarlyDegradationReportView vm={earlyDegradationVm} />
              ) : null}
              {releaseReadinessVm?.show ? (
                <ReleaseReadinessView vm={releaseReadinessVm} />
              ) : null}
              {platformObservabilityVm?.show && !platformObservabilityVm?.empty && DashboardSectionState ? (
                <DashboardSectionState state={platformObservabilitySection} onRetry={onRetry}>
                  <PlatformObservabilityView vm={platformObservabilityVm} />
                </DashboardSectionState>
              ) : null}
            </div>
          </details>
        ) : null}
      </section>

      {/* Block 5 — Explore */}
      {exploreChildren ? (
        <DashboardMoreIntelligenceSection title={block5.title} summary={block5.summary}>
          {exploreChildren}
        </DashboardMoreIntelligenceSection>
      ) : null}
    </div>
  );
}
