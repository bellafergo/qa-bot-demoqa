/** V1.1 — Shared dashboard section empty / error / loading helpers. */

export const DASHBOARD_SECTION_I18N_KEYS = {
  loadError: "dash.section.load_error",
  loading: "dash.section.loading",
  ctaIntegrations: "dash.section.cta.integrations",
  ctaIncidents: "dash.section.cta.incidents",
  ctaSettings: "dash.section.cta.settings",
  ctaReports: "dash.section.cta.reports",
  retry: "dash.refresh",
};

export const DASHBOARD_SECTION_CTA = {
  integrations: { path: "/integrations", labelKey: DASHBOARD_SECTION_I18N_KEYS.ctaIntegrations },
  incidents: { path: "/incidents", labelKey: DASHBOARD_SECTION_I18N_KEYS.ctaIncidents },
  settings: { path: "/settings", labelKey: DASHBOARD_SECTION_I18N_KEYS.ctaSettings },
  reports: { path: "/dashboard", labelKey: DASHBOARD_SECTION_I18N_KEYS.ctaReports },
};

/**
 * Build a consistent section state for dashboard intelligence cards.
 */
export function buildDashboardSectionState({
  data,
  loadError = "",
  loading = false,
  empty = false,
  emptyMessage = "",
  emptyCta = null,
  capabilityState = null,
  t,
}) {
  const hasError = Boolean(loadError);
  const isLoading = Boolean(loading) && !hasError && data == null;
  const isGated = Boolean(capabilityState && capabilityState.state !== "AVAILABLE");
  const isEmpty = !hasError && !isLoading && !isGated && empty;

  return {
    show: hasError || isLoading || data != null || isEmpty || isGated,
    loading: isLoading,
    error: hasError ? loadError : "",
    empty: isEmpty,
    emptyMessage: isEmpty ? emptyMessage : "",
    capabilityState: isGated ? capabilityState : null,
    emptyCta: isEmpty && emptyCta
      ? {
          path: emptyCta.path,
          label: t(emptyCta.labelKey),
        }
      : null,
    loadingMessage: t(DASHBOARD_SECTION_I18N_KEYS.loading),
    retryLabel: t(DASHBOARD_SECTION_I18N_KEYS.retry),
  };
}

export function isColdProject({
  releaseReadiness,
  valueDashboard,
  businessRisk,
  executiveImpact,
}) {
  const noReadiness = !releaseReadiness?.release_readiness;
  const noValue = !valueDashboard || isValueDashboardEmptyForCold(valueDashboard);
  const noRiskIntel = !businessRisk || businessRisk.has_intelligence === false;
  const noImpactHistory = !executiveImpact?.has_sufficient_history;
  return noReadiness && noValue && noRiskIntel && noImpactHistory;
}

function isValueDashboardEmptyForCold(dashboard) {
  return (
    (dashboard.incidents_investigated || 0) === 0
    && (dashboard.executive_reports_generated || 0) === 0
    && (dashboard.release_readiness_reports || 0) === 0
  );
}

export function buildColdProjectGuidanceViewModel(isCold, t) {
  if (!isCold) return { show: false };
  return {
    show: true,
    message: t("dash.cold_project.message"),
    actions: [
      { path: DASHBOARD_SECTION_CTA.integrations.path, label: t(DASHBOARD_SECTION_CTA.integrations.labelKey) },
      { path: DASHBOARD_SECTION_CTA.incidents.path, label: t(DASHBOARD_SECTION_CTA.incidents.labelKey) },
      { path: DASHBOARD_SECTION_CTA.settings.path, label: t(DASHBOARD_SECTION_CTA.settings.labelKey) },
    ],
  };
}
