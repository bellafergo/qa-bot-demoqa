/** View helpers for Security Readiness (SEC-01A). */

export const SECURITY_READINESS_I18N_KEYS = {
  title: "security_readiness.title",
  subtitle: "security_readiness.subtitle",
  empty: "security_readiness.empty",
  authentication: "security_readiness.authentication",
  ssoReadiness: "security_readiness.sso_readiness",
  rbac: "security_readiness.rbac",
  auditTrail: "security_readiness.audit_trail",
  securityScore: "security_readiness.security_score",
  configured: "security_readiness.configured",
  notConfigured: "security_readiness.not_configured",
  providersTitle: "security_readiness.providers_title",
  providerEnabled: "security_readiness.provider_enabled",
  providerDisabled: "security_readiness.provider_disabled",
  readOnlyNote: "security_readiness.read_only_note",
  methodLocal: "security_readiness.method.local",
  methodSso: "security_readiness.method.sso",
  methodHybrid: "security_readiness.method.hybrid",
  availableSso: "security_readiness.available_sso",
  configuredSso: "security_readiness.configured_sso",
};

const METHOD_LABEL_KEY = {
  LOCAL: SECURITY_READINESS_I18N_KEYS.methodLocal,
  SSO: SECURITY_READINESS_I18N_KEYS.methodSso,
  HYBRID: SECURITY_READINESS_I18N_KEYS.methodHybrid,
};

export function authenticationMethodLabelKey(method) {
  return METHOD_LABEL_KEY[String(method || "LOCAL").toUpperCase()]
    || SECURITY_READINESS_I18N_KEYS.methodLocal;
}

export function readinessBadgeClass(ready) {
  return ready ? "badge badge-green" : "badge badge-orange";
}

export function securityScoreBadgeClass(score) {
  const n = Number(score);
  if (n >= 75) return "badge badge-green";
  if (n >= 50) return "badge badge-blue";
  if (n >= 25) return "badge badge-orange";
  return "badge badge-red";
}

export function buildSecurityReadinessViewModel({ readiness, providers, t }) {
  const report = readiness || {};
  const providerList = providers?.providers || [];
  const empty = providerList.length === 0;

  return {
    show: true,
    empty,
    emptyMessage: t(SECURITY_READINESS_I18N_KEYS.empty),
    title: t(SECURITY_READINESS_I18N_KEYS.title),
    subtitle: t(SECURITY_READINESS_I18N_KEYS.subtitle),
    readOnlyNote: t(SECURITY_READINESS_I18N_KEYS.readOnlyNote),
    authenticationLabel: t(SECURITY_READINESS_I18N_KEYS.authentication),
    authenticationMethod: t(authenticationMethodLabelKey(report.authentication_method)),
    ssoReadinessLabel: t(SECURITY_READINESS_I18N_KEYS.ssoReadiness),
    ssoReadinessText: report.sso_ready
      ? t(SECURITY_READINESS_I18N_KEYS.configured)
      : t(SECURITY_READINESS_I18N_KEYS.notConfigured),
    ssoReadinessBadgeClass: readinessBadgeClass(report.sso_ready),
    rbacLabel: t(SECURITY_READINESS_I18N_KEYS.rbac),
    rbacText: report.rbac_ready
      ? t(SECURITY_READINESS_I18N_KEYS.configured)
      : t(SECURITY_READINESS_I18N_KEYS.notConfigured),
    rbacBadgeClass: readinessBadgeClass(report.rbac_ready),
    auditLabel: t(SECURITY_READINESS_I18N_KEYS.auditTrail),
    auditText: report.audit_ready
      ? t(SECURITY_READINESS_I18N_KEYS.configured)
      : t(SECURITY_READINESS_I18N_KEYS.notConfigured),
    auditBadgeClass: readinessBadgeClass(report.audit_ready),
    securityScoreLabel: t(SECURITY_READINESS_I18N_KEYS.securityScore),
    securityScore: report.security_score ?? 0,
    securityScoreBadgeClass: securityScoreBadgeClass(report.security_score),
    availableSsoLabel: t(SECURITY_READINESS_I18N_KEYS.availableSso),
    configuredSsoLabel: t(SECURITY_READINESS_I18N_KEYS.configuredSso),
    availableSsoProviders: (report.available_sso_providers || []).join(", ") || "—",
    configuredSsoProviders: (report.configured_sso_providers || []).join(", ") || "—",
    summary: report.summary || "",
    providersTitle: t(SECURITY_READINESS_I18N_KEYS.providersTitle),
    providerEnabledLabel: t(SECURITY_READINESS_I18N_KEYS.providerEnabled),
    providerDisabledLabel: t(SECURITY_READINESS_I18N_KEYS.providerDisabled),
    providers: providerList.map((provider) => ({
      ...provider,
      statusLabel: provider.enabled
        ? t(SECURITY_READINESS_I18N_KEYS.providerEnabled)
        : t(SECURITY_READINESS_I18N_KEYS.providerDisabled),
      statusBadgeClass: provider.enabled ? "badge badge-green" : "badge badge-gray",
    })),
  };
}
