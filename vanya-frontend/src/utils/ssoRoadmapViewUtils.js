/** Enterprise SSO roadmap presentation (SEC-01D UX — no credential forms). */

export const SSO_ROADMAP_I18N_KEYS = {
  title: "sso_roadmap.title",
  subtitle: "sso_roadmap.subtitle",
  availableTodayTitle: "sso_roadmap.available_today_title",
  availableTodayDesc: "sso_roadmap.available_today_desc",
  roadmapTitle: "sso_roadmap.roadmap_title",
  roadmapDesc: "sso_roadmap.roadmap_desc",
  statusAvailable: "sso_roadmap.status_available",
  statusPlanned: "sso_roadmap.status_planned",
  targetQuarter: "sso_roadmap.target_quarter",
  notifyCta: "sso_roadmap.notify_cta",
  readOnlyNote: "sso_roadmap.read_only_note",
  providerMicrosoft: "sso_config.provider.microsoft",
  providerGoogle: "sso_config.provider.google",
  providerOkta: "sso_config.provider.okta",
};

const PROVIDER_LABEL_KEY = {
  MICROSOFT: SSO_ROADMAP_I18N_KEYS.providerMicrosoft,
  GOOGLE: SSO_ROADMAP_I18N_KEYS.providerGoogle,
  OKTA: SSO_ROADMAP_I18N_KEYS.providerOkta,
};

const ROADMAP_TARGET = {
  MICROSOFT: "Q3 2026",
  GOOGLE: "Q3 2026",
  OKTA: "Q4 2026",
};

export function buildSsoRoadmapViewModel({ providers, t }) {
  const list = (providers?.providers || []).map((provider) => {
    const key = String(provider?.provider || "").toUpperCase();
    return {
      provider: key,
      providerLabel: t(PROVIDER_LABEL_KEY[key] || key),
      targetQuarter: ROADMAP_TARGET[key] || "2026",
      statusLabel: t(SSO_ROADMAP_I18N_KEYS.statusPlanned),
      statusBadgeClass: "badge badge-gray",
    };
  });

  return {
    show: true,
    title: t(SSO_ROADMAP_I18N_KEYS.title),
    subtitle: t(SSO_ROADMAP_I18N_KEYS.subtitle),
    availableTodayTitle: t(SSO_ROADMAP_I18N_KEYS.availableTodayTitle),
    availableTodayDesc: t(SSO_ROADMAP_I18N_KEYS.availableTodayDesc),
    availableTodayBadge: t(SSO_ROADMAP_I18N_KEYS.statusAvailable),
    roadmapTitle: t(SSO_ROADMAP_I18N_KEYS.roadmapTitle),
    roadmapDesc: t(SSO_ROADMAP_I18N_KEYS.roadmapDesc),
    targetQuarterLabel: t(SSO_ROADMAP_I18N_KEYS.targetQuarter),
    readOnlyNote: t(SSO_ROADMAP_I18N_KEYS.readOnlyNote),
    providers: list,
  };
}
