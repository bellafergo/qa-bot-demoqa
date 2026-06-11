/** View helpers for Enterprise SSO configuration (SEC-01D). */

export const SSO_I18N_KEYS = {
  title: "sso_config.title",
  subtitle: "sso_config.subtitle",
  empty: "sso_config.empty",
  configured: "sso_config.configured",
  notConfigured: "sso_config.not_configured",
  validated: "sso_config.validated",
  notValidated: "sso_config.not_validated",
  clientId: "sso_config.client_id",
  tenantId: "sso_config.tenant_id",
  issuer: "sso_config.issuer",
  validate: "sso_config.validate",
  generateLoginUrl: "sso_config.generate_login_url",
  loginUrl: "sso_config.login_url",
  validationSuccess: "sso_config.validation_success",
  validationFailed: "sso_config.validation_failed",
  readOnlyNote: "sso_config.read_only_note",
  providerMicrosoft: "sso_config.provider.microsoft",
  providerGoogle: "sso_config.provider.google",
  providerOkta: "sso_config.provider.okta",
};

const PROVIDER_LABEL_KEY = {
  MICROSOFT: SSO_I18N_KEYS.providerMicrosoft,
  GOOGLE: SSO_I18N_KEYS.providerGoogle,
  OKTA: SSO_I18N_KEYS.providerOkta,
};

export function providerLabelKey(provider) {
  return PROVIDER_LABEL_KEY[String(provider || "").toUpperCase()] || provider;
}

export function statusBadgeClass(active) {
  return active ? "badge badge-green" : "badge badge-orange";
}

export function mapSsoProvider(provider, t) {
  if (!provider?.provider) return null;
  return {
    provider: provider.provider,
    providerLabel: t(providerLabelKey(provider.provider)),
    enabled: Boolean(provider.enabled),
    clientId: provider.client_id || "",
    tenantId: provider.tenant_id || "",
    issuer: provider.issuer || "",
    configured: Boolean(provider.configured),
    validated: Boolean(provider.validated),
    configuredLabel: provider.configured
      ? t(SSO_I18N_KEYS.configured)
      : t(SSO_I18N_KEYS.notConfigured),
    validatedLabel: provider.validated
      ? t(SSO_I18N_KEYS.validated)
      : t(SSO_I18N_KEYS.notValidated),
    configuredBadgeClass: statusBadgeClass(provider.configured),
    validatedBadgeClass: statusBadgeClass(provider.validated),
    clientIdLabel: t(SSO_I18N_KEYS.clientId),
    tenantIdLabel: t(SSO_I18N_KEYS.tenantId),
    issuerLabel: t(SSO_I18N_KEYS.issuer),
    validateLabel: t(SSO_I18N_KEYS.validate),
    generateLoginUrlLabel: t(SSO_I18N_KEYS.generateLoginUrl),
    loginUrlLabel: t(SSO_I18N_KEYS.loginUrl),
    showTenantId: provider.provider === "MICROSOFT",
    showIssuer: provider.provider === "OKTA",
  };
}

export function buildSsoConfigurationViewModel({ providers, t }) {
  const list = (providers?.providers || [])
    .map((provider) => mapSsoProvider(provider, t))
    .filter(Boolean);

  return {
    show: true,
    empty: list.length === 0,
    emptyMessage: t(SSO_I18N_KEYS.empty),
    title: t(SSO_I18N_KEYS.title),
    subtitle: t(SSO_I18N_KEYS.subtitle),
    readOnlyNote: t(SSO_I18N_KEYS.readOnlyNote),
    validationSuccessLabel: t(SSO_I18N_KEYS.validationSuccess),
    validationFailedLabel: t(SSO_I18N_KEYS.validationFailed),
    providers: list,
  };
}
