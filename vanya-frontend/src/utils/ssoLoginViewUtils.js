/** View helpers for enterprise SSO login (EC-01G). */

export const SSO_LOGIN_I18N_KEYS = {
  title: "sso_login.title",
  subtitle: "sso_login.subtitle",
  empty: "sso_login.empty",
  loginWithMicrosoft: "sso_login.login_microsoft",
  loginWithGoogle: "sso_login.login_google",
  loginWithOkta: "sso_login.login_okta",
  redirecting: "sso_login.redirecting",
  localDivider: "sso_login.local_divider",
  identityTitle: "sso_login.identity_title",
  identityProvider: "sso_login.identity_provider",
  identityEmail: "sso_login.identity_email",
  identityDisplayName: "sso_login.identity_display_name",
  identityMethod: "sso_login.identity_method",
  methodLocal: "sso_login.method.local",
  methodSso: "sso_login.method.sso",
  methodHybrid: "sso_login.method.hybrid",
};

const PROVIDER_LABEL_KEY = {
  MICROSOFT: SSO_LOGIN_I18N_KEYS.loginWithMicrosoft,
  GOOGLE: SSO_LOGIN_I18N_KEYS.loginWithGoogle,
  OKTA: SSO_LOGIN_I18N_KEYS.loginWithOkta,
};

const METHOD_LABEL_KEY = {
  LOCAL: SSO_LOGIN_I18N_KEYS.methodLocal,
  SSO: SSO_LOGIN_I18N_KEYS.methodSso,
  HYBRID: SSO_LOGIN_I18N_KEYS.methodHybrid,
};

export function providerButtonLabel(provider, t) {
  const key = PROVIDER_LABEL_KEY[String(provider || "").toUpperCase()];
  return key ? t(key) : provider;
}

export function authenticationMethodLabel(method, t) {
  const key = METHOD_LABEL_KEY[String(method || "LOCAL").toUpperCase()];
  return key ? t(key) : method;
}

export function buildSsoLoginViewModel({ providers, t }) {
  const list = Array.isArray(providers) ? providers : [];
  return {
    show: true,
    empty: list.length === 0,
    emptyMessage: t(SSO_LOGIN_I18N_KEYS.empty),
    title: t(SSO_LOGIN_I18N_KEYS.title),
    subtitle: t(SSO_LOGIN_I18N_KEYS.subtitle),
    localDivider: t(SSO_LOGIN_I18N_KEYS.localDivider),
    redirectingLabel: t(SSO_LOGIN_I18N_KEYS.redirecting),
    providers: list.map((provider) => ({
      provider: String(provider).toUpperCase(),
      label: providerButtonLabel(provider, t),
    })),
  };
}

export function buildSsoIdentityViewModel({ identity, readiness, t }) {
  const report = readiness || {};
  const id = identity || {};
  const method = report.authentication_method || (id.provider_type && id.provider_type !== "LOCAL" ? "SSO" : "LOCAL");

  return {
    show: true,
    title: t(SSO_LOGIN_I18N_KEYS.identityTitle),
    providerLabel: t(SSO_LOGIN_I18N_KEYS.identityProvider),
    emailLabel: t(SSO_LOGIN_I18N_KEYS.identityEmail),
    displayNameLabel: t(SSO_LOGIN_I18N_KEYS.identityDisplayName),
    methodLabel: t(SSO_LOGIN_I18N_KEYS.identityMethod),
    provider: id.provider_type || report.active_provider_type || "LOCAL",
    email: id.email || "—",
    displayName: id.display_name || "—",
    authenticationMethod: authenticationMethodLabel(method, t),
    hasIdentity: Boolean(id.email || id.user_id),
  };
}
