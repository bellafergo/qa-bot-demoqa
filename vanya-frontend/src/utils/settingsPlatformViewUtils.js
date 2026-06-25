/** Customer-facing labels for Settings platform diagnostics (no raw internals). */

export const SETTINGS_PLATFORM_I18N_KEYS = {
  serviceTitle: "settings.platform.service_title",
  serviceConnected: "settings.platform.service_connected",
  serviceStatus: "settings.platform.service_status",
  platformTitle: "settings.platform.title",
  aiEngine: "settings.platform.ai_engine",
  database: "settings.platform.database",
  evidenceStorage: "settings.platform.evidence_storage",
  authService: "settings.platform.auth_service",
  statusOperational: "settings.platform.status_operational",
  statusNeedsSetup: "settings.platform.status_needs_setup",
  statusConnected: "settings.platform.status_connected",
  statusNotConfigured: "settings.platform.status_not_configured",
  lastUpdated: "settings.platform.last_updated",
  lastUpdatedValue: "settings.platform.last_updated_value",
};

export function buildSettingsPlatformViewModel(meta, t) {
  const m = meta || {};
  const hasOpenAI = m.has_openai_key === true;
  const hasDB = m.has_db === true;
  const hasCloudinary = m.has_cloudinary === true;
  const supabaseConfigured = m.supabase_configured === true;
  const supabaseOk = m.supabase_ok === true;

  return {
    serviceTitle: t(SETTINGS_PLATFORM_I18N_KEYS.serviceTitle),
    serviceConnectedLabel: t(SETTINGS_PLATFORM_I18N_KEYS.serviceConnected),
    serviceStatusLabel: t(SETTINGS_PLATFORM_I18N_KEYS.serviceStatus),
    serviceOperational: hasOpenAI,
    platformTitle: t(SETTINGS_PLATFORM_I18N_KEYS.platformTitle),
    aiEngineLabel: t(SETTINGS_PLATFORM_I18N_KEYS.aiEngine),
    aiEngineStatus: hasOpenAI
      ? t(SETTINGS_PLATFORM_I18N_KEYS.statusOperational)
      : t(SETTINGS_PLATFORM_I18N_KEYS.statusNeedsSetup),
    databaseLabel: t(SETTINGS_PLATFORM_I18N_KEYS.database),
    databaseStatus: hasDB
      ? t(SETTINGS_PLATFORM_I18N_KEYS.statusConnected)
      : t(SETTINGS_PLATFORM_I18N_KEYS.statusNotConfigured),
    evidenceStorageLabel: t(SETTINGS_PLATFORM_I18N_KEYS.evidenceStorage),
    evidenceStorageStatus: hasCloudinary
      ? t(SETTINGS_PLATFORM_I18N_KEYS.statusConnected)
      : t(SETTINGS_PLATFORM_I18N_KEYS.statusNotConfigured),
    authServiceLabel: t(SETTINGS_PLATFORM_I18N_KEYS.authService),
    authServiceStatus: !supabaseConfigured
      ? t(SETTINGS_PLATFORM_I18N_KEYS.statusNotConfigured)
      : supabaseOk
        ? t(SETTINGS_PLATFORM_I18N_KEYS.statusOperational)
        : t(SETTINGS_PLATFORM_I18N_KEYS.statusConnected),
    lastUpdatedLabel: t(SETTINGS_PLATFORM_I18N_KEYS.lastUpdated),
    lastUpdatedValue: t(SETTINGS_PLATFORM_I18N_KEYS.lastUpdatedValue),
  };
}
