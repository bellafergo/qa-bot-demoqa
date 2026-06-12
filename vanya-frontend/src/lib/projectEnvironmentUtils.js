/** Helpers for project environment configuration (onboarding ENT-03A). */

export const ENVIRONMENT_TYPES = ["QA", "STAGING", "PRODUCTION", "OTHER"];

export function readEnvironmentsFromProject(project) {
  const envs = project?.settings?.environments;
  if (!Array.isArray(envs) || !envs.length) return [];
  return envs.map((e, index) => ({
    id: `env-${index}`,
    name: String(e?.name || "").trim(),
    type: String(e?.type || "OTHER").trim().toUpperCase(),
    url: String(e?.url || "").trim(),
  }));
}

export function suggestDefaultEnvironments(baseUrl, existingRows = []) {
  if (existingRows.length > 0) return existingRows;
  const url = String(baseUrl || "").trim();
  if (!url) return [];
  return [
    {
      id: "suggested-production",
      name: "Production",
      type: "PRODUCTION",
      url,
    },
  ];
}

export function validateEnvironmentRows(rows) {
  const errors = [];
  (rows || []).forEach((row, index) => {
    if (!String(row?.name || "").trim()) {
      errors.push({ index, field: "name" });
    }
    if (!String(row?.type || "").trim()) {
      errors.push({ index, field: "type" });
    }
  });
  return errors;
}

export function buildEnvironmentsPayload(rows) {
  return (rows || [])
    .filter((row) => String(row?.name || "").trim() && String(row?.type || "").trim())
    .map((row) => {
      const out = {
        name: String(row.name).trim(),
        type: String(row.type).trim().toUpperCase(),
      };
      const url = String(row.url || "").trim();
      if (url) out.url = url;
      return out;
    });
}

export function newEnvironmentRow(type = "QA") {
  return {
    id: `env-new-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
    name: "",
    type,
    url: "",
  };
}

export function buildProjectSettingsPayload({ loginProfile, variables, environments, includeEnvironments }) {
  const settings = {};
  if (loginProfile && Object.keys(loginProfile).length) {
    settings.login_profile = loginProfile;
  }
  if (variables && Object.keys(variables).length) {
    settings.variables = variables;
  }
  if (includeEnvironments) {
    settings.environments = buildEnvironmentsPayload(environments);
  }
  return settings;
}
