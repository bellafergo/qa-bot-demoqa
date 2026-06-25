/** Enterprise demo readiness — filter internal test projects from customer-facing lists. */

export const DEMO_ENTERPRISE_PROJECT = {
  id: "demo-enterprise-qa",
  name: "Demo Enterprise QA",
  description: "Demonstration project for evaluating Vanya capabilities.",
  color: "#4f46e5",
};

export const DEMO_ENTERPRISE_PROJECT_ES = {
  ...DEMO_ENTERPRISE_PROJECT,
  description: "Proyecto demostrativo para evaluación de capacidades de Vanya.",
};

const JUNK_ID_OR_NAME = /^(t|test|demo|prueba|temp|temporal|sandbox|dev|foo|bar)$/i;

export function isJunkDemoProject(project) {
  if (!project?.id) return true;
  const id = String(project.id).trim().toLowerCase();
  const name = String(project.name || "").trim().toLowerCase();
  const description = String(project.description || "").trim().toLowerCase();

  if (id === DEMO_ENTERPRISE_PROJECT.id) return false;
  if (name === "demo enterprise qa") return false;

  if (JUNK_ID_OR_NAME.test(id) || JUNK_ID_OR_NAME.test(name)) return true;
  if (name.length > 0 && name.length <= 2) return true;
  if (["test", "demo", "t", "prueba", "temporal", "temp"].includes(description)) return true;

  return false;
}

export function filterPresentationProjects(projects) {
  return (Array.isArray(projects) ? projects : []).filter((p) => !isJunkDemoProject(p));
}

export function hasDemoEnterpriseProject(projects) {
  return filterPresentationProjects(projects).some((p) => p.id === DEMO_ENTERPRISE_PROJECT.id);
}

export function buildDemoEnterpriseProjectPayload(lang = "en") {
  const base = lang === "es" ? DEMO_ENTERPRISE_PROJECT_ES : DEMO_ENTERPRISE_PROJECT;
  return {
    id: base.id,
    name: base.name,
    description: base.description,
    color: base.color,
    base_url: null,
    settings: {},
  };
}
