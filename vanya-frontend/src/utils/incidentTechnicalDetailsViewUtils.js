/** Collapsible technical metadata for Incident Investigator (customer-facing gating). */

export const INCIDENT_TECHNICAL_I18N_KEYS = {
  title: "incident.qa.technical_details_title",
  engine: "incident.qa.technical_engine",
  analyzeOnly: "incident.qa.technical_analyze_only",
  analyzeOnlyYes: "incident.qa.technical_analyze_only_yes",
  analyzeOnlyNo: "incident.qa.technical_analyze_only_no",
  reportId: "incident.qa.technical_report_id",
  incidentId: "incident.qa.technical_incident_id",
};

export function buildIncidentTechnicalDetailsViewModel(report, t) {
  const meta = report?.meta || {};
  const engineVersion = meta.engine_version ? String(meta.engine_version) : null;
  const analyzeOnly = meta.analyze_only === true;
  const reportId = report?.report_id || report?.id || meta.report_id || null;
  const incidentId = report?.incident_id || meta.incident_id || null;

  return {
    show: Boolean(engineVersion || meta.analyze_only != null || reportId || incidentId),
    title: t(INCIDENT_TECHNICAL_I18N_KEYS.title),
    engineLabel: t(INCIDENT_TECHNICAL_I18N_KEYS.engine),
    engineVersion,
    analyzeOnlyLabel: t(INCIDENT_TECHNICAL_I18N_KEYS.analyzeOnly),
    analyzeOnlyText: analyzeOnly
      ? t(INCIDENT_TECHNICAL_I18N_KEYS.analyzeOnlyYes)
      : t(INCIDENT_TECHNICAL_I18N_KEYS.analyzeOnlyNo),
    reportIdLabel: t(INCIDENT_TECHNICAL_I18N_KEYS.reportId),
    reportId: reportId ? String(reportId) : null,
    incidentIdLabel: t(INCIDENT_TECHNICAL_I18N_KEYS.incidentId),
    incidentId: incidentId ? String(incidentId) : null,
  };
}
