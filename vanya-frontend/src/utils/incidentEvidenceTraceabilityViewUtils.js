/** Evidence traceability view helpers for Incident Investigator (UX-04). */

export const EVIDENCE_TRACE_I18N_KEYS = {
  evidenceSummary: "incident.qa.evidence_trace.evidence_summary",
  rootCauseContributors: "incident.qa.evidence_trace.root_cause_contributors",
  evidenceSources: "incident.qa.evidence_trace.evidence_sources",
  whyExplanation: "incident.qa.evidence_trace.why_explanation",
  whyPrefix: "incident.qa.evidence_trace.why_prefix",
  noSupportingEvidence: "incident.qa.evidence_trace.no_supporting_evidence",
  confidence: "incident.qa.evidence_trace.confidence",
  runs: "incident.qa.evidence_trace.runs",
  prs: "incident.qa.evidence_trace.prs",
  failureClusters: "incident.qa.evidence_trace.failure_clusters",
  systemMemory: "incident.qa.evidence_trace.system_memory",
  historicalIncidents: "incident.qa.evidence_trace.historical_incidents",
  technicalRisk: "incident.qa.evidence_trace.technical_risk",
  operationalRisk: "incident.qa.evidence_trace.operational_risk",
  executiveRisk: "incident.qa.evidence_trace.executive_risk",
  riskGroupingTitle: "incident.qa.evidence_trace.risk_grouping_title",
};

const HIGH_SEVERITIES = new Set(["CRITICAL", "HIGH", "BROKEN", "RED", "ORANGE"]);

export function parseInsightText(insightText) {
  const raw = String(insightText || "").trim();
  const match = raw.match(/^(.+?)\s+(?:is|are)\s+(BROKEN|CRITICAL|HIGH|MEDIUM|LOW|DEGRADED|RED|ORANGE|YELLOW|GREEN)$/i);
  if (match) {
    return { subject: match[1].trim(), severity: match[2].toUpperCase(), raw };
  }
  return { subject: raw, severity: "UNKNOWN", raw };
}

export function isCriticalOrHighInsight(insightText) {
  const { severity } = parseInsightText(insightText);
  return HIGH_SEVERITIES.has(severity);
}

export function formatTraceConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  const pct = n <= 1 ? Math.round(n * 100) : Math.round(n);
  return `${pct}%`;
}

function normalizeToken(value) {
  return String(value || "").toLowerCase().replace(/[^a-z0-9]+/g, " ").trim();
}

function insightTokens(insight) {
  const parsed = parseInsightText(insight);
  return normalizeToken(parsed.subject).split(/\s+/).filter(Boolean);
}

function isAuthRelatedInsight(insight) {
  const subject = parseInsightText(insight).subject.toLowerCase();
  return subject.includes("auth") || subject.includes("login") || subject.includes("journey");
}

function textMatchesInsight(text, insight) {
  const hay = normalizeToken(text);
  const tokens = insightTokens(insight);
  if (!tokens.length) return false;
  const hits = tokens.filter((token) => hay.includes(token));
  return hits.length >= Math.min(2, tokens.length);
}

function textMatchesInsightOrDomain(text, insight) {
  if (textMatchesInsight(text, insight)) return true;
  if (!isAuthRelatedInsight(insight)) return false;
  const hay = normalizeToken(text);
  return hay.includes("auth") || hay.includes("login") || hay.includes("selector") || hay.includes("contract");
}

function findMatchingJourney(report, insight) {
  const journeys = report?.data_journey_validation?.journeys || [];
  const results = report?.data_journey_validation?.results || [];
  const parsed = parseInsightText(insight);
  const journey = journeys.find(
    (j) =>
      textMatchesInsight(j.name, insight)
      || textMatchesInsight(j.business_area, insight)
      || normalizeToken(parsed.subject).includes(normalizeToken(j.name)),
  );
  if (!journey) return null;
  const result = results.find((r) => r.journey_id === journey.journey_id) || null;
  return { journey, result };
}

function inferAffectedModule(report, insight) {
  const modules = [
    ...(report?.impacted_modules || []),
    ...(report?.impacted_modules_ranked || []).map((m) => m.module || m.name),
  ].filter(Boolean);
  const hit = modules.find((module) => textMatchesInsight(module, insight));
  if (hit) return hit;

  const failedRuns = (report?.related_runs || []).filter((r) => String(r.status || "").toLowerCase() !== "passed");
  const runModule = failedRuns.find((r) => r.module)?.module;
  if (runModule && textMatchesInsight(runModule, insight)) return runModule;
  if (runModule) return runModule;

  const parsed = parseInsightText(insight);
  if (parsed.subject.toLowerCase().includes("auth")) return "Auth";
  if (parsed.subject.toLowerCase().includes("payment")) return "Payments";
  return modules[0] || null;
}

function countFailureClusters(report) {
  const timelineClusters = (report?.timeline || []).filter((e) => e.event_type === "failure_cluster").length;
  const correlationClusters = (report?.evidence_correlation?.evidence || []).filter((e) =>
    String(e.source || e.related_entity_type || "").toLowerCase().includes("failure_cluster"),
  ).length;
  return timelineClusters + correlationClusters;
}

function failedRuns(report) {
  return (report?.related_runs || []).filter((r) => {
    const status = String(r.status || "").toLowerCase();
    return status && status !== "passed" && status !== "success";
  });
}

function topFailingTest(report) {
  const counts = new Map();
  for (const run of failedRuns(report)) {
    const name = run.test_name || run.test_id;
    if (!name) continue;
    counts.set(name, (counts.get(name) || 0) + 1);
  }
  return [...counts.entries()].sort((a, b) => b[1] - a[1])[0] || null;
}

export function buildEvidenceSummary(report, insight, t) {
  const parsed = parseInsightText(insight);
  const bullets = [];
  const failed = failedRuns(report);

  if (failed.length) {
    bullets.push(t("incident.qa.evidence_trace.summary_correlated_runs", { count: failed.length }));
  }

  const topTest = topFailingTest(report);
  if (topTest && topTest[1] > 1) {
    bullets.push(t("incident.qa.evidence_trace.summary_test_failures", { test: topTest[0], count: topTest[1] }));
  } else if (topTest) {
    bullets.push(t("incident.qa.evidence_trace.summary_test_failed_once", { test: topTest[0] }));
  }

  const module = inferAffectedModule(report, insight);
  if (module) {
    bullets.push(t("incident.qa.evidence_trace.summary_module_affected", { module }));
  }

  if (countFailureClusters(report) > 0) {
    bullets.push(t("incident.qa.evidence_trace.summary_cluster_overlap"));
  }

  const similarCount = report?.historical_learning?.similar_incidents?.length || 0;
  if (similarCount > 0) {
    bullets.push(t("incident.qa.evidence_trace.summary_historical_match", { count: similarCount }));
  }

  const journeyMatch = findMatchingJourney(report, insight);
  if (journeyMatch?.result?.missing_stages?.length) {
    bullets.push(
      t("incident.qa.evidence_trace.summary_missing_stages", { count: journeyMatch.result.missing_stages.length }),
    );
  }

  const deploymentFactors = report?.deployment_risk_assessment?.contributing_factors || [];
  if (parsed.subject.toLowerCase().includes("deployment") && deploymentFactors.length) {
    bullets.push(t("incident.qa.evidence_trace.summary_deployment_factors", { count: deploymentFactors.length }));
  }

  const contractAssessments = report?.contract_risk_assessment?.assessments || [];
  if (parsed.subject.toLowerCase().includes("contract") && contractAssessments.length) {
    const critical = contractAssessments.filter((a) => HIGH_SEVERITIES.has(String(a.overall_risk_level || "").toUpperCase()));
    if (critical.length) {
      bullets.push(t("incident.qa.evidence_trace.summary_critical_contracts", { count: critical.length }));
    }
  }

  const correlations = report?.evidence_correlation?.total_correlations || 0;
  if (correlations > 0 && failed.length === 0) {
    bullets.push(t("incident.qa.evidence_trace.summary_correlations", { count: correlations }));
  }

  const unique = [...new Set(bullets)].slice(0, 6);
  return {
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSummary),
  };
}

function contributorFromHypothesis(hypothesis) {
  return {
    title: hypothesis.statement,
    confidence: hypothesis.confidence,
    confidenceText: formatTraceConfidence(hypothesis.confidence),
  };
}

function contributorFromFactor(factor) {
  return {
    title: factor.title || factor.description,
    confidence: factor.weight ?? factor.confidence,
    confidenceText: formatTraceConfidence(factor.weight ?? factor.confidence),
  };
}

export function buildRootCauseContributors(report, insight, t) {
  const contributors = [];
  const parsed = parseInsightText(insight);

  for (const hypothesis of report?.hypotheses || []) {
    if (!hypothesis?.statement) continue;
    if (
      textMatchesInsightOrDomain(hypothesis.statement, insight)
      || textMatchesInsightOrDomain(hypothesis.basis, insight)
    ) {
      contributors.push(contributorFromHypothesis(hypothesis));
    }
  }

  for (const factor of report?.deployment_risk_assessment?.contributing_factors || []) {
    if (
      parsed.subject.toLowerCase().includes("deployment")
      || textMatchesInsight(factor.title, insight)
      || textMatchesInsight(factor.description, insight)
    ) {
      contributors.push(contributorFromFactor(factor));
    }
  }

  for (const assessment of report?.contract_risk_assessment?.assessments || []) {
    for (const factor of assessment.factors || []) {
      if (textMatchesInsight(factor.title || factor.description, insight) || parsed.subject.toLowerCase().includes("contract")) {
        contributors.push(contributorFromFactor(factor));
      }
    }
  }

  for (const takeaway of report?.decision_center?.key_takeaways || []) {
    if (textMatchesInsight(takeaway.title, insight) || textMatchesInsight(takeaway.description, insight)) {
      contributors.push({
        title: takeaway.title,
        confidence: takeaway.priority ? takeaway.priority / 100 : 0.75,
        confidenceText: formatTraceConfidence(takeaway.priority ? takeaway.priority / 100 : 0.75),
      });
    }
  }

  for (const step of report?.storyline || []) {
    if (textMatchesInsight(step.title, insight) || textMatchesInsight(step.description, insight)) {
      contributors.push({
        title: step.title,
        confidence: step.confidence,
        confidenceText: formatTraceConfidence(step.confidence),
      });
    }
  }

  if (!contributors.length) {
    const topHypothesis = (report?.hypotheses || [])[0];
    if (topHypothesis) contributors.push(contributorFromHypothesis(topHypothesis));
    for (const factor of (report?.deployment_risk_assessment?.contributing_factors || []).slice(0, 2)) {
      contributors.push(contributorFromFactor(factor));
    }
  }

  const deduped = [];
  const seen = new Set();
  for (const item of contributors.sort((a, b) => (Number(b.confidence) || 0) - (Number(a.confidence) || 0))) {
    const key = normalizeToken(item.title);
    if (!key || seen.has(key)) continue;
    seen.add(key);
    deduped.push(item);
  }

  return {
    contributors: deduped.slice(0, 5),
    empty: deduped.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.rootCauseContributors),
    confidenceLabel: t(EVIDENCE_TRACE_I18N_KEYS.confidence),
  };
}

export function buildEvidenceSources(report, insight, t) {
  const failureClusterCount = countFailureClusters(report);
  const systemMemoryCount =
    (report?.evidence_strength?.evidence?.length || 0)
    + (report?.evidence_found?.length || 0)
    + (report?.evidence_strength?.inference?.length || 0);

  const sources = [
    { key: "runs", label: t(EVIDENCE_TRACE_I18N_KEYS.runs), count: (report?.related_runs || []).length },
    { key: "prs", label: t(EVIDENCE_TRACE_I18N_KEYS.prs), count: (report?.related_prs || []).length },
    { key: "failureClusters", label: t(EVIDENCE_TRACE_I18N_KEYS.failureClusters), count: failureClusterCount },
    { key: "systemMemory", label: t(EVIDENCE_TRACE_I18N_KEYS.systemMemory), count: systemMemoryCount },
    {
      key: "historicalIncidents",
      label: t(EVIDENCE_TRACE_I18N_KEYS.historicalIncidents),
      count: report?.historical_learning?.similar_incidents?.length || 0,
    },
  ];

  return {
    sources,
    empty: sources.every((s) => s.count === 0),
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSources),
  };
}

export function buildWhyExplanation(report, insight, t) {
  const parsed = parseInsightText(insight);
  const bullets = [];
  const failed = failedRuns(report);
  const journeyMatch = findMatchingJourney(report, insight);

  if (failed.length >= 2) {
    const pct = Math.max(100, Math.round((failed.length / Math.max((report?.related_runs || []).length, 1)) * 100));
    bullets.push(t("incident.qa.evidence_trace.why_failures_increased", { pct }));
  }

  const depNodes = report?.enterprise_dependency_map?.nodes || [];
  const criticalNodes = depNodes.filter((n) => HIGH_SEVERITIES.has(String(n.risk_level || n.severity || "").toUpperCase()));
  if (criticalNodes.length || parsed.subject.toLowerCase().includes("journey") || parsed.subject.toLowerCase().includes("auth")) {
    bullets.push(t("incident.qa.evidence_trace.why_dependency_map"));
  }

  const similar = report?.historical_learning?.similar_incidents?.length || 0;
  if (similar > 0) {
    bullets.push(t("incident.qa.evidence_trace.why_historical_incidents", { count: similar }));
  }

  if (journeyMatch?.result?.status === "BROKEN") {
    bullets.push(t("incident.qa.evidence_trace.why_journey_broken"));
  }

  if (parsed.subject.toLowerCase().includes("deployment") && report?.deployment_risk_assessment?.summary) {
    bullets.push(report.deployment_risk_assessment.summary);
  }

  if (parsed.subject.toLowerCase().includes("contract")) {
    const assessment = (report?.contract_risk_assessment?.assessments || []).find((a) =>
      HIGH_SEVERITIES.has(String(a.overall_risk_level || "").toUpperCase()),
    );
    if (assessment?.summary) bullets.push(assessment.summary);
  }

  if (report?.executive_quality_report?.historical_pattern_summary) {
    bullets.push(report.executive_quality_report.historical_pattern_summary);
  }

  const unique = [...new Set(bullets.filter(Boolean))].slice(0, 5);

  return {
    prefix: t(EVIDENCE_TRACE_I18N_KEYS.whyPrefix, { subject: parsed.subject, severity: parsed.severity }),
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.whyExplanation),
    show: isCriticalOrHighInsight(insight),
  };
}

function riskItem(title, severity, report, t) {
  return {
    title,
    severity: String(severity || "UNKNOWN").toUpperCase(),
    trace: buildInsightTraceViewModel(report, title, t),
  };
}

export function groupIncidentRisks(report, t) {
  const technical = [];
  const operational = [];
  const executive = [];
  const seen = new Set();

  const add = (bucket, title, severity) => {
    const key = normalizeToken(title);
    if (!key || seen.has(key)) return;
    seen.add(key);
    bucket.push(riskItem(title, severity, report, t));
  };

  for (const assessment of report?.contract_risk_assessment?.assessments || []) {
    const name = assessment.contract?.service_name || assessment.contract_id;
    const level = String(assessment.overall_risk_level || "").toUpperCase();
    if (HIGH_SEVERITIES.has(level)) {
      add(technical, `${name} contract risk is ${level}`, level);
    }
  }

  const deployment = report?.deployment_risk_assessment;
  if (deployment && HIGH_SEVERITIES.has(String(deployment.risk_level || "").toUpperCase())) {
    add(technical, `Deployment Risk is ${String(deployment.risk_level).toUpperCase()}`, deployment.risk_level);
  }

  for (const node of report?.enterprise_dependency_map?.nodes || []) {
    const level = String(node.risk_level || node.severity || "").toUpperCase();
    if (HIGH_SEVERITIES.has(level)) {
      add(technical, `${node.label || node.name || "Dependency"} Risk is ${level}`, level);
    }
  }

  for (const result of report?.data_journey_validation?.results || []) {
    const journey = (report?.data_journey_validation?.journeys || []).find((j) => j.journey_id === result.journey_id);
    const status = String(result.status || "").toUpperCase();
    if (journey && HIGH_SEVERITIES.has(status)) {
      add(operational, `${journey.name} Journey is ${status}`, status);
    }
  }

  for (const signal of report?.multi_environment?.signals || []) {
    const severity = String(signal.severity || "").toUpperCase();
    if (HIGH_SEVERITIES.has(severity)) {
      add(operational, signal.title || `${signal.description?.slice(0, 48)} is ${severity}`, severity);
    }
  }

  const eqr = report?.executive_quality_report;
  if (eqr) {
    for (const risk of eqr.top_risks || []) {
      const parsed = parseInsightText(risk);
      const subject = parsed.subject.toLowerCase();
      if (subject.includes("contract") || subject.includes("dependency") || subject.includes("deployment")) {
        add(technical, risk, parsed.severity);
      } else if (subject.includes("journey") || subject.includes("degradation")) {
        add(operational, risk, parsed.severity);
      } else {
        add(executive, risk, parsed.severity);
      }
    }
    if (HIGH_SEVERITIES.has(String(eqr.overall_risk_level || "").toUpperCase())) {
      add(executive, `Executive Quality Risk is ${String(eqr.overall_risk_level).toUpperCase()}`, eqr.overall_risk_level);
    }
  }

  const decision = report?.decision_center;
  if (decision && ["RED", "ORANGE"].includes(String(decision.overall_status || "").toUpperCase())) {
    add(executive, `Decision Center status is ${String(decision.overall_status).toUpperCase()}`, decision.overall_status);
  }

  return {
    title: t(EVIDENCE_TRACE_I18N_KEYS.riskGroupingTitle),
    technicalRisk: {
      label: t(EVIDENCE_TRACE_I18N_KEYS.technicalRisk),
      items: technical,
    },
    operationalRisk: {
      label: t(EVIDENCE_TRACE_I18N_KEYS.operationalRisk),
      items: operational,
    },
    executiveRisk: {
      label: t(EVIDENCE_TRACE_I18N_KEYS.executiveRisk),
      items: executive,
    },
    hasItems: technical.length + operational.length + executive.length > 0,
  };
}

export function buildInsightTraceViewModel(report, insight, t) {
  const show = isCriticalOrHighInsight(insight);
  return {
    insight,
    show,
    evidenceSummary: buildEvidenceSummary(report, insight, t),
    rootCauseContributors: buildRootCauseContributors(report, insight, t),
    evidenceSources: buildEvidenceSources(report, insight, t),
    whyExplanation: buildWhyExplanation(report, insight, t),
  };
}

export function buildInsightTextForJourney(journey) {
  const name = journey?.name || "Journey";
  const status = String(journey?.status || "UNKNOWN").toUpperCase();
  if (name.toLowerCase().includes("journey")) {
    return `${name} is ${status}`;
  }
  return `${name} Journey is ${status}`;
}

export function buildInsightTextForDeployment(assessment) {
  return `Deployment Risk is ${String(assessment?.risk_level || "UNKNOWN").toUpperCase()}`;
}

export function buildInsightTextForContract(assessment) {
  const name = assessment?.contract?.service_name || assessment?.contract_id || "Contract";
  return `${name} contract risk is ${String(assessment?.overall_risk_level || "UNKNOWN").toUpperCase()}`;
}
