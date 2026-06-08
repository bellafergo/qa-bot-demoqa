// src/utils/prAnalysisViewUtils.js
/** Pure transforms for PR Intelligence enterprise UI — no invented risk data. */

/** Map v1 CRITICAL/HIGH/MEDIUM/LOW or legacy low/medium/high → enterprise tier. */
export function toEnterpriseRiskTier(level) {
  const lv = String(level || "").toUpperCase();
  if (lv === "CRITICAL" || lv === "HIGH" || lv === "high") return "high";
  if (lv === "MEDIUM" || lv === "medium") return "medium";
  return "low";
}

export function inferFileType(filePath) {
  const p = String(filePath || "").toLowerCase();
  if (p.endsWith(".py")) return "python";
  if (p.endsWith(".tsx") || p.endsWith(".jsx")) return "react";
  if (p.endsWith(".ts") || p.endsWith(".js")) return "javascript";
  if (p.endsWith(".vue")) return "vue";
  if (p.endsWith(".go")) return "go";
  if (p.endsWith(".java")) return "java";
  if (p.endsWith(".sql")) return "sql";
  if (p.includes("/api/") || p.includes("api_")) return "api";
  if (p.includes("test_") || p.includes(".test.") || p.includes(".spec.")) return "test";
  return "other";
}

export function inferTestCategory({ name = "", reason = "", testCaseId = "", isApi = false, isUi = false }) {
  const blob = `${name} ${reason} ${testCaseId}`.toLowerCase();
  if (/\bsmoke\b/.test(blob)) return "smoke";
  if (isApi || /\bapi\b/.test(blob) || /\bgraphql\b/.test(blob)) return "api";
  if (isUi || /\bui\b/.test(blob) || /\be2e\b/.test(blob)) return "ui";
  if (/\bregression\b/.test(blob) || /\brecurrent\b/.test(blob) || /\bfail/.test(blob)) return "regression";
  return "general";
}

export function computeV1Confidence(v1) {
  const mappings = v1?.file_mappings || [];
  if (!mappings.length) return "low";
  const avg =
    mappings.reduce((s, m) => s + (Number(m.confidence) || 0), 0) / mappings.length;
  if (avg >= 0.75) return "high";
  if (avg >= 0.45) return "medium";
  return "low";
}

/** PR-scoped risk from v1 report (prefers pr_risk_*, falls back to deprecated risk_*). */
export function resolvePrRisk(v1) {
  if (!v1) return { score: null, level: null };
  const scoreRaw = v1.pr_risk_score ?? v1.risk_score;
  const level = v1.pr_risk_level ?? v1.risk_level ?? null;
  const score = scoreRaw != null && scoreRaw !== "" ? Number(scoreRaw) : null;
  return {
    score: Number.isFinite(score) ? score : null,
    level: level ? String(level) : null,
  };
}

/** Project baseline risk (prefers project_risk_*, falls back to deprecated risk_*). */
export function resolveProjectRisk(v1) {
  if (!v1) return { score: null, level: null };
  const scoreRaw = v1.project_risk_score ?? v1.risk_score;
  const level = v1.project_risk_level ?? v1.risk_level ?? null;
  const score = scoreRaw != null && scoreRaw !== "" ? Number(scoreRaw) : null;
  return {
    score: Number.isFinite(score) ? score : null,
    level: level ? String(level) : null,
  };
}

/** Risk explainability signals from PR Risk Composer (v1.3+). */
export function resolveRiskSignals(v1) {
  if (!v1 || !Array.isArray(v1.risk_signals)) return [];
  return v1.risk_signals.filter((s) => s && (s.title || s.explanation));
}

export function formatRiskSignalImpact(impact) {
  const n = Number(impact);
  if (!Number.isFinite(n) || n === 0) return "0";
  const rounded = Math.round(n * 10) / 10;
  return rounded > 0 ? `+${rounded}` : `${rounded}`;
}

export function sumRiskSignalImpacts(signals) {
  return (signals || []).reduce((total, s) => {
    const n = Number(s?.impact);
    return total + (Number.isFinite(n) ? n : 0);
  }, 0);
}

function _classificationByPath(fileClassifications) {
  const map = {};
  for (const entry of fileClassifications || []) {
    const path = entry?.file_path;
    if (path) map[path] = entry;
  }
  return map;
}

/** Resolve display type for a file: CCE primary_class when present, else inferFileType(). */
export function resolveFileChangeType(filePath, fileClassifications) {
  const cce = _classificationByPath(fileClassifications)[filePath];
  if (cce?.primary_class) {
    return {
      type: String(cce.primary_class),
      fromCce: true,
      cceConfidence: cce.confidence != null ? Number(cce.confidence) : null,
      cceSignals: Array.isArray(cce.signals) ? cce.signals.filter(Boolean) : [],
    };
  }
  return {
    type: inferFileType(filePath),
    fromCce: false,
    cceConfidence: null,
    cceSignals: [],
  };
}

/** Build modified-files rows from mappings + optional GitHub file stats. */
export function buildModifiedFiles({ v1, ghFiles = [], changedFiles = [] }) {
  const ghByName = Object.fromEntries((ghFiles || []).map((f) => [f.filename, f]));
  const mappingByFile = Object.fromEntries((v1?.file_mappings || []).map((m) => [m.file_path, m]));
  const fileClassifications = v1?.file_classifications || [];

  const paths = new Set([
    ...(v1?.file_mappings || []).map((m) => m.file_path),
    ...(fileClassifications || []).map((c) => c.file_path),
    ...(changedFiles || []),
    ...(ghFiles || []).map((f) => f.filename),
  ]);

  return [...paths].filter(Boolean).sort().map((filePath) => {
    const map = mappingByFile[filePath];
    const gh = ghByName[filePath];
    const changeType = resolveFileChangeType(filePath, fileClassifications);
    return {
      filePath,
      type: changeType.type,
      fromCce: changeType.fromCce,
      cceConfidence: changeType.cceConfidence,
      cceSignals: changeType.cceSignals,
      module: map?.module || "—",
      confidence: map?.confidence ?? null,
      additions: gh?.additions ?? null,
      deletions: gh?.deletions ?? null,
      status: gh?.status || "",
    };
  });
}

export function collectRiskReasons({ v1, legacy }) {
  if (v1) {
    const lines = [...(v1.reasoning || [])];
    for (const m of v1.impacted_modules || []) {
      for (const r of m.reasons || []) {
        if (r && !lines.includes(r)) lines.push(r);
      }
    }
    return lines.slice(0, 12);
  }
  return (legacy?.risk_reasons || []).slice(0, 12);
}

export function buildSuggestedActions({ v1, legacy, modifiedFiles = [], changedFilesList = [] }) {
  const actions = [];

  if (v1) {
    for (const tc of v1.recommended_tests || []) {
      actions.push({
        key: `run-${tc.test_case_id}`,
        kind: "run_test",
        label: tc.name || tc.test_case_id,
        detail: tc.reason || tc.module || "",
        testCaseId: tc.test_case_id,
      });
    }
    for (const row of modifiedFiles.slice(0, 6)) {
      actions.push({
        key: `review-${row.filePath}`,
        kind: "review_file",
        label: row.filePath,
        detail: row.module !== "—" ? row.module : "",
      });
    }
  }

  if (legacy) {
    const ids = legacy.matched_test_case_ids || [];
    for (const id of ids.slice(0, 8)) {
      actions.push({
        key: `run-${id}`,
        kind: "run_test",
        label: id,
        detail: legacy.test_match_reasons?.[id] || "",
        testCaseId: id,
      });
    }
    for (const f of changedFilesList.slice(0, 6)) {
      actions.push({ key: `review-${f}`, kind: "review_file", label: f, detail: "" });
    }
  }

  const seen = new Set();
  return actions.filter((a) => {
    if (seen.has(a.key)) return false;
    seen.add(a.key);
    return true;
  }).slice(0, 12);
}

export function parseChangedFilesList(raw) {
  if (Array.isArray(raw)) return raw.filter(Boolean);
  return String(raw || "")
    .split(/[\n,]+/)
    .map((s) => s.trim())
    .filter(Boolean);
}

export function buildRecommendedTests({ v1, legacy }) {
  if (v1) {
    const apiSet = new Set();
    const uiSet = new Set();
    return (v1.recommended_tests || []).map((tc) => ({
      testCaseId: tc.test_case_id,
      name: tc.name || tc.test_case_id,
      module: tc.module || "—",
      reason: tc.reason || "—",
      category: inferTestCategory({
        name: tc.name,
        reason: tc.reason,
        testCaseId: tc.test_case_id,
        isApi: apiSet.has(tc.test_case_id),
        isUi: uiSet.has(tc.test_case_id),
      }),
    }));
  }

  if (!legacy) return [];

  const apiSet = new Set(legacy.recommended_api_tests || []);
  const uiSet = new Set(legacy.recommended_ui_tests || []);

  return (legacy.matched_test_case_ids || []).map((id) => ({
    testCaseId: id,
    name: id,
    module: "—",
    reason: legacy.test_match_reasons?.[id] || "—",
    category: inferTestCategory({
      testCaseId: id,
      reason: legacy.test_match_reasons?.[id],
      isApi: apiSet.has(id),
      isUi: uiSet.has(id),
    }),
  }));
}
