import { describe, it, expect } from "vitest";
import {
  toEnterpriseRiskTier,
  inferFileType,
  inferTestCategory,
  computeV1Confidence,
  buildModifiedFiles,
  collectRiskReasons,
  buildSuggestedActions,
  buildRecommendedTests,
  parseChangedFilesList,
  resolvePrRisk,
  resolveProjectRisk,
  resolveFileChangeType,
  resolveRiskSignals,
  formatRiskSignalImpact,
  sumRiskSignalImpacts,
} from "./prAnalysisViewUtils";

describe("toEnterpriseRiskTier", () => {
  it("maps CRITICAL/HIGH to high", () => {
    expect(toEnterpriseRiskTier("CRITICAL")).toBe("high");
    expect(toEnterpriseRiskTier("high")).toBe("high");
  });
  it("maps MEDIUM to medium", () => {
    expect(toEnterpriseRiskTier("MEDIUM")).toBe("medium");
  });
  it("defaults to low", () => {
    expect(toEnterpriseRiskTier("LOW")).toBe("low");
    expect(toEnterpriseRiskTier()).toBe("low");
  });
});

describe("inferFileType", () => {
  it("detects common extensions", () => {
    expect(inferFileType("src/auth/login.tsx")).toBe("react");
    expect(inferFileType("api/auth.py")).toBe("python");
  });
});

describe("inferTestCategory", () => {
  it("classifies smoke and api from name", () => {
    expect(inferTestCategory({ name: "Login Smoke" })).toBe("smoke");
    expect(inferTestCategory({ name: "users API list" })).toBe("api");
  });
});

describe("computeV1Confidence", () => {
  it("returns low when no mappings", () => {
    expect(computeV1Confidence({})).toBe("low");
  });
  it("returns high when avg confidence >= 0.75", () => {
    expect(computeV1Confidence({
      file_mappings: [{ confidence: 0.8 }, { confidence: 0.9 }],
    })).toBe("high");
  });
});

describe("buildModifiedFiles", () => {
  it("merges v1 mappings and github files", () => {
    const rows = buildModifiedFiles({
      v1: { file_mappings: [{ file_path: "auth.py", module: "Auth", confidence: 0.9 }] },
      ghFiles: [{ filename: "auth.py", additions: 10, deletions: 2, status: "modified" }],
      changedFiles: ["login.tsx"],
    });
    expect(rows.map((r) => r.filePath).sort()).toEqual(["auth.py", "login.tsx"]);
    expect(rows.find((r) => r.filePath === "auth.py").module).toBe("Auth");
    expect(rows.find((r) => r.filePath === "auth.py").additions).toBe(10);
    expect(rows.find((r) => r.filePath === "login.tsx").fromCce).toBe(false);
  });

  it("prefers file_classifications primary_class over inferFileType", () => {
    const rows = buildModifiedFiles({
      v1: {
        file_mappings: [{ file_path: "lib/candidates/queries.ts", module: "Candidatos", confidence: 0.8 }],
        file_classifications: [{
          file_path: "lib/candidates/queries.ts",
          primary_class: "comments",
          confidence: 0.9,
          signals: ["all changed lines are comments"],
        }],
      },
      changedFiles: ["lib/candidates/queries.ts"],
    });
    const row = rows.find((r) => r.filePath === "lib/candidates/queries.ts");
    expect(row.type).toBe("comments");
    expect(row.fromCce).toBe(true);
    expect(row.cceConfidence).toBe(0.9);
    expect(row.cceSignals[0]).toMatch(/comment/i);
  });

  it("falls back to inferFileType when no classification", () => {
    const row = buildModifiedFiles({
      v1: { file_mappings: [{ file_path: "src/auth/login.tsx", module: "Auth", confidence: 0.9 }] },
      changedFiles: ["src/auth/login.tsx"],
    }).find((r) => r.filePath === "src/auth/login.tsx");
    expect(row.fromCce).toBe(false);
    expect(row.type).toBe("react");
  });
});

describe("resolvePrRisk", () => {
  it("prefers pr_risk_* over deprecated risk_*", () => {
    const r = resolvePrRisk({
      pr_risk_score: 12,
      pr_risk_level: "LOW",
      risk_score: 82,
      risk_level: "HIGH",
    });
    expect(r.score).toBe(12);
    expect(r.level).toBe("LOW");
  });

  it("falls back to risk_* when pr_risk_* missing", () => {
    const r = resolvePrRisk({ risk_score: 55, risk_level: "MEDIUM" });
    expect(r.score).toBe(55);
    expect(r.level).toBe("MEDIUM");
  });
});

describe("resolveProjectRisk", () => {
  it("prefers project_risk_* over deprecated risk_*", () => {
    const r = resolveProjectRisk({
      project_risk_score: 18,
      project_risk_level: "LOW",
      risk_score: 82,
      risk_level: "HIGH",
    });
    expect(r.score).toBe(18);
    expect(r.level).toBe("LOW");
  });

  it("falls back to risk_* when project_risk_* missing", () => {
    const r = resolveProjectRisk({ risk_score: 40, risk_level: "MEDIUM" });
    expect(r.score).toBe(40);
    expect(r.level).toBe("MEDIUM");
  });
});

describe("resolveFileChangeType", () => {
  it("returns CCE entry when path matches", () => {
    const r = resolveFileChangeType("README.md", [{
      file_path: "README.md",
      primary_class: "docs",
      confidence: 0.95,
      signals: ["path matches documentation file pattern"],
    }]);
    expect(r.fromCce).toBe(true);
    expect(r.type).toBe("docs");
  });

  it("falls back to inferFileType without classifications", () => {
    const r = resolveFileChangeType("src/foo.py", []);
    expect(r.fromCce).toBe(false);
    expect(r.type).toBe("python");
  });
});

describe("risk explainability utils", () => {
  it("resolveRiskSignals returns backend signals", () => {
    const v1 = {
      risk_signals: [
        { category: "module", title: "Critical module: AUTH", impact: 12, explanation: "Auth scrutiny." },
        { category: "change_type", title: "Comment-only change", impact: 2.5, explanation: "Comments only." },
      ],
    };
    expect(resolveRiskSignals(v1)).toHaveLength(2);
    expect(resolveRiskSignals(null)).toEqual([]);
    expect(resolveRiskSignals({})).toEqual([]);
  });

  it("formatRiskSignalImpact renders signed values", () => {
    expect(formatRiskSignalImpact(12)).toBe("+12");
    expect(formatRiskSignalImpact(-7.9)).toBe("-7.9");
    expect(formatRiskSignalImpact(0)).toBe("0");
  });

  it("sumRiskSignalImpacts adds impacts", () => {
    const signals = [
      { impact: 2.5 },
      { impact: 12 },
      { impact: -7.9 },
    ];
    expect(sumRiskSignalImpacts(signals)).toBeCloseTo(6.6, 1);
  });
});

describe("collectRiskReasons", () => {
  it("dedupes v1 reasoning and module reasons", () => {
    const lines = collectRiskReasons({
      v1: {
        reasoning: ["affects login"],
        impacted_modules: [{ module: "Auth", reasons: ["affects login", "permissions"] }],
      },
    });
    expect(lines).toEqual(["affects login", "permissions"]);
  });
  it("uses legacy risk_reasons", () => {
    expect(collectRiskReasons({ legacy: { risk_reasons: ["x"] } })).toEqual(["x"]);
  });
});

describe("buildSuggestedActions", () => {
  it("builds run and review actions from v1", () => {
    const actions = buildSuggestedActions({
      v1: {
        recommended_tests: [{ test_case_id: "t1", name: "Login Smoke", reason: "auth", module: "Auth" }],
      },
      modifiedFiles: [{ filePath: "auth.py", module: "Auth" }],
    });
    expect(actions.some((a) => a.kind === "run_test" && a.testCaseId === "t1")).toBe(true);
    expect(actions.some((a) => a.kind === "review_file" && a.label === "auth.py")).toBe(true);
  });
});

describe("buildRecommendedTests", () => {
  it("maps v1 recommended tests with categories", () => {
    const tests = buildRecommendedTests({
      v1: {
        recommended_tests: [{ test_case_id: "t1", name: "Login Smoke", module: "Auth", reason: "auth flow" }],
      },
    });
    expect(tests[0].category).toBe("smoke");
  });
});

describe("parseChangedFilesList", () => {
  it("parses newline and comma separated paths", () => {
    expect(parseChangedFilesList("a.py\nb.tsx, c.js")).toEqual(["a.py", "b.tsx", "c.js"]);
  });
});
