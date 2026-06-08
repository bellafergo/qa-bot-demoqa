// src/components/pr/PRAnalysisEnterpriseView.jsx
import React, { useMemo } from "react";
import { Link, useNavigate } from "react-router-dom";
import { useLang } from "../../i18n/LangContext";
import {
  buildModifiedFiles,
  buildRecommendedTests,
  buildSuggestedActions,
  collectRiskReasons,
  computeV1Confidence,
  resolvePrRisk,
  resolveProjectRisk,
  toEnterpriseRiskTier,
} from "../../utils/prAnalysisViewUtils";

function EnterpriseRiskBadge({ tier, t }) {
  const key = `pr.enterprise.status.${tier || "low"}`;
  const cls =
    tier === "high" ? "badge-red" : tier === "medium" ? "badge-orange" : "badge-green";
  return <span className={`badge ${cls}`}>{t(key)}</span>;
}

function RiskLevelBadge({ level, t }) {
  const lv = (level || "LOW").toUpperCase();
  const cls =
    lv === "CRITICAL" || lv === "HIGH"
      ? "badge-red"
      : lv === "MEDIUM"
        ? "badge-blue"
        : "badge-gray";
  const label = t(`knowledge.risk_level.${lv}`);
  return (
    <span className={`badge ${cls}`}>
      {label === `knowledge.risk_level.${lv}` ? lv : label}
    </span>
  );
}

function fmtDate(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short", day: "numeric", hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

function FileTypeCell({ row, t }) {
  const labelKey = `pr.enterprise.change_class.${row.type}`;
  const translated = t(labelKey);
  const label = row.fromCce ? (translated === labelKey ? row.type : translated) : row.type;
  return (
    <td style={{ fontSize: 12 }}>
      {row.fromCce ? (
        <span className="badge badge-gray">{label}</span>
      ) : (
        label
      )}
      {row.fromCce && (row.cceSignals?.[0] || row.cceConfidence != null) ? (
        <div
          style={{ fontSize: 10, color: "var(--text-3)", marginTop: 4, lineHeight: 1.4 }}
          title={(row.cceSignals || []).join("; ")}
        >
          {row.cceSignals?.[0] || ""}
          {row.cceConfidence != null
            ? `${row.cceSignals?.[0] ? " · " : ""}${Math.round(row.cceConfidence * 100)}%`
            : ""}
        </div>
      ) : null}
    </td>
  );
}

function KnowledgeBanner({ hasKnowledge, loading, t }) {
  return (
    <div
      className="card"
      style={{
        padding: "12px 16px",
        marginBottom: 16,
        borderColor: hasKnowledge ? "var(--green)" : "var(--border)",
        background: "var(--surface)",
      }}
    >
      <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.5 }}>
        {loading ? "…" : hasKnowledge ? t("pr.enterprise.knowledge.used") : t("pr.enterprise.knowledge.unavailable")}
      </div>
      {!loading && !hasKnowledge ? (
        <Link to="/knowledge" className="btn btn-secondary btn-sm" style={{ marginTop: 10 }}>
          {t("pr.enterprise.knowledge.cta")}
        </Link>
      ) : null}
    </div>
  );
}

function EnqueueFeedback({ enqueueResult, t, navigate }) {
  if (!enqueueResult) return null;
  return (
    <div className={`alert ${!enqueueResult.ok ? "alert-error" : "alert-success"}`} style={{ marginBottom: 16 }}>
      {!enqueueResult.ok ? (
        `✗ ${enqueueResult.error}`
      ) : (
        <div style={{ display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap" }}>
          <span>
            {t("pr.result.enqueued_manual_prefix")} {enqueueResult.job_id?.slice(0, 14)}… {t("pr.result.enqueued_manual_suffix")} — {enqueueResult.total_count} {t("pr.result.enqueue_tests")}
          </span>
          <button type="button" className="btn btn-secondary btn-sm" style={{ fontSize: 11 }} onClick={() => navigate("/execution")}>
            {t("pr.result.go_execution")}
          </button>
        </div>
      )}
    </div>
  );
}

export default function PRAnalysisEnterpriseView({
  mode,
  v1,
  legacy,
  form = {},
  ghFiles = [],
  changedFilesList = [],
  analyzedAt,
  hasKnowledge,
  knowledgeLoading,
  enqueueResult,
  enqueueing,
  onEnqueue,
  onSendToRisk,
  sendingRisk,
  onSaveDrafts,
  savingDrafts,
  saveDraftsResult,
  showLegacyExtras = true,
}) {
  const { t } = useLang();
  const navigate = useNavigate();

  const executive = useMemo(() => {
    if (mode === "v1" && v1) {
      const prRisk = resolvePrRisk(v1);
      return {
        prId: form.pr_id || "—",
        title: form.title || v1.project_name || v1.project_id,
        tier: toEnterpriseRiskTier(prRisk.level),
        riskLevel: prRisk.level,
        riskScore: prRisk.score,
        confidence: computeV1Confidence(v1),
        summary: v1.summary,
        changeSurface: null,
      };
    }
    if (mode === "legacy" && legacy) {
      return {
        prId: legacy.pr_id || form.pr_id || "—",
        title: form.title || "—",
        tier: toEnterpriseRiskTier(legacy.inferred_risk_level),
        riskLevel: legacy.inferred_risk_level,
        riskScore: null,
        confidence: legacy.confidence,
        summary: legacy.summary,
        changeSurface: legacy.change_surface,
      };
    }
    return null;
  }, [mode, v1, legacy, form]);

  const modifiedFiles = useMemo(
    () => buildModifiedFiles({ v1, ghFiles, changedFiles: changedFilesList }),
    [v1, ghFiles, changedFilesList],
  );

  const recommendedTests = useMemo(
    () => buildRecommendedTests({ v1, legacy }),
    [v1, legacy],
  );

  const riskReasons = useMemo(
    () => collectRiskReasons({ v1, legacy }),
    [v1, legacy],
  );

  const suggestedActions = useMemo(
    () => buildSuggestedActions({ v1, legacy, modifiedFiles, changedFilesList }),
    [v1, legacy, modifiedFiles, changedFilesList],
  );

  const impactedModules = v1?.impacted_modules?.length
    ? v1.impacted_modules
    : (legacy?.inferred_modules || []).map((m) => ({ module: m, reasons: [], module_risk_level: null, module_risk_score: null, matched_files: [] }));

  const projectRisk = useMemo(
    () => (mode === "v1" && v1 ? resolveProjectRisk(v1) : { score: null, level: null }),
    [mode, v1],
  );

  if (!executive) return null;

  const testCount = recommendedTests.length;
  const canEnqueue = testCount > 0 && !enqueueResult?.ok;

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
      <KnowledgeBanner hasKnowledge={hasKnowledge} loading={knowledgeLoading} t={t} />
      <EnqueueFeedback enqueueResult={enqueueResult} t={t} navigate={navigate} />

      {/* Executive summary */}
      <div className="card" style={{ padding: "18px 22px" }}>
        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.07em", marginBottom: 10 }}>
          {t("pr.enterprise.summary_title")}
        </div>
        <div style={{ fontSize: 18, fontWeight: 700, color: "var(--text-1)", marginBottom: 8 }}>
          {executive.prId !== "—" ? `${t("pr.enterprise.pr_label")} #${executive.prId}` : t("pr.enterprise.manual_analysis")}
        </div>
        {executive.title && executive.title !== "—" ? (
          <div style={{ fontSize: 14, color: "var(--text-2)", marginBottom: 14 }}>{executive.title}</div>
        ) : null}

        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(130px, 1fr))", gap: 14 }}>
          <div>
            <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase" }}>{t("pr.enterprise.status_label")}</div>
            <div style={{ marginTop: 6 }}><EnterpriseRiskBadge tier={executive.tier} t={t} /></div>
          </div>
          {executive.riskScore != null ? (
            <div>
              <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase" }}>{t("pr.enterprise.risk_score")}</div>
              <div style={{ marginTop: 6, fontSize: 16, fontWeight: 700 }}>{Math.round(executive.riskScore)}/100</div>
            </div>
          ) : null}
          <div>
            <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase" }}>{t("pr.enterprise.confidence")}</div>
            <div style={{ marginTop: 6, fontSize: 14, fontWeight: 600 }}>{executive.confidence || "—"}</div>
          </div>
          <div>
            <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase" }}>{t("pr.enterprise.analyzed_at")}</div>
            <div style={{ marginTop: 6, fontSize: 13 }}>{fmtDate(analyzedAt)}</div>
          </div>
          {executive.changeSurface ? (
            <div>
              <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase" }}>{t("pr.enterprise.change_surface")}</div>
              <div style={{ marginTop: 6, fontSize: 13 }}>{t(`pr.enterprise.surface.${executive.changeSurface}`)}</div>
            </div>
          ) : null}
        </div>

        {executive.summary ? (
          <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.6, marginTop: 14, marginBottom: 0 }}>{executive.summary}</p>
        ) : null}

        {mode === "v1" && v1 && (projectRisk.level || projectRisk.score != null) ? (
          <div style={{ marginTop: 12, fontSize: 11, color: "var(--text-3)" }}>
            {t("pr.enterprise.project_risk_note")}{" "}
            {projectRisk.level ? <RiskLevelBadge level={projectRisk.level} t={t} /> : null}
            {projectRisk.score != null ? (
              <span style={{ marginLeft: 6 }}>{Math.round(projectRisk.score)}/100</span>
            ) : null}
          </div>
        ) : null}
      </div>

      {/* Modified files */}
      {modifiedFiles.length > 0 ? (
        <div className="card">
          <div className="section-title" style={{ marginBottom: 12 }}>{t("pr.enterprise.files_changed")}</div>
          <table className="data-table">
            <thead>
              <tr>
                <th>{t("pr.enterprise.col.file")}</th>
                <th>{t("pr.enterprise.col.type")}</th>
                <th>{t("pr.enterprise.col.module")}</th>
              </tr>
            </thead>
            <tbody>
              {modifiedFiles.map((row) => (
                <tr key={row.filePath}>
                  <td style={{ fontFamily: "monospace", fontSize: 11 }}>{row.filePath}</td>
                  <FileTypeCell row={row} t={t} />
                  <td style={{ fontSize: 12 }}>{row.module}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      ) : null}

      {/* Impacted modules */}
      <div className="card">
        <div className="section-title" style={{ marginBottom: 12 }}>{t("pr.enterprise.modules_impacted")}</div>
        {impactedModules.length ? (
          <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
            {impactedModules.map((m) => (
              <div key={m.module} style={{ borderLeft: "3px solid var(--accent)", paddingLeft: 12 }}>
                <div style={{ display: "flex", gap: 8, alignItems: "center", flexWrap: "wrap" }}>
                  <strong>{m.module}</strong>
                  {m.module_risk_level ? <RiskLevelBadge level={m.module_risk_level} t={t} /> : null}
                  {m.module_risk_score != null ? (
                    <span className="badge badge-gray">{m.module_risk_score}/100</span>
                  ) : null}
                </div>
                {(m.matched_files || []).length > 0 ? (
                  <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>
                    {(m.matched_files || []).join(", ")}
                  </div>
                ) : null}
                {(m.reasons || []).length > 0 ? (
                  <ul style={{ margin: "6px 0 0", paddingLeft: 18, fontSize: 12, color: "var(--text-3)", lineHeight: 1.6 }}>
                    {m.reasons.map((r, i) => <li key={i}>{r}</li>)}
                  </ul>
                ) : null}
              </div>
            ))}
          </div>
        ) : (
          <p style={{ fontSize: 13, color: "var(--text-3)" }}>{t("pr.v1.no_modules")}</p>
        )}
      </div>

      {/* Recommended tests */}
      <div className="card">
        <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12, flexWrap: "wrap", gap: 8 }}>
          <div className="section-title" style={{ margin: 0 }}>{t("pr.enterprise.recommended_tests")}</div>
          {canEnqueue ? (
            <button type="button" className="btn btn-primary btn-sm" onClick={onEnqueue} disabled={enqueueing}>
              {enqueueing ? t("pr.result.enqueueing") : `${t("pr.result.enqueue_prefix")} ${testCount} ${t("pr.result.enqueue_tests")}`}
            </button>
          ) : null}
        </div>
        {recommendedTests.length ? (
          <table className="data-table">
            <thead>
              <tr>
                <th>{t("pr.enterprise.col.test_type")}</th>
                <th>{t("knowledge.col.test")}</th>
                <th>{t("knowledge.col.module")}</th>
                <th>{t("pr.v1.col.reason")}</th>
              </tr>
            </thead>
            <tbody>
              {recommendedTests.map((tc) => (
                <tr key={tc.testCaseId}>
                  <td><span className="badge badge-gray">{t(`pr.enterprise.test_type.${tc.category}`)}</span></td>
                  <td style={{ fontFamily: "monospace", fontSize: 12 }}>{tc.name}</td>
                  <td>{tc.module}</td>
                  <td style={{ fontSize: 12 }}>{tc.reason}</td>
                </tr>
              ))}
            </tbody>
          </table>
        ) : (
          <p style={{ fontSize: 13, color: "var(--text-3)" }}>{t("pr.v1.no_tests")}</p>
        )}
      </div>

      {/* Why risky */}
      {riskReasons.length > 0 ? (
        <div className="card">
          <div className="section-title" style={{ marginBottom: 12 }}>{t("pr.enterprise.why_risky")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {riskReasons.map((line, i) => <li key={i}>{line}</li>)}
          </ul>
        </div>
      ) : null}

      {/* Suggested actions */}
      {suggestedActions.length > 0 ? (
        <div className="card">
          <div className="section-title" style={{ marginBottom: 12 }}>{t("pr.enterprise.suggested_actions")}</div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none", display: "flex", flexDirection: "column", gap: 8 }}>
            {suggestedActions.map((a) => (
              <li
                key={a.key}
                style={{
                  display: "flex",
                  gap: 10,
                  alignItems: "flex-start",
                  padding: "10px 12px",
                  border: "1px solid var(--border)",
                  borderRadius: 6,
                  background: "var(--surface)",
                  fontSize: 13,
                }}
              >
                <span style={{ color: "var(--green)", fontWeight: 700 }}>✓</span>
                <div>
                  <div style={{ fontWeight: 600, color: "var(--text-1)" }}>
                    {a.kind === "run_test" ? t("pr.enterprise.action.run_test", { name: a.label }) : t("pr.enterprise.action.review_file", { file: a.label })}
                  </div>
                  {a.detail ? <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 2 }}>{a.detail}</div> : null}
                </div>
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {/* Legacy-only extras */}
      {mode === "legacy" && showLegacyExtras && legacy ? (
        <>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
            <button type="button" className="btn btn-secondary btn-sm" onClick={onSendToRisk} disabled={sendingRisk || !legacy.inferred_modules?.length}>
              {sendingRisk ? "…" : t("pr.send_risk.btn")}
            </button>
          </div>
          {legacy.suggested_new_tests?.length > 0 ? (
            <div className="card">
              <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", gap: 10, marginBottom: 12, flexWrap: "wrap" }}>
                <div className="section-title" style={{ margin: 0 }}>
                  {legacy.suggested_new_tests.length} {t("pr.result.draft_suggestions")}
                </div>
                <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
                  {!saveDraftsResult?.saved_count && !saveDraftsResult?.error ? (
                    <button type="button" className="btn btn-primary btn-sm" onClick={onSaveDrafts} disabled={savingDrafts}>
                      {savingDrafts ? t("pr.save_drafts.saving") : t("pr.save_drafts.btn")}
                    </button>
                  ) : null}
                  <button type="button" className="btn btn-secondary btn-sm" onClick={() => navigate("/drafts")}>
                    {t("pr.save_drafts.open")}
                  </button>
                </div>
              </div>
              {saveDraftsResult ? (
                <div className={`alert ${saveDraftsResult.error ? "alert-error" : "alert-success"}`} style={{ marginBottom: 12, fontSize: 12 }}>
                  {saveDraftsResult.error
                    ? `✗ ${saveDraftsResult.error}`
                    : (
                      <span>
                        ✓ {saveDraftsResult.saved_count ?? 0} {t("pr.save_drafts.saved")}
                        {saveDraftsResult.error_count > 0 ? ` · ${saveDraftsResult.error_count} ${t("pr.save_drafts.batch_failed")}` : ""}
                        {" "}
                        <button type="button" className="btn btn-secondary btn-sm" style={{ fontSize: 11, marginLeft: 8 }} onClick={() => navigate("/drafts")}>
                          {t("pr.save_drafts.open")}
                        </button>
                      </span>
                    )}
                </div>
              ) : null}
              <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
                {legacy.suggested_new_tests.map((d) => (
                  <div key={d.draft_id} style={{ borderLeft: "3px solid var(--accent)", paddingLeft: 12 }}>
                    <div style={{ fontWeight: 600, fontSize: 13, marginBottom: 4 }}>{d.name}</div>
                    <div style={{ display: "flex", gap: 6, flexWrap: "wrap", marginBottom: 6 }}>
                      <span className="badge badge-gray">{d.module}</span>
                      <span className="badge badge-gray">{t("pr.result.confidence")} {d.confidence}</span>
                    </div>
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>{d.rationale}</div>
                  </div>
                ))}
              </div>
            </div>
          ) : null}
        </>
      ) : null}
    </div>
  );
}
