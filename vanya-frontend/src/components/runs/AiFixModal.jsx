import React, { useState, useEffect } from "react";
import { getTest, updateTest, previewCatalogAiEdit, apiErrorMessage } from "../../api";
import { useLang } from "../../i18n/LangContext";
import {
  runPersistedLookupId,
  resolveTestCaseIdFromRun,
  findFailingStepInfo,
  buildAiCatalogInstructionPrefill,
  fmtMs,
} from "../../utils/runHelpers.js";
import ConfirmModal from "./ConfirmModal.jsx";

export default function AiFixModal({
  open,
  onClose,
  detail,
  onApplied,
}) {
  const { t } = useLang();
  const [aiCatalogFetchLoading, setAiCatalogFetchLoading] = useState(false);
  const [aiCatalogTest, setAiCatalogTest] = useState(null);
  const [aiCatalogFetchError, setAiCatalogFetchError] = useState("");
  const [aiInstruction, setAiInstruction] = useState("");
  const [aiGenLoading, setAiGenLoading] = useState(false);
  const [aiProposal, setAiProposal] = useState(null);
  const [aiModalError, setAiModalError] = useState("");
  const [confirmAiApplyOpen, setConfirmAiApplyOpen] = useState(false);
  const [aiApplyBusy, setAiApplyBusy] = useState(false);
  const [aiApplyError, setAiApplyError] = useState("");
  const [showMainAfterConfirmCancel, setShowMainAfterConfirmCancel] = useState(false);

  function resetAiCatalogFixState() {
    onClose();
    setAiCatalogFetchLoading(false);
    setAiCatalogTest(null);
    setAiCatalogFetchError("");
    setAiInstruction("");
    setAiGenLoading(false);
    setAiProposal(null);
    setAiModalError("");
    setConfirmAiApplyOpen(false);
    setAiApplyBusy(false);
    setAiApplyError("");
    setShowMainAfterConfirmCancel(false);
  }

  useEffect(() => {
    if (!open || !detail) return;
    if (detail.error) return;
    const tcId = resolveTestCaseIdFromRun(detail);
    setAiModalError("");
    setAiProposal(null);
    setAiCatalogFetchError("");
    setAiCatalogTest(null);
    const failing = findFailingStepInfo(detail);
    setAiInstruction(buildAiCatalogInstructionPrefill(detail, failing, t));
    setShowMainAfterConfirmCancel(false);
    if (!tcId) {
      setAiCatalogFetchError(t("runs.ai_fix.no_test_case"));
      return;
    }
    setAiCatalogFetchLoading(true);
    (async () => {
      try {
        const full = await getTest(tcId);
        setAiCatalogTest(full);
      } catch (e) {
        setAiCatalogFetchError(apiErrorMessage(e) || t("runs.ai_fix.catalog_load_failed"));
      } finally {
        setAiCatalogFetchLoading(false);
      }
    })();
  }, [open, detail]);

  async function handleAiFixGenerate() {
    if (!aiCatalogTest || !aiInstruction.trim()) return;
    setAiGenLoading(true);
    setAiModalError("");
    setAiProposal(null);
    try {
      const steps = Array.isArray(aiCatalogTest.steps) ? aiCatalogTest.steps : [];
      const assertions = Array.isArray(aiCatalogTest.assertions) ? aiCatalogTest.assertions : [];
      const result = await previewCatalogAiEdit({
        test_case_id: aiCatalogTest.test_case_id,
        name: aiCatalogTest.name,
        module: aiCatalogTest.module,
        priority: aiCatalogTest.priority,
        steps,
        assertions,
        instruction: aiInstruction.trim(),
      });
      setAiProposal(result);
    } catch (e) {
      setAiModalError(apiErrorMessage(e) || t("runs.ai_fix.generate_failed"));
    } finally {
      setAiGenLoading(false);
    }
  }

  const showMainModal = open || showMainAfterConfirmCancel;

  return (
    <>
      {showMainModal && (
        <div
          style={{
            position: "fixed",
            inset: 0,
            background: "rgba(0,0,0,0.5)",
            zIndex: 10000,
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            padding: 16,
          }}
          role="dialog"
          aria-modal="true"
        >
          <div className="card" style={{ width: "min(720px, 100%)", maxHeight: "90vh", overflow: "auto", padding: 0 }}>
            <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
              <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)" }}>{t("runs.ai_fix.modal_title")}</div>
              <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4 }}>{runPersistedLookupId(detail)}</div>
            </div>
            <div style={{ padding: "14px 20px" }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 8 }}>
                {t("runs.ai_fix.failure_summary")}
              </div>
              <div
                style={{
                  fontSize: 12,
                  color: "var(--text-2)",
                  lineHeight: 1.55,
                  marginBottom: 14,
                  padding: 10,
                  background: "var(--surface-2)",
                  borderRadius: 6,
                  border: "1px solid var(--border)",
                }}
              >
                <div>
                  <span style={{ color: "var(--text-3)" }}>{t("runs.ai_fix.field_test_case")}</span>{" "}
                  <code>{resolveTestCaseIdFromRun(detail) || "—"}</code>
                </div>
                <div style={{ marginTop: 6 }}>
                  <span style={{ color: "var(--text-3)" }}>{t("runs.ai_fix.field_run_id")}</span>{" "}
                  <code style={{ wordBreak: "break-all" }}>{runPersistedLookupId(detail)}</code>
                </div>
                {detail.evidence_id && (
                  <div style={{ marginTop: 6 }}>
                    <span style={{ color: "var(--text-3)" }}>{t("runs.ai_fix.field_evidence")}</span>{" "}
                    <code style={{ wordBreak: "break-all" }}>{String(detail.evidence_id)}</code>
                  </div>
                )}
                <div style={{ marginTop: 6 }}>
                  <span style={{ color: "var(--text-3)" }}>{t("runs.ai_fix.field_status")}</span>{" "}
                  {String(detail.status || "—")}
                  {detail.duration_ms != null && (
                    <span style={{ marginLeft: 8, color: "var(--text-3)" }}>
                      · {fmtMs(detail.duration_ms)}
                    </span>
                  )}
                </div>
                {(detail.error_summary || detail.reason || detail.message || detail.error_message) && (
                  <div style={{ marginTop: 8 }}>
                    <span style={{ color: "var(--text-3)" }}>{t("runs.ai_fix.field_error")}</span>{" "}
                    {detail.error_summary || detail.reason || detail.message || detail.error_message}
                  </div>
                )}
                {(() => {
                  const fs = findFailingStepInfo(detail);
                  if (!fs) return null;
                  return (
                    <div style={{ marginTop: 8 }}>
                      <span style={{ color: "var(--text-3)" }}>{t("runs.ai_fix.field_failing_step")}</span>{" "}
                      #{fs.index} — {fs.action}
                    </div>
                  );
                })()}
                {(detail.correlation_id || detail.meta?.correlation_id) && (
                  <div style={{ marginTop: 6, fontSize: 11, color: "var(--text-3)" }}>
                    correlation: {detail.correlation_id || detail.meta?.correlation_id}
                  </div>
                )}
              </div>

              {aiCatalogFetchLoading && (
                <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12 }}>{t("runs.ai_fix.loading_catalog")}</div>
              )}
              {aiCatalogFetchError && (
                <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
                  {aiCatalogFetchError}
                </div>
              )}
              {aiModalError && (
                <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
                  {aiModalError}
                </div>
              )}

              {!aiProposal ? (
                <>
                  <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>
                    {t("runs.ai_fix.instruction_label")}
                  </label>
                  <textarea
                    className="input"
                    rows={5}
                    style={{ width: "100%", fontSize: 12, resize: "vertical" }}
                    value={aiInstruction}
                    onChange={e => setAiInstruction(e.target.value)}
                    disabled={aiGenLoading || !!aiCatalogFetchError || aiCatalogFetchLoading || !aiCatalogTest}
                  />
                  <div style={{ display: "flex", gap: 8, marginTop: 12, flexWrap: "wrap", justifyContent: "flex-end" }}>
                    <button
                      type="button"
                      className="btn btn-secondary btn-sm"
                      onClick={resetAiCatalogFixState}
                      disabled={aiGenLoading}
                    >
                      {t("common.cancel")}
                    </button>
                    <button
                      type="button"
                      className="btn btn-primary btn-sm"
                      onClick={handleAiFixGenerate}
                      disabled={
                        aiGenLoading ||
                        !aiCatalogTest ||
                        !aiInstruction.trim() ||
                        aiCatalogFetchLoading ||
                        !!aiCatalogFetchError
                      }
                    >
                      {aiGenLoading ? t("runs.ai_fix.generating") : t("runs.ai_fix.generate")}
                    </button>
                  </div>
                </>
              ) : (
                <>
                  {aiProposal.change_summary?.length > 0 && (
                    <div style={{ marginBottom: 12 }}>
                      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 4 }}>
                        {t("runs.ai_fix.change_summary")}
                      </div>
                      <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, color: "var(--text-2)" }}>
                        {aiProposal.change_summary.map((line, i) => (
                          <li key={i} style={{ marginBottom: 3 }}>{line}</li>
                        ))}
                      </ul>
                    </div>
                  )}
                  <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 4 }}>{t("runs.ai_fix.steps_label")}</div>
                  <pre
                    style={{
                      margin: "0 0 10px",
                      padding: 8,
                      fontSize: 10,
                      fontFamily: "monospace",
                      maxHeight: 140,
                      overflow: "auto",
                      background: "var(--surface-1)",
                      border: "1px solid var(--border)",
                      borderRadius: 4,
                      color: "var(--text-2)",
                    }}
                  >
                    {JSON.stringify(aiProposal.steps, null, 2)}
                  </pre>
                  <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 4 }}>{t("runs.ai_fix.assertions_label")}</div>
                  <pre
                    style={{
                      margin: "0 0 12px",
                      padding: 8,
                      fontSize: 10,
                      fontFamily: "monospace",
                      maxHeight: 120,
                      overflow: "auto",
                      background: "var(--surface-1)",
                      border: "1px solid var(--border)",
                      borderRadius: 4,
                      color: "var(--text-2)",
                    }}
                  >
                    {JSON.stringify(aiProposal.assertions, null, 2)}
                  </pre>
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", justifyContent: "flex-end" }}>
                    <button type="button" className="btn btn-secondary btn-sm" onClick={() => setAiProposal(null)}>
                      {t("runs.ai_fix.discard_proposal")}
                    </button>
                    <button
                      type="button"
                      className="btn btn-primary btn-sm"
                      onClick={() => {
                        setAiApplyError("");
                        onClose();
                        setConfirmAiApplyOpen(true);
                      }}
                    >
                      {t("runs.ai_fix.apply_catalog")}
                    </button>
                  </div>
                </>
              )}
            </div>
          </div>
        </div>
      )}

      <ConfirmModal
        open={confirmAiApplyOpen}
        busy={aiApplyBusy}
        error={aiApplyError}
        zIndex={10001}
        title={t("runs.ai_fix.apply_confirm_title")}
        description={t("runs.ai_fix.apply_confirm_desc")}
        cancelLabel={t("common.cancel")}
        confirmLabel={t("runs.ai_fix.apply_confirm_btn")}
        onCancel={() => {
          setConfirmAiApplyOpen(false);
          setAiApplyError("");
          if (aiProposal && aiCatalogTest) setShowMainAfterConfirmCancel(true);
        }}
        onConfirm={async () => {
          if (!aiCatalogTest || !aiProposal) return;
          setAiApplyBusy(true);
          setAiApplyError("");
          try {
            const runLabel = runPersistedLookupId(detail);
            await updateTest(aiCatalogTest.test_case_id, {
              name: aiCatalogTest.name,
              module: aiCatalogTest.module,
              priority: aiCatalogTest.priority,
              steps: aiProposal.steps,
              assertions: aiProposal.assertions,
              _change_note: `Runs: corrección IA desde run ${runLabel}`,
            });
            setConfirmAiApplyOpen(false);
            onApplied(aiCatalogTest.test_case_id);
            resetAiCatalogFixState();
          } catch (e) {
            setAiApplyError(apiErrorMessage(e) || t("runs.ai_fix.apply_failed"));
            setShowMainAfterConfirmCancel(true);
          } finally {
            setAiApplyBusy(false);
          }
        }}
      />
    </>
  );
}
