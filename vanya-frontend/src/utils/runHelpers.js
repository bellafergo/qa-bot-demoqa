export function fmtRunsKpiDate(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short", day: "numeric", hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

export function fmtRunsKpiMs(ms) {
  if (ms == null || ms === 0) return "—";
  return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`;
}
export function runPersistedLookupId(run) {
  if (!run) return "";
  const rid = run.run_id != null ? String(run.run_id).trim() : "";
  return rid || "";
}
export function statusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s === "pass" || s === "passed" || s === "completed") return "badge badge-green";
  if (s === "fail" || s === "failed")                      return "badge badge-red";
  if (s === "error")                                      return "badge badge-orange";
  if (s === "running")                                     return "badge badge-blue";
  if (s === "queued")                                      return "badge badge-orange";
  if (s === "planning" || s === "compiled")                 return "badge badge-orange";
  if (s === "canceled" || s === "cancelled")              return "badge badge-gray";
  return "badge badge-gray";
}

export function statusIcon(status) {
  const s = String(status || "").toLowerCase();
  if (s === "pass" || s === "passed" || s === "completed") return "✓";
  if (s === "fail" || s === "failed") return "✕";
  if (s === "error") return "⚠";
  if (s === "running") return "⏱";
  if (s === "queued") return "⏳";
  if (s === "planning" || s === "compiled") return "⌁";
  if (s === "canceled" || s === "cancelled") return "⦸";
  return "•";
}

export function statusBadgeText(status) {
  const label = status || "—";
  return `${statusIcon(status)} ${label}`;
}

export function stepStatusColor(status) {
  const s = String(status || "").toLowerCase();
  if (s.includes("pass")) return "var(--green)";
  if (s.includes("fail") || s.includes("error")) return "var(--red)";
  return "var(--text-3)";
}

export function fmtDate(iso) {
  if (!iso) return "—";
  try { return new Date(iso).toLocaleString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" }); }
  catch { return "—"; }
}

export function fmtMs(ms) {
  if (ms == null) return "—";
  return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`;
}

export function getStepsCount(run) {
  if (!run) return 0;
  const n =
    run.steps_count ??
    (Array.isArray(run.steps) ? run.steps.length : null) ??
    (Array.isArray(run.steps_result) ? run.steps_result.length : null) ??
    0;
  const nn = Number(n);
  return Number.isFinite(nn) ? nn : 0;
}

export function hasEvidenceForList(run) {
  if (!run) return false;
  const arts = run?.artifacts || {};
  return Boolean(
    run.evidence_url ||
      run.report_url ||
      arts.evidence_url ||
      arts.report_url ||
      arts.screenshot_b64 ||
      run.meta?.evidence_url ||
      run.meta?.report_url
  );
}

export function runStatusIsFailure(status) {
  const s = String(status || "").toLowerCase();
  return s === "fail" || s === "failed" || s === "error";
}

/** Catalog-linked test id from a run detail payload, or null if not applicable. */
export function resolveTestCaseIdFromRun(detail) {
  const id = detail?.test_id || detail?.test_case_id;
  if (id == null) return null;
  const t = String(id).trim();
  if (!t || t === "_async") return null;
  return t;
}

export function findFailingStepInfo(detail) {
  if (!detail) return null;
  const steps = detail.steps_result || detail.steps || [];
  const meta = detail.meta || {};
  const idx = detail.step_index ?? meta.step_index;
  if (idx != null && steps[idx]) {
    const s = steps[idx];
    return {
      index: Number(idx),
      action: s.action || s.type || s.name || "—",
      status: s.status,
    };
  }
  for (let i = 0; i < steps.length; i++) {
    const st = String(steps[i]?.status || "").toLowerCase();
    if (st.includes("fail") || st === "error") {
      const s = steps[i];
      return {
        index: i,
        action: s.action || s.type || s.name || "—",
        status: s.status,
      };
    }
  }
  return null;
}

export function buildAiCatalogInstructionPrefill(detail, failing, t) {
  const tc = resolveTestCaseIdFromRun(detail);
  const name = detail.test_name || "";
  const err =
    detail.error_summary ||
    detail.reason ||
    detail.message ||
    detail.error_message ||
    "";
  const hint = detail.hint || detail.meta?.hint;
  const lines = [];
  if (tc) {
    lines.push(
      t("runs.ai_fix.prefill_intro", { tc, suffix: name ? ` — ${name}` : "" })
    );
  }
  if (err) lines.push(`${t("runs.ai_fix.prefill_error_prefix")}${err}`);
  if (hint) lines.push(`${t("runs.ai_fix.prefill_hint_prefix")}${hint}`);
  if (failing != null) {
    lines.push(
      t("runs.ai_fix.prefill_failing_step", {
        index: failing.index,
        action: failing.action,
        status: failing.status || "—",
      })
    );
  }
  lines.push(t("runs.ai_fix.prefill_footer"));
  return lines.join("\n");
}
export function inferRunType(run) {
  if (!run) return null;
  const runner = (run.meta?.runner || "").toLowerCase();
  if (runner === "desktop")                          return "desktop";
  if (runner === "api")                              return "api";
  const tt = (run.test_type || run.meta?.tc_type || "").toLowerCase();
  if (tt === "desktop")                              return "desktop";
  if (tt === "api")                                  return "api";
  const tcId = (run.test_id || run.test_case_id || "").toUpperCase();
  if (tcId.startsWith("TC-POS-") || tcId.startsWith("POS-")) return "desktop";
  return "ui";
}

export function inferBackend(run) {
  if (!run || run.meta?.is_mock == null) return null;
  return run.meta.is_mock ? "mock" : "real";
}

export function stepFieldDisplay(value) {
  if (value == null || value === "") return null;
  if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
    return String(value);
  }
  if (typeof value === "object") {
    const p = value.primary;
    if (p != null && p !== "") return String(p);
    try {
      return JSON.stringify(value);
    } catch {
      return "—";
    }
  }
  return String(value);
}

export function bestStepTarget(step) {
  if (!step || typeof step !== "object") return "—";
  const keys = ["target", "window", "control", "selector", "url", "value"];
  for (let i = 0; i < keys.length; i++) {
    const rendered = stepFieldDisplay(step[keys[i]]);
    if (rendered != null) return rendered;
  }
  return "—";
}

// ── Evidence helpers ──────────────────────────────────────────────────────────

export function toDataUri(b64) {
  if (!b64) return null;
  return b64.startsWith("data:") ? b64 : `data:image/png;base64,${b64}`;
}

export function getScreenshotSrc(detail) {
  if (!detail) return null;
  // 1. canonical artifacts.screenshot_b64 (new contract)
  const artifacts = detail.artifacts || {};
  if (artifacts.screenshot_b64) return toDataUri(artifacts.screenshot_b64);
  // 2. top-level screenshot_b64 (legacy chat/execute runs)
  if (detail.screenshot_b64) return toDataUri(detail.screenshot_b64);
  // 3. meta.screenshot_b64 / meta.screenshot_url (legacy)
  const meta = detail.meta || {};
  if (meta.screenshot_b64) return toDataUri(meta.screenshot_b64);
  if (meta.screenshot_url) return meta.screenshot_url;
  // 4. last step entry with a screenshot
  const steps = detail.steps_result || detail.steps || [];
  for (let i = steps.length - 1; i >= 0; i--) {
    const s = steps[i];
    if (s?.screenshot_b64) return toDataUri(s.screenshot_b64);
    if (s?.screenshot_url) return s.screenshot_url;
  }
  return null;
}

/** Parse backend confidence: 0–1 ratio, or 0–100 percent, or numeric string. */
export function parseConfidenceNumber(raw) {
  if (raw == null || raw === "") return null;
  const n = typeof raw === "number" ? raw : parseFloat(String(raw).replace(/%/g, "").trim());
  if (!Number.isFinite(n)) return null;
  if (n >= 0 && n <= 1) return n;
  if (n > 1 && n <= 100) return n / 100;
  return null;
}

/** Display + tier for styling (high / medium / low / none). */
export function formatHealingConfidence(conf) {
  if (conf == null || !Number.isFinite(conf)) {
    return { label: "—", detail: null, tier: "none" };
  }
  const pct = Math.round(conf * 100);
  const label = `${pct}%`;
  const detail = conf.toFixed(2);
  let tier = "low";
  if (conf >= 0.85) tier = "high";
  else if (conf >= 0.5) tier = "medium";
  return { label, detail, tier };
}

export function healingConfidenceColor(tier) {
  if (tier === "high") return "var(--green)";
  if (tier === "medium") return "var(--orange-text)";
  if (tier === "low") return "var(--text-3)";
  return "var(--text-3)";
}

/**
 * Normalize healing for the Runs detail UI.
 * Prefers resolution_log (fallback_used); merges healing_log for missing fields or healing-only rows.
 * stepIndex is 0-based (runner step_index).
 */
export function collectHealingRows(run) {
  const res = Array.isArray(run?.resolution_log) ? run.resolution_log : [];
  const heals = Array.isArray(run?.healing_log) ? run.healing_log : [];
  const healByStep = new Map();
  for (const h of heals) {
    if (h?.selector_healed && typeof h.step_index === "number") {
      healByStep.set(h.step_index, h);
    }
  }

  const raw = [];
  for (const e of res) {
    if (!e?.fallback_used) continue;
    const si = typeof e.step_index === "number" ? e.step_index : null;
    raw.push({ stepIndex: si, e, h: si != null ? healByStep.get(si) : undefined });
  }
  for (const h of heals) {
    if (!h?.selector_healed || typeof h.step_index !== "number") continue;
    if (raw.some((r) => r.stepIndex === h.step_index)) continue;
    raw.push({ stepIndex: h.step_index, e: null, h });
  }

  const byStep = new Map();
  const orphans = [];
  for (const row of raw) {
    if (row.stepIndex == null) {
      orphans.push(row);
    } else {
      byStep.set(row.stepIndex, row);
    }
  }

  const rows = [...byStep.values(), ...orphans].map(({ stepIndex, e, h }) => ({
    stepIndex,
    action: e?.action ?? h?.action ?? "—",
    originalSelector: e?.original_selector ?? e?.primary ?? h?.original_selector ?? h?.selector ?? "—",
    healedSelector: e?.resolved ?? h?.healed_selector ?? "—",
    strategy: e?.fallback_type ?? e?.used ?? h?.healing_strategy ?? h?.strategy ?? "—",
    confidenceRaw: e?.confidence ?? h?.confidence ?? null,
    healingReason: e?.healing_reason ?? h?.healing_reason ?? null,
  }));

  rows.sort((a, b) => {
    if (a.stepIndex == null && b.stepIndex == null) return 0;
    if (a.stepIndex == null) return 1;
    if (b.stepIndex == null) return -1;
    return a.stepIndex - b.stepIndex;
  });

  return rows.map((r) => ({
    ...r,
    confidence: parseConfidenceNumber(r.confidenceRaw),
  }));
}
