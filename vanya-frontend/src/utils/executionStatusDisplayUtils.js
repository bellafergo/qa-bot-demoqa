/** Locale-aware execution / job status labels for customer-facing tables. */

export const EXECUTION_STATUS_I18N_KEYS = {
  passed: "status.execution.passed",
  failed: "status.execution.failed",
  running: "status.execution.running",
  queued: "status.execution.queued",
  completed: "status.execution.completed",
  pending: "status.execution.pending",
  stopped: "status.execution.stopped",
  ready: "status.execution.ready",
  warning: "status.execution.warning",
  success: "status.execution.success",
  partial: "status.execution.partial",
  error: "status.execution.failed",
  pass: "status.execution.passed",
  fail: "status.execution.failed",
  done: "status.execution.completed",
  unknown: "status.execution.unknown",
};

export function formatExecutionStatus(status, t) {
  const key = String(status || "").trim().toLowerCase();
  const i18nKey = EXECUTION_STATUS_I18N_KEYS[key] || EXECUTION_STATUS_I18N_KEYS.unknown;
  return t(i18nKey);
}
