/** Coverage page presentation helpers (UX only). */

export function isCoverageInsufficientHistory(summary) {
  if (!summary) return false;
  const totalTests = summary.total_test_cases ?? 0;
  const modules = summary.modules ?? [];
  if (totalTests > 0) return false;
  if (!modules.length) return false;
  return modules.every((m) => (m.coverage_pct ?? 0) === 0);
}
