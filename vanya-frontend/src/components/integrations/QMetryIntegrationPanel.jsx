import React, { useCallback, useEffect, useState } from "react";
import {
  getQMetryStatus,
  listQMetryProjects,
  listQMetryTestCases,
  listQMetryTestCycles,
  listQMetryTestSuites,
  listQMetryTestRuns,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import QMetryConnectionCard from "./QMetryConnectionCard.jsx";
import QMetryProjectCard from "./QMetryProjectCard.jsx";
import QMetryTestCaseCard from "./QMetryTestCaseCard.jsx";
import { buildQMetryIntegrationViewModel } from "../../utils/qmetryViewUtils.js";

function QMetryDiscoveryListSection({ section }) {
  if (!section) return null;
  return (
    <div style={{ marginBottom: 12 }}>
      <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
        {section.label}
      </div>
      {section.showEmpty ? (
        <p style={{ fontSize: 12, color: "var(--text-2)", margin: 0 }}>{section.emptyText}</p>
      ) : (
        <div style={{ display: "grid", gap: 6 }}>
          {section.items.map((item) => (
            <div
              key={item.key}
              style={{
                padding: "8px 10px",
                borderRadius: 6,
                border: "1px solid var(--border)",
                background: "var(--bg-2)",
                fontSize: 12,
                display: "flex",
                justifyContent: "space-between",
                gap: 8,
                flexWrap: "wrap",
              }}
            >
              <span style={{ fontFamily: "monospace", fontWeight: 600 }}>{item.key}</span>
              <span style={{ color: "var(--text-2)" }}>{item.label}</span>
              {item.meta ? <span style={{ color: "var(--text-3)", fontSize: 11 }}>{item.meta}</span> : null}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default function QMetryIntegrationPanel({ refreshToken = 0 }) {
  const { t } = useLang();
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [status, setStatus] = useState(null);
  const [projects, setProjects] = useState([]);
  const [testCases, setTestCases] = useState([]);
  const [testCycles, setTestCycles] = useState([]);
  const [testSuites, setTestSuites] = useState([]);
  const [testRuns, setTestRuns] = useState([]);

  const load = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const [st, pr, tc, cy, su, ru] = await Promise.all([
        getQMetryStatus(),
        listQMetryProjects(),
        listQMetryTestCases(),
        listQMetryTestCycles(),
        listQMetryTestSuites(),
        listQMetryTestRuns(),
      ]);
      setStatus(st);
      setProjects(Array.isArray(pr?.projects) ? pr.projects : []);
      setTestCases(Array.isArray(tc?.test_cases) ? tc.test_cases : []);
      setTestCycles(Array.isArray(cy?.test_cycles) ? cy.test_cycles : []);
      setTestSuites(Array.isArray(su?.test_suites) ? su.test_suites : []);
      setTestRuns(Array.isArray(ru?.test_runs) ? ru.test_runs : []);
    } catch (e) {
      setError(e.message || t("integrations.qmetry.load_error"));
      setStatus(null);
      setProjects([]);
      setTestCases([]);
      setTestCycles([]);
      setTestSuites([]);
      setTestRuns([]);
    } finally {
      setLoading(false);
    }
  }, [t]);

  useEffect(() => {
    load();
  }, [load, refreshToken]);

  const vm = buildQMetryIntegrationViewModel({
    status,
    projects,
    testCases,
    testCycles,
    testSuites,
    testRuns,
    t,
  });

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 10 }}>
        <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)" }}>
          {t("integrations.qmetry.discovery_title")}
        </div>
        <button type="button" className="btn btn-sm" onClick={load} disabled={loading}>
          {loading ? "…" : vm.refreshLabel}
        </button>
      </div>

      {error ? (
        <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>{error}</div>
      ) : null}

      {loading && !status ? (
        <div style={{ fontSize: 12, color: "var(--text-2)", padding: "8px 0" }}>{vm.loadingLabel}</div>
      ) : (
        <QMetryConnectionCard vm={vm.connection} />
      )}

      {!loading && vm.connection.connected ? (
        <>
          <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
            {vm.projectsLabel}
          </div>
          {vm.showEmptyProjects ? (
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 12px" }}>{vm.emptyProjectsText}</p>
          ) : (
            <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
              {vm.projects.map((p) => (
                <QMetryProjectCard key={p.projectId} vm={p} />
              ))}
            </div>
          )}

          <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", margin: "12px 0 8px" }}>
            {vm.testCasesLabel}
          </div>
          {vm.showEmptyTestCases ? (
            <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 12px" }}>{vm.emptyTestCasesText}</p>
          ) : (
            <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
              {vm.testCases.map((tc) => (
                <QMetryTestCaseCard key={tc.testCaseId} vm={tc} />
              ))}
            </div>
          )}

          <QMetryDiscoveryListSection section={vm.testCycles} />
          <QMetryDiscoveryListSection section={vm.testSuites} />
          <QMetryDiscoveryListSection section={vm.testRuns} />
        </>
      ) : null}
    </div>
  );
}
