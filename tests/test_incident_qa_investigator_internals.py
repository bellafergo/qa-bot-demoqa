# tests/test_incident_qa_investigator_internals.py
"""Direct unit tests for incident_qa_investigator_service internal helpers (no HTTP)."""
from __future__ import annotations

from types import SimpleNamespace

import pytest

from models.incident_models import (
    ConfidenceFactor,
    IncidentHypothesis,
    ProjectInvestigateIncidentRequest,
    RelatedPRAnalysisSummary,
    RelatedPRSummary,
    RelatedRunSummary,
)
from services.incident_qa_investigator_service import (
    _align_global_confidence,
    _build_actions_available,
    _build_hypotheses,
    _compute_confidence_v2,
    _impacted_modules,
    _match_runs_by_hints,
    _rank_hypotheses,
    _recommended_tests,
)


def _run(
    run_id: str,
    *,
    test_id: str = "TC-1",
    test_name: str = "Login test",
    module: str = "auth",
    error_summary: str | None = None,
    rca_summary: str | None = None,
) -> RelatedRunSummary:
    return RelatedRunSummary(
        run_id=run_id,
        test_id=test_id,
        test_name=test_name,
        module=module,
        status="failed",
        error_summary=error_summary,
        rca_summary=rca_summary,
    )


def _empty_confidence_inputs(**overrides):
    base = dict(
        related_runs=[],
        related_evidence=[],
        related_pr_analysis=[],
        browser_events=[],
        hypotheses=[],
        clusters=[],
        knowledge_ctx=None,
        data_gaps=[],
        impacted_modules=[],
        module_overlap_with_pr=False,
    )
    base.update(overrides)
    return base


class TestComputeConfidenceV2:
    def test_baseline_no_evidence_is_low_and_bounded(self):
        score, factors = _compute_confidence_v2(**_empty_confidence_inputs())
        assert 0.05 <= score <= 0.95
        labels = {f.label for f in factors}
        assert "no_failed_runs" in labels
        assert "no_evidence" in labels

    def test_failed_runs_increase_confidence_over_baseline(self):
        baseline, _ = _compute_confidence_v2(**_empty_confidence_inputs())
        with_runs, factors = _compute_confidence_v2(
            **_empty_confidence_inputs(related_runs=[_run("r1", error_summary="timeout")]),
        )
        assert with_runs > baseline
        assert any(f.label == "failed_runs" and f.delta > 0 for f in factors)

    def test_major_signals_produce_high_confidence(self):
        score, factors = _compute_confidence_v2(
            related_runs=[_run("r1"), _run("r2", module="checkout")],
            related_evidence=[SimpleNamespace(), SimpleNamespace()],
            related_pr_analysis=[RelatedPRAnalysisSummary(pr_number="42", reason="same module")],
            browser_events=[{"id": "bw1"}, {"id": "bw2"}],
            hypotheses=[
                IncidentHypothesis(statement="evidence-backed", confidence=0.8, basis="evidence"),
            ],
            clusters=[{"module": "auth", "total_failures": 3}],
            knowledge_ctx=SimpleNamespace(hints=["recent failure in auth"]),
            data_gaps=[],
            impacted_modules=["auth", "checkout"],
            module_overlap_with_pr=True,
        )
        assert score >= 0.5
        assert len(factors) >= 5

    def test_confidence_caps_at_upper_bound(self):
        score, _ = _compute_confidence_v2(
            related_runs=[_run(f"r{i}") for i in range(10)],
            related_evidence=[SimpleNamespace() for _ in range(10)],
            related_pr_analysis=[
                RelatedPRAnalysisSummary(pr_number=str(i), reason="same module")
                for i in range(5)
            ],
            browser_events=[{"id": f"e{i}"} for i in range(10)],
            hypotheses=[
                IncidentHypothesis(statement=f"h{i}", confidence=0.9, basis="evidence")
                for i in range(5)
            ],
            clusters=[{"module": f"m{i}", "total_failures": 10} for i in range(5)],
            knowledge_ctx=SimpleNamespace(hints=["h1", "h2", "h3"]),
            data_gaps=[],
            impacted_modules=["auth", "checkout", "payments"],
            module_overlap_with_pr=True,
        )
        assert score <= 0.95

    def test_confidence_floors_at_lower_bound(self):
        score, _ = _compute_confidence_v2(
            **_empty_confidence_inputs(
                data_gaps=[f"gap-{i}" for i in range(20)],
            ),
        )
        assert score >= 0.05

    def test_module_overlap_adds_factor(self):
        _, factors = _compute_confidence_v2(
            **_empty_confidence_inputs(
                related_runs=[_run("r1")],
                related_evidence=[SimpleNamespace()],
                impacted_modules=["auth"],
                module_overlap_with_pr=True,
            ),
        )
        overlap = [f for f in factors if f.label == "module_overlap"]
        assert len(overlap) == 1
        assert overlap[0].delta == pytest.approx(0.10)

    def test_data_gap_penalty_is_capped(self):
        _, factors = _compute_confidence_v2(
            **_empty_confidence_inputs(data_gaps=[f"gap-{i}" for i in range(10)]),
        )
        gap = [f for f in factors if f.label == "data_gaps"]
        assert len(gap) == 1
        assert abs(gap[0].delta) <= 0.15


class TestBuildHypotheses:
    def test_no_data_returns_fallback_hypothesis(self):
        hyps = _build_hypotheses(
            description="unknown issue",
            hints=set(),
            related_runs=[],
            clusters=[],
            regressions=[],
            knowledge_ctx=None,
            related_prs=[],
            related_pr_analysis=[],
        )
        assert len(hyps) == 1
        assert hyps[0].basis == "assumption"
        assert hyps[0].confidence == pytest.approx(0.2)

    def test_run_evidence_generates_evidence_hypothesis(self):
        hyps = _build_hypotheses(
            description="login fails",
            hints={"login"},
            related_runs=[_run("r1", error_summary="401 Unauthorized")],
            clusters=[],
            regressions=[],
            knowledge_ctx=None,
            related_prs=[],
            related_pr_analysis=[],
        )
        assert any(h.basis == "evidence" and "run:r1" in h.supporting_refs for h in hyps)

    def test_pr_analysis_generates_pr_based_hypothesis(self):
        pra = RelatedPRAnalysisSummary(
            pr_number="99",
            provider="github",
            pr_risk_score=80.0,
            risk_level="HIGH",
            reason="same module overlap with auth",
        )
        hyps = _build_hypotheses(
            description="auth regression",
            hints={"auth"},
            related_runs=[],
            clusters=[],
            regressions=[],
            knowledge_ctx=None,
            related_prs=[],
            related_pr_analysis=[pra],
        )
        assert any("PR Analysis" in h.statement and h.basis == "evidence" for h in hyps)

    def test_multiple_runs_same_module_single_run_hypothesis(self):
        hyps = _build_hypotheses(
            description="auth issue",
            hints={"auth"},
            related_runs=[
                _run("r1", module="auth", error_summary="err1"),
                _run("r2", module="auth", error_summary="err2"),
            ],
            clusters=[],
            regressions=[],
            knowledge_ctx=None,
            related_prs=[],
            related_pr_analysis=[],
        )
        run_hyps = [h for h in hyps if any(ref.startswith("run:") for ref in h.supporting_refs)]
        assert len(run_hyps) == 1


class TestRankHypotheses:
    def test_orders_by_confidence_descending(self):
        hyps = [
            IncidentHypothesis(statement="low", confidence=0.3, basis="inference"),
            IncidentHypothesis(statement="high", confidence=0.9, basis="evidence"),
            IncidentHypothesis(statement="mid", confidence=0.6, basis="evidence"),
        ]
        ranked = _rank_hypotheses(hyps)
        confidences = [h.confidence for h in ranked]
        assert confidences == sorted(confidences, reverse=True)
        assert ranked[0].id == "H1"
        assert ranked[0].rank == 1

    def test_empty_list_returns_empty_list(self):
        assert _rank_hypotheses([]) == []


class TestImpactedModules:
    def test_modules_from_runs(self):
        mods = _impacted_modules(
            runs=[_run("r1", module="auth"), _run("r2", module="checkout")],
            clusters=[],
            knowledge_ctx=None,
            hints=set(),
        )
        assert "auth" in mods
        assert "checkout" in mods

    def test_deduplicates_modules_case_insensitively(self):
        mods = _impacted_modules(
            runs=[_run("r1", module="Auth"), _run("r2", module="auth")],
            clusters=[{"module": "AUTH"}],
            knowledge_ctx=None,
            hints=set(),
        )
        assert len(mods) == 1

    def test_pr_analysis_modules_included(self):
        mods = _impacted_modules(
            runs=[],
            clusters=[],
            knowledge_ctx=None,
            hints=set(),
            pr_analysis_mods=["payments", "payments"],
        )
        assert mods == ["payments"]


class TestRecommendedTests:
    def test_prioritizes_pr_analysis_tests_then_runs(self):
        rec = _recommended_tests(
            runs=[_run("r1", test_id="TC-RUN")],
            regressions=[],
            knowledge_ctx=None,
            pr_analysis_tests=["TC-PR"],
        )
        assert rec[0] == "TC-PR"
        assert "TC-RUN" in rec

    def test_deduplicates_test_ids(self):
        rec = _recommended_tests(
            runs=[_run("r1", test_id="TC-1"), _run("r2", test_id="TC-1")],
            regressions=[{"test_case_id": "TC-1"}],
            knowledge_ctx=None,
        )
        assert rec.count("TC-1") == 1


class TestMatchRunsByHints:
    def _many_runs(self, n: int = 15) -> list[RelatedRunSummary]:
        return [_run(f"r{i}", test_name=f"test-{i}", module=f"mod-{i}") for i in range(n)]

    def test_no_hints_returns_bounded_default(self):
        runs = self._many_runs()
        matched = _match_runs_by_hints(runs, set())
        assert len(matched) == 10

    def test_matching_hint_filters_by_name_module_or_error(self):
        runs = [
            _run("r1", test_name="checkout flow", module="shop"),
            _run("r2", test_name="other", module="auth", error_summary="login timeout"),
            _run("r3", test_name="unrelated", module="reports"),
        ]
        matched = _match_runs_by_hints(runs, {"login"})
        assert len(matched) >= 1
        assert all(
            "login" in f"{r.test_name} {r.module} {r.error_summary or ''}".lower()
            for r in matched
        )

    def test_fallback_when_no_match(self):
        runs = self._many_runs(8)
        matched = _match_runs_by_hints(runs, {"zzznomatch"})
        assert len(matched) == 5
        assert matched == runs[:5]


class TestBuildActionsAvailable:
    def test_generates_actions_from_basic_data(self):
        req = ProjectInvestigateIncidentRequest(
            description="login broken",
            target_url="https://app.example.com/login",
            include_browser_probe=False,
        )
        actions = _build_actions_available(
            req=req,
            related_runs=[_run("r1")],
            related_prs=[
                RelatedPRSummary(provider="github", pr_id="7", title="Fix auth"),
            ],
            related_pr_analysis=[],
            recommended_tests=["TC-1"],
            browser_run=None,
        )
        action_names = {a.action for a in actions}
        assert "run_browser_probe" in action_names
        assert "analyze_related_pr" in action_names
        assert "generate_rca" in action_names
        assert "run_recommended_tests" in action_names

    def test_actions_require_user_approval(self):
        req = ProjectInvestigateIncidentRequest(description="issue", target_url="https://x.com")
        actions = _build_actions_available(
            req=req,
            related_runs=[_run("r1")],
            related_prs=[],
            related_pr_analysis=[],
            recommended_tests=[],
            browser_run=None,
        )
        assert actions
        assert all(a.requires_user_approval for a in actions)


class TestAlignGlobalConfidence:
    def test_aligns_to_primary_hypothesis(self):
        hyps = _rank_hypotheses([
            IncidentHypothesis(statement="top", confidence=0.82, basis="evidence", supporting_refs=["run:1"]),
            IncidentHypothesis(statement="second", confidence=0.4, basis="inference"),
        ])
        aligned, factors = _align_global_confidence(hyps, breakdown_score=0.7, factors=[])
        assert 0.05 <= aligned <= 0.95
        assert aligned >= 0.7 * 0.85 - 0.01
        assert any(f.label == "primary_hypothesis" for f in factors)

    def test_no_hypotheses_caps_low(self):
        aligned, factors = _align_global_confidence([], breakdown_score=0.8, factors=[])
        assert aligned <= 0.25
        assert any("No hypotheses" in f.reason for f in factors)

    def test_missing_factors_does_not_raise(self):
        hyps = _rank_hypotheses([
            IncidentHypothesis(statement="only", confidence=0.5, basis="inference"),
        ])
        aligned, factors = _align_global_confidence(hyps, breakdown_score=0.6, factors=[])
        assert isinstance(aligned, float)
        assert isinstance(factors, list)
        assert all(isinstance(f, ConfidenceFactor) for f in factors)
