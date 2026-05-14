// src/pages/RunEvidenceDetailPage.jsx
/**
 * In-SPA evidence for GET /runs/:runId?format=json (authenticated fetch). Path param is canonical run_id.
 * Avoids opening API /runs URLs in a new tab (no Bearer on navigation).
 */
import React, { useEffect, useState } from "react";
import { useNavigate, useParams } from "react-router-dom";
import { apiGet, apiErrorMessage, ApiHttpError } from "../api";
import { EvidenceLookupResultView } from "./RunsPage.jsx";
import { useLang } from "../i18n/LangContext";

export default function RunEvidenceDetailPage() {
  const { runId } = useParams();
  const navigate = useNavigate();
  const { t } = useLang();
  const [run, setRun] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  useEffect(() => {
    const id = String(runId || "").trim();
    if (!id) {
      setLoading(false);
      setError(t("ev.spa.missing_id"));
      setRun(null);
      return;
    }
    let cancelled = false;
    (async () => {
      setLoading(true);
      setError("");
      setRun(null);
      try {
        const data = await apiGet(`/runs/${encodeURIComponent(id)}?format=json`);
        if (!cancelled) setRun(data);
      } catch (e) {
        if (cancelled) return;
        if (e instanceof ApiHttpError && e.status === 404) {
          setError(t("ev.spa.not_found", { id }));
        } else {
          setError(apiErrorMessage(e) || t("runs.history.error"));
        }
      } finally {
        if (!cancelled) setLoading(false);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [runId, t]);

  const goBack = () => {
    if (typeof window !== "undefined" && window.history.length > 1) {
      navigate(-1);
    } else {
      navigate("/evidence");
    }
  };

  return (
    <div className="page-wrap">
      <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 20, flexWrap: "wrap" }}>
        <button type="button" className="btn btn-secondary btn-sm" onClick={goBack}>
          ← {t("ev.spa.back")}
        </button>
        <button
          type="button"
          className="btn btn-secondary btn-sm"
          onClick={() => navigate("/runs", { state: { tab: 1 } })}
        >
          {t("runs.lookup.title")}
        </button>
        <button type="button" className="btn btn-secondary btn-sm" onClick={() => navigate("/evidence")}>
          {t("ev.title")}
        </button>
      </div>

      <div className="page-header" style={{ marginBottom: 16 }}>
        <h1 className="page-title">{t("ev.spa.page_title")}</h1>
        {runId ? (
          <p className="page-subtitle" style={{ fontFamily: "ui-monospace, monospace", marginTop: 6 }}>
            {runId}
          </p>
        ) : null}
      </div>

      {loading && (
        <div className="card" style={{ padding: 24, color: "var(--text-3)" }}>
          {t("runs.lookup.loading")}
        </div>
      )}
      {error && !loading && <div className="alert alert-error">{error}</div>}
      {!loading && !error && run && <EvidenceLookupResultView run={run} />}
    </div>
  );
}
