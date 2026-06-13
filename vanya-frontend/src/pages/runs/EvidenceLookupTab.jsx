import React, { useState, useCallback, useRef } from "react";
import { apiGet, apiErrorMessage, ApiHttpError } from "../../api";
import { useLang } from "../../i18n/LangContext";
import EvidenceLookupResultView from "../../components/runs/EvidenceLookupResultView.jsx";

export default function EvidenceLookupTab() {
  const { t } = useLang();
  const [evidenceId, setEvidenceId] = useState("");
  const [loading, setLoading]       = useState(false);
  const [run, setRun]               = useState(null);
  const [error, setError]           = useState("");
  const fetchInFlightRef = useRef(false);

  const handleFetch = useCallback(async () => {
    const id = evidenceId.trim();
    if (!id || fetchInFlightRef.current) return;
    fetchInFlightRef.current = true;
    setLoading(true);
    setError("");
    setRun(null);
    try {
      const data = await apiGet(`/runs/${encodeURIComponent(id)}?format=json`);
      setRun(data);
    } catch (e) {
      if (e instanceof ApiHttpError && e.status === 404) {
        setError(`Run not found: ${id}`);
      } else {
        setError(apiErrorMessage(e) || "Network error");
      }
    } finally {
      setLoading(false);
      fetchInFlightRef.current = false;
    }
  }, [evidenceId]);

  return (
    <>
      <div className="card" style={{ marginBottom: 24 }}>
        <div className="page-header" style={{ marginBottom: 16 }}>
          <h1 className="page-title">{t("runs.lookup.title")}</h1>
          <p className="page-subtitle" style={{ whiteSpace: "pre-wrap" }}>{t("runs.lookup.subtitle")}</p>
        </div>
        <div style={{ display: "flex", gap: 10 }}>
          <input
            className="input"
            value={evidenceId}
            onChange={e => setEvidenceId(e.target.value)}
            onKeyDown={e => e.key === "Enter" && handleFetch()}
            placeholder={t("runs.lookup.placeholder")}
            style={{ flex: 1 }}
          />
          <button className="btn btn-primary" onClick={handleFetch} disabled={loading || !evidenceId.trim()}>
            {loading ? t("runs.lookup.loading") : t("runs.lookup.fetch")}
          </button>
        </div>
        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {run && <EvidenceLookupResultView run={run} />}
    </>
  );
}
