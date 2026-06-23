import React, { useCallback, useEffect, useMemo, useState } from "react";
import { getSecurityProviders, getSecurityReadiness, apiErrorMessage } from "../../api";
import { useLang } from "../../i18n/LangContext";
import { buildSecurityReadinessViewModel } from "../../utils/securityReadinessViewUtils.js";
import SecurityProviderCard from "./SecurityProviderCard.jsx";
import SecurityReadinessCard from "./SecurityReadinessCard.jsx";
import { LoadingState, ErrorState } from "../../ui/EmptyState.jsx";

export default function SecuritySection() {
  const { t } = useLang();
  const [readiness, setReadiness] = useState(null);
  const [providers, setProviders] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const [readinessData, providersData] = await Promise.all([
        getSecurityReadiness(),
        getSecurityProviders(false),
      ]);
      setReadiness(readinessData);
      setProviders(providersData);
    } catch (err) {
      setError(apiErrorMessage(err));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    load();
  }, [load]);

  const vm = useMemo(
    () => buildSecurityReadinessViewModel({ readiness, providers, t }),
    [readiness, providers, t],
  );

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{vm.title}</div>
      <p style={{ margin: "0 0 14px", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
        {vm.subtitle}
      </p>

      {loading ? (
        <LoadingState message={t("common.loading")} />
      ) : error ? (
        <ErrorState
          title={vm.title}
          description={error}
          onRetry={load}
          retryLabel={t("common.retry")}
        />
      ) : vm.empty ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{vm.emptyMessage}</p>
      ) : (
        <>
          <SecurityReadinessCard vm={vm} />
          <div style={{ marginTop: 16 }}>
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
              {vm.providersTitle}
            </div>
            <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
              {vm.providers.map((provider) => (
                <SecurityProviderCard key={provider.provider_id} provider={provider} />
              ))}
            </ul>
          </div>
        </>
      )}

      <p style={{ margin: "12px 0 0", fontSize: 11, color: "var(--text-3)", fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
