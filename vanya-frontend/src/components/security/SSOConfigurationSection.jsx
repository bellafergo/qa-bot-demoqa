import React, { useCallback, useEffect, useMemo, useState } from "react";
import { getSsoProviders, apiErrorMessage } from "../../api";
import { useLang } from "../../i18n/LangContext";
import { buildSsoRoadmapViewModel } from "../../utils/ssoRoadmapViewUtils.js";
import { LoadingState, ErrorState } from "../../ui/EmptyState.jsx";
import { EnterpriseAvailableTodayCard } from "./SSORoadmapProviderRow.jsx";

export default function SSOConfigurationSection() {
  const { t } = useLang();
  const [providers, setProviders] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const data = await getSsoProviders();
      setProviders(data);
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
    () => buildSsoRoadmapViewModel({ providers, t }),
    [providers, t],
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
      ) : (
        <>
          <div style={{ marginBottom: 16 }}>
            <EnterpriseAvailableTodayCard
              title={vm.availableTodayTitle}
              description={vm.availableTodayDesc}
              badgeLabel={vm.availableTodayBadge}
            />
          </div>
        </>
      )}

      <p style={{ margin: "12px 0 0", fontSize: 11, color: "var(--text-3)", fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
