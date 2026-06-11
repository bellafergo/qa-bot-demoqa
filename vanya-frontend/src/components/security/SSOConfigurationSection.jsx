import React, { useCallback, useEffect, useMemo, useState } from "react";
import {
  getSsoProviders,
  validateSsoProvider,
  getSsoLoginUrl,
  apiErrorMessage,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import { buildSsoConfigurationViewModel } from "../../utils/ssoViewUtils.js";
import SSOProviderCard from "./SSOProviderCard.jsx";
import PermissionGate from "./PermissionGate.jsx";

export default function SSOConfigurationSection() {
  const { t } = useLang();
  const [providers, setProviders] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");
  const [validatingProvider, setValidatingProvider] = useState("");
  const [generatingProvider, setGeneratingProvider] = useState("");
  const [validationMessages, setValidationMessages] = useState({});
  const [loginUrls, setLoginUrls] = useState({});

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
    () => buildSsoConfigurationViewModel({ providers, t }),
    [providers, t],
  );

  const handleValidate = async (payload) => {
    setValidatingProvider(payload.provider);
    setError("");
    try {
      const result = await validateSsoProvider(payload);
      const message = result.valid
        ? `${vm.validationSuccessLabel}: ${result.message}`
        : `${vm.validationFailedLabel}: ${result.message}`;
      setValidationMessages((prev) => ({ ...prev, [payload.provider]: message }));
      await load();
    } catch (err) {
      setValidationMessages((prev) => ({
        ...prev,
        [payload.provider]: `${vm.validationFailedLabel}: ${apiErrorMessage(err)}`,
      }));
    } finally {
      setValidatingProvider("");
    }
  };

  const handleGenerateLoginUrl = async (provider) => {
    setGeneratingProvider(provider);
    setError("");
    try {
      const result = await getSsoLoginUrl(provider);
      setLoginUrls((prev) => ({ ...prev, [provider]: result.login_url || "" }));
    } catch (err) {
      setError(apiErrorMessage(err));
    } finally {
      setGeneratingProvider("");
    }
  };

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{vm.title}</div>
      <p style={{ margin: "0 0 14px", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
        {vm.subtitle}
      </p>

      <PermissionGate permission="MANAGE_SECURITY" showDenied>
      {loading ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>…</div>
      ) : error ? (
        <div className="alert alert-error" style={{ fontSize: 12 }}>{error}</div>
      ) : vm.empty ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{vm.emptyMessage}</p>
      ) : (
        <ul style={{ margin: 0, padding: 0 }}>
          {vm.providers.map((provider) => (
            <SSOProviderCard
              key={provider.provider}
              provider={provider}
              validating={validatingProvider === provider.provider}
              generating={generatingProvider === provider.provider}
              validationMessage={validationMessages[provider.provider] || ""}
              loginUrl={loginUrls[provider.provider] || ""}
              onValidate={handleValidate}
              onGenerateLoginUrl={handleGenerateLoginUrl}
            />
          ))}
        </ul>
      )}

      <p style={{ margin: "12px 0 0", fontSize: 11, color: "var(--text-3)", fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
      </PermissionGate>
    </div>
  );
}
