import React from "react";
import SSOLoginButton from "./SSOLoginButton.jsx";

export default function SSOLoginView({ vm, onProviderLogin, busyProvider = "" }) {
  if (!vm?.show || vm.empty) {
    return (
      <p style={{ fontSize: 12, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {vm?.emptyMessage}
      </p>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
      {vm.providers.map((provider) => (
        <SSOLoginButton
          key={provider.provider}
          label={provider.label}
          busy={busyProvider === provider.provider}
          disabled={Boolean(busyProvider) && busyProvider !== provider.provider}
          onClick={() => onProviderLogin?.(provider.provider)}
        />
      ))}
    </div>
  );
}
