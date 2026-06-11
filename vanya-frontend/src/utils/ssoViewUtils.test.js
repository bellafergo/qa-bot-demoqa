import { describe, it, expect } from "vitest";
import {
  SSO_I18N_KEYS,
  buildSsoConfigurationViewModel,
  mapSsoProvider,
} from "./ssoViewUtils.js";

const t = (key) => key;

const sampleProviders = {
  providers: [
    {
      provider: "MICROSOFT",
      enabled: true,
      client_id: "ms-client",
      tenant_id: "common",
      configured: true,
      validated: true,
    },
    {
      provider: "GOOGLE",
      enabled: false,
      client_id: "",
      configured: false,
      validated: false,
    },
    {
      provider: "OKTA",
      enabled: false,
      client_id: "",
      issuer: "",
      configured: false,
      validated: false,
    },
  ],
  total: 3,
};

describe("ssoViewUtils", () => {
  it("maps provider cards with validation state", () => {
    const vm = mapSsoProvider(sampleProviders.providers[0], t);
    expect(vm.providerLabel).toBe(SSO_I18N_KEYS.providerMicrosoft);
    expect(vm.configured).toBe(true);
    expect(vm.validated).toBe(true);
    expect(vm.showTenantId).toBe(true);
  });

  it("builds configuration view model with grouping", () => {
    const vm = buildSsoConfigurationViewModel({ providers: sampleProviders, t });
    expect(vm.show).toBe(true);
    expect(vm.providers).toHaveLength(3);
    expect(vm.providers[0].validatedLabel).toBe(SSO_I18N_KEYS.validated);
    expect(vm.providers[1].validatedLabel).toBe(SSO_I18N_KEYS.notValidated);
  });

  it("shows empty state when no providers", () => {
    const vm = buildSsoConfigurationViewModel({ providers: { providers: [] }, t });
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(SSO_I18N_KEYS.empty);
  });

  it("exposes login url labels for actions", () => {
    const vm = mapSsoProvider(sampleProviders.providers[0], t);
    expect(vm.generateLoginUrlLabel).toBe(SSO_I18N_KEYS.generateLoginUrl);
    expect(vm.loginUrlLabel).toBe(SSO_I18N_KEYS.loginUrl);
  });
});
