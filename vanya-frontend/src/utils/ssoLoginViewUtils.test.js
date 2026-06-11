import { describe, expect, it } from "vitest";
import {
  SSO_LOGIN_I18N_KEYS,
  buildSsoIdentityViewModel,
  buildSsoLoginViewModel,
  providerButtonLabel,
} from "./ssoLoginViewUtils.js";

const t = (key) => key;

describe("ssoLoginViewUtils", () => {
  it("builds provider buttons for configured providers", () => {
    const vm = buildSsoLoginViewModel({ providers: ["MICROSOFT", "GOOGLE"], t });
    expect(vm.empty).toBe(false);
    expect(vm.providers).toHaveLength(2);
    expect(vm.providers[0].label).toBe(SSO_LOGIN_I18N_KEYS.loginWithMicrosoft);
  });

  it("renders empty state when no providers configured", () => {
    const vm = buildSsoLoginViewModel({ providers: [], t });
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(SSO_LOGIN_I18N_KEYS.empty);
  });

  it("maps provider button labels", () => {
    expect(providerButtonLabel("OKTA", t)).toBe(SSO_LOGIN_I18N_KEYS.loginWithOkta);
  });

  it("builds authenticated identity view model", () => {
    const vm = buildSsoIdentityViewModel({
      identity: {
        provider_type: "MICROSOFT",
        email: "user@example.com",
        display_name: "Test User",
        user_id: "sso:microsoft:abc",
      },
      readiness: { authentication_method: "SSO" },
      t,
    });
    expect(vm.provider).toBe("MICROSOFT");
    expect(vm.email).toBe("user@example.com");
    expect(vm.displayName).toBe("Test User");
    expect(vm.authenticationMethod).toBe(SSO_LOGIN_I18N_KEYS.methodSso);
  });

  it("hides provider buttons when provider list is empty", () => {
    const vm = buildSsoLoginViewModel({ providers: [], t });
    expect(vm.providers).toHaveLength(0);
    expect(vm.show).toBe(true);
  });
});
