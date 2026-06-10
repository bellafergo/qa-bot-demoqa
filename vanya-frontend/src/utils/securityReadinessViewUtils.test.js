import { describe, expect, it } from "vitest";
import {
  SECURITY_READINESS_I18N_KEYS,
  authenticationMethodLabelKey,
  buildSecurityReadinessViewModel,
  readinessBadgeClass,
  securityScoreBadgeClass,
} from "./securityReadinessViewUtils.js";

const t = (key) => key;

describe("securityReadinessViewUtils", () => {
  const readiness = {
    authentication_method: "LOCAL",
    sso_ready: false,
    audit_ready: false,
    rbac_ready: false,
    security_score: 25,
    summary: "Authentication is LOCAL via LOCAL.",
  };

  const providers = {
    providers: [
      { provider_id: "local", provider_name: "Local Authentication", provider_type: "LOCAL", enabled: true },
      { provider_id: "google", provider_name: "Google Workspace", provider_type: "GOOGLE", enabled: false },
    ],
    total: 2,
  };

  it("maps authentication method labels", () => {
    expect(authenticationMethodLabelKey("LOCAL")).toBe(SECURITY_READINESS_I18N_KEYS.methodLocal);
    expect(authenticationMethodLabelKey("SSO")).toBe(SECURITY_READINESS_I18N_KEYS.methodSso);
  });

  it("maps readiness and score badges", () => {
    expect(readinessBadgeClass(false)).toBe("badge badge-orange");
    expect(securityScoreBadgeClass(25)).toBe("badge badge-orange");
    expect(securityScoreBadgeClass(100)).toBe("badge badge-green");
  });

  it("builds readiness view model", () => {
    const vm = buildSecurityReadinessViewModel({ readiness, providers, t });
    expect(vm.empty).toBe(false);
    expect(vm.authenticationMethod).toBe(SECURITY_READINESS_I18N_KEYS.methodLocal);
    expect(vm.ssoReadinessText).toBe(SECURITY_READINESS_I18N_KEYS.notConfigured);
    expect(vm.securityScore).toBe(25);
    expect(vm.providers).toHaveLength(2);
  });

  it("handles empty providers", () => {
    const vm = buildSecurityReadinessViewModel({ readiness, providers: { providers: [] }, t });
    expect(vm.empty).toBe(true);
  });
});
