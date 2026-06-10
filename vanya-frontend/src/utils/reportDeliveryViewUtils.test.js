import { describe, expect, it } from "vitest";
import {
  REPORT_DELIVERY_I18N_KEYS,
  anyDeliveryChannelReady,
  buildReportDeliveryViewModel,
  canSendDelivery,
  channelLabelKey,
  isChannelReady,
  reportTypeLabelKey,
} from "./reportDeliveryViewUtils.js";

const t = (key) => key;

describe("reportDeliveryViewUtils", () => {
  const readiness = {
    connectors: {
      slack: { ready: true },
      email: { ready: false },
      teams: { ready: false },
    },
  };

  it("maps report type and channel labels", () => {
    expect(reportTypeLabelKey("RELEASE_READINESS")).toBe(
      REPORT_DELIVERY_I18N_KEYS.releaseReadiness,
    );
    expect(channelLabelKey("email")).toBe(REPORT_DELIVERY_I18N_KEYS.email);
  });

  it("detects channel readiness", () => {
    expect(isChannelReady(readiness, "slack")).toBe(true);
    expect(isChannelReady(readiness, "email")).toBe(false);
    expect(anyDeliveryChannelReady(readiness)).toBe(true);
    expect(anyDeliveryChannelReady({ connectors: {} })).toBe(false);
  });

  it("builds view model with empty state when no channels ready", () => {
    const vm = buildReportDeliveryViewModel({ readiness: { connectors: {} }, t });
    expect(vm.empty).toBe(true);
    expect(vm.channels).toHaveLength(3);
    expect(vm.reportTypes).toHaveLength(4);
  });

  it("validates send button state", () => {
    expect(canSendDelivery({ channel: "slack", readiness, sending: false })).toBe(true);
    expect(canSendDelivery({ channel: "email", readiness, recipient: "", sending: false })).toBe(false);
    expect(canSendDelivery({ channel: "email", readiness, recipient: "cto@example.com", sending: false })).toBe(false);
    expect(canSendDelivery({ channel: "slack", readiness, sending: true })).toBe(false);
  });
});
