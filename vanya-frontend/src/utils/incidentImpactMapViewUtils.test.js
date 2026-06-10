import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  IMPACT_MAP_I18N_KEYS,
  buildImpactMapDrilldownItem,
  buildImpactMapViewModel,
  formatImpactConfidence,
  getImpactSeverityLabelKey,
  hasImpactMapSection,
  isImpactMapEmpty,
} from "./incidentImpactMapViewUtils.js";

const t = (key) => key;

describe("incidentImpactMapViewUtils", () => {
  it("detects impact map section presence and empty state", () => {
    expect(hasImpactMapSection({ impact_map: [] })).toBe(true);
    expect(hasImpactMapSection({})).toBe(false);
    expect(isImpactMapEmpty({ impact_map: [] })).toBe(true);
  });

  it("renders impact map view model with translation keys", () => {
    const vm = buildImpactMapViewModel(
      {
        impact_map: [
          {
            title: "Checkout",
            description: "Multiple signals point to Checkout as an impacted area.",
            severity: "high",
            confidence: 0.84,
            related_entity_count: 12,
            related_entity_type: "failure_cluster",
            related_entity_id: "cluster_7",
          },
        ],
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(IMPACT_MAP_I18N_KEYS.title);
    expect(vm.emptyMessage).toBe(IMPACT_MAP_I18N_KEYS.empty);
    expect(vm.nodes[0].severityLabel).toBe(IMPACT_MAP_I18N_KEYS.severityHigh);
    expect(vm.nodes[0].confidenceText).toBe("84%");
    expect(vm.nodes[0].related_entity_count).toBe(12);
  });

  it("renders empty state via i18n key", () => {
    const vm = buildImpactMapViewModel({ impact_map: [] }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe("incident.qa.impact_map_empty");
  });

  it("maps severity labels for medium and low", () => {
    expect(getImpactSeverityLabelKey("medium")).toBe(IMPACT_MAP_I18N_KEYS.severityMedium);
    expect(getImpactSeverityLabelKey("low")).toBe(IMPACT_MAP_I18N_KEYS.severityLow);
  });

  it("formats confidence percentage", () => {
    expect(formatImpactConfidence(0.635)).toBe("64%");
    expect(formatImpactConfidence("bad")).toBe("—");
  });

  it("builds drilldown item compatible with II-02D navigation", () => {
    const item = buildImpactMapDrilldownItem({
      title: "Checkout",
      description: "Multiple signals",
      related_entity_type: "browser_watch",
      related_entity_id: "watch_123",
    });
    expect(item).not.toBeNull();
    const target = buildDrilldownNavigation(item);
    expect(target?.path).toBe("/browser-watch?watch=watch_123");
  });

  it("exposes EN translation keys used by impact map section", () => {
    expect(IMPACT_MAP_I18N_KEYS.signals).toBe("incident.qa.impact_map_signals");
    expect(IMPACT_MAP_I18N_KEYS.openDetails).toBe("incident.qa.impact_map_open_details");
  });
});
