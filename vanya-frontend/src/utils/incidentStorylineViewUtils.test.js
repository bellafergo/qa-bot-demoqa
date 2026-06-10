import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  STORYLINE_I18N_KEYS,
  buildStorylineDrilldownItem,
  buildStorylineViewModel,
  formatStorylineConfidence,
  hasStorylineSection,
  isStorylineEmpty,
} from "./incidentStorylineViewUtils.js";

const t = (key) => key;

describe("incidentStorylineViewUtils", () => {
  it("detects storyline section presence", () => {
    expect(hasStorylineSection({ storyline: [] })).toBe(true);
    expect(hasStorylineSection({ storyline: [{ title: "PR merged" }] })).toBe(true);
    expect(hasStorylineSection({})).toBe(false);
    expect(isStorylineEmpty({ storyline: [] })).toBe(true);
  });

  it("builds storyline view model with translation keys", () => {
    const vm = buildStorylineViewModel(
      {
        storyline: [
          {
            step_number: 1,
            title: "PR #483 merged",
            description: "Recent code change correlated with incident evidence",
            confidence: 0.92,
            timestamp: "2026-06-09T08:00:00+00:00",
          },
        ],
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(STORYLINE_I18N_KEYS.title);
    expect(vm.emptyMessage).toBe(STORYLINE_I18N_KEYS.empty);
    expect(vm.steps[0].confidenceText).toBe("92%");
    expect(vm.steps[0].isLast).toBe(true);
  });

  it("renders empty state message via i18n key", () => {
    const vm = buildStorylineViewModel({ storyline: [] }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe("incident.qa.storyline_empty");
  });

  it("builds drilldown item compatible with II-02D navigation", () => {
    const item = buildStorylineDrilldownItem({
      title: "Failed run: Auth login",
      description: "AssertionError",
      related_entity_type: "run",
      related_entity_id: "RUN-99",
    });
    expect(item).not.toBeNull();
    const target = buildDrilldownNavigation(item);
    expect(target?.path).toBe("/runs");
    expect(target?.state?.run_id).toBe("RUN-99");
  });

  it("returns null drilldown item when entity metadata missing", () => {
    expect(buildStorylineDrilldownItem({ title: "Investigation conclusion" })).toBeNull();
  });

  it("formats storyline confidence", () => {
    expect(formatStorylineConfidence(0.885)).toBe("89%");
    expect(formatStorylineConfidence("bad")).toBe("—");
  });
});
