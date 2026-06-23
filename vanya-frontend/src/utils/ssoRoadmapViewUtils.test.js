import { describe, expect, it } from "vitest";
import { buildSsoRoadmapViewModel } from "./ssoRoadmapViewUtils.js";

const t = (key) => key;

describe("buildSsoRoadmapViewModel", () => {
  it("maps providers to planned roadmap rows", () => {
    const vm = buildSsoRoadmapViewModel({
      providers: {
        providers: [
          { provider: "MICROSOFT" },
          { provider: "GOOGLE" },
          { provider: "OKTA" },
        ],
      },
      t,
    });

    expect(vm.title).toBe("sso_roadmap.title");
    expect(vm.providers).toHaveLength(3);
    expect(vm.providers[0].statusLabel).toBe("sso_roadmap.status_planned");
    expect(vm.providers[0].targetQuarter).toBe("Q3 2026");
    expect(vm.providers[2].targetQuarter).toBe("Q4 2026");
  });

  it("exposes available-today copy", () => {
    const vm = buildSsoRoadmapViewModel({ providers: { providers: [] }, t });
    expect(vm.availableTodayTitle).toBe("sso_roadmap.available_today_title");
    expect(vm.availableTodayBadge).toBe("sso_roadmap.status_available");
  });
});
