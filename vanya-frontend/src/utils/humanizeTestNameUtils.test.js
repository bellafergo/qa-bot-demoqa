import { describe, expect, it } from "vitest";
import { formatTestDisplayName, humanizeTestName } from "./humanizeTestNameUtils.js";

const t = (key) => key;

describe("humanizeTestNameUtils", () => {
  it("humanizes internal explorer smoke identifiers", () => {
    expect(humanizeTestName("vanya_self__explorer__exploration_landing_smoke", t)).toBe(
      "test_name.segment.exploration test_name.segment.landing",
    );
  });

  it("prefers explicit test name when different from id", () => {
    expect(formatTestDisplayName({
      testId: "AUTH-001",
      testName: "Login válido",
    }, t)).toBe("Login válido");
  });

  it("humanizes auth and checkout style ids", () => {
    expect(humanizeTestName("api_login_smoke", t)).toContain("test_name.segment.login");
    expect(humanizeTestName("checkout_flow", t)).toContain("test_name.segment.checkout");
  });
});
