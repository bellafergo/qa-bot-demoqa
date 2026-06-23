import { describe, expect, it } from "vitest";
import {
  localizeBackendMessage,
  localizeBackendMessages,
  BACKEND_MESSAGE_I18N_KEYS,
} from "./localizeBackendMessage.js";

const t = (key, vars) => {
  const map = {
    [BACKEND_MESSAGE_I18N_KEYS.noFailedTestRuns]: "No runs in {{hours}}h",
    [BACKEND_MESSAGE_I18N_KEYS.noFailureClusters]: "Sin clusters",
    [BACKEND_MESSAGE_I18N_KEYS.noIncidentReport]: "Sin informe",
  };
  let s = map[key] || key;
  if (vars) {
    Object.entries(vars).forEach(([k, v]) => {
      s = s.replace(`{{${k}}}`, String(v));
    });
  }
  return s;
};

describe("localizeBackendMessage", () => {
  it("maps exact backend strings", () => {
    expect(localizeBackendMessage("No failure intelligence clusters available.", t)).toBe("Sin clusters");
  });

  it("maps parameterized backend strings", () => {
    expect(
      localizeBackendMessage("No failed test runs found in the last 72h for this project.", t),
    ).toBe("No runs in 72h");
  });

  it("returns original when no mapping exists", () => {
    expect(localizeBackendMessage("Custom backend note", t)).toBe("Custom backend note");
  });

  it("localizes arrays", () => {
    expect(
      localizeBackendMessages(
        ["No failure intelligence clusters available.", "unknown"],
        t,
      ),
    ).toEqual(["Sin clusters", "unknown"]);
  });
});
