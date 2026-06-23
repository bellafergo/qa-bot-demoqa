import { describe, expect, it, beforeEach } from "vitest";
import {
  dismissInitializeProjectPanel,
  isInitializeProjectPanelDismissed,
  clearInitializeProjectPanelDismiss,
} from "./initStepUtils.js";

describe("initStepUtils dismiss", () => {
  const pid = "test-project-dismiss";
  const store = new Map();

  beforeEach(() => {
    store.clear();
    globalThis.localStorage = {
      getItem: (key) => store.get(key) ?? null,
      setItem: (key, value) => { store.set(key, String(value)); },
      removeItem: (key) => { store.delete(key); },
    };
  });

  it("tracks dismiss state per project", () => {
    clearInitializeProjectPanelDismiss(pid);
    expect(isInitializeProjectPanelDismissed(pid)).toBe(false);
    dismissInitializeProjectPanel(pid);
    expect(isInitializeProjectPanelDismissed(pid)).toBe(true);
    clearInitializeProjectPanelDismiss(pid);
    expect(isInitializeProjectPanelDismissed(pid)).toBe(false);
  });
});
