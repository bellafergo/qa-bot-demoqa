import { describe, expect, it } from "vitest";
import { buildSettingsPlatformViewModel } from "./settingsPlatformViewUtils.js";

const t = (key) => key;

describe("buildSettingsPlatformViewModel", () => {
  it("maps backend meta to commercial labels without exposing internals", () => {
    const vm = buildSettingsPlatformViewModel({
      has_openai_key: true,
      has_db: true,
      has_cloudinary: false,
      supabase_configured: true,
      supabase_ok: true,
      model: "gpt-4.1-mini",
      render_git_commit: "abc123",
      sessions_in_memory: 4,
      doc_cache_items: 12,
    }, t);

    expect(vm.aiEngineLabel).toBe("settings.platform.ai_engine");
    expect(vm.aiEngineStatus).toBe("settings.platform.status_operational");
    expect(vm.databaseStatus).toBe("settings.platform.status_connected");
    expect(vm.evidenceStorageStatus).toBe("settings.platform.status_not_configured");
    expect(vm.lastUpdatedValue).toBe("settings.platform.last_updated_value");
    expect(vm).not.toHaveProperty("model");
    expect(vm).not.toHaveProperty("gitCommit");
  });
});
