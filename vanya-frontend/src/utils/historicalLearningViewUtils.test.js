import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  HISTORICAL_LEARNING_I18N_KEYS,
  buildHistoricalLearningViewModel,
  buildSimilarIncidentDrilldownItem,
  buildSimilarIncidentPreviewPayload,
  formatHistoricalLearningConfidence,
  formatSimilarityScore,
  hasHistoricalLearningSection,
  isHistoricalLearningEmpty,
} from "./historicalLearningViewUtils.js";

const t = (key) => key;

describe("historicalLearningViewUtils", () => {
  it("detects historical learning section and empty state", () => {
    expect(hasHistoricalLearningSection({ historical_learning: null })).toBe(true);
    expect(hasHistoricalLearningSection({})).toBe(false);
    expect(isHistoricalLearningEmpty({ historical_learning: null })).toBe(true);
  });

  it("renders pattern summary and similar incident list", () => {
    const vm = buildHistoricalLearningViewModel(
      {
        historical_learning: {
          pattern_summary: "5 similar incidents involved Checkout.",
          confidence: 0.82,
          similar_incidents: [
            {
              incident_id: "hist-41",
              title: "Checkout Regression #41",
              similarity_score: 0.91,
              summary: "Checkout failures clustered after release.",
              occurrence_timestamp: "2026-06-08T10:00:00+00:00",
              related_entity_type: "failure_cluster",
              related_entity_id: "cluster_7",
            },
          ],
        },
      },
      t,
      (iso) => (iso ? "Jun 8" : "—"),
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.learning.pattern_summary).toContain("Checkout");
    expect(vm.learning.confidenceText).toBe("82%");
    expect(vm.learning.incidents[0].similarityText).toBe("91%");
    expect(vm.learning.incidents[0].timestampText).toBe("Jun 8");
  });

  it("renders empty state via i18n key", () => {
    const vm = buildHistoricalLearningViewModel({ historical_learning: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.historical_learning_empty");
  });

  it("formats similarity score display", () => {
    expect(formatSimilarityScore(0.876)).toBe("88%");
    expect(formatHistoricalLearningConfidence(0.82)).toBe("82%");
  });

  it("builds pattern summary in view model", () => {
    const vm = buildHistoricalLearningViewModel(
      {
        historical_learning: {
          pattern_summary: "Most incidents affecting Authentication required Login Smoke validation.",
          confidence: 0.76,
          similar_incidents: [],
        },
      },
      t,
    );
    expect(vm.learning.pattern_summary).toContain("Authentication");
  });

  it("builds drilldown item for II-02D navigation", () => {
    const item = buildSimilarIncidentDrilldownItem({
      title: "Checkout Regression #41",
      summary: "Cluster overlap",
      related_entity_type: "failure_cluster",
      related_entity_id: "cluster_7",
    });
    const target = buildDrilldownNavigation(item);
    expect(target?.path).toContain("failure-intel");
  });

  it("exposes translation keys", () => {
    expect(HISTORICAL_LEARNING_I18N_KEYS.title).toBe("incident.qa.historical_learning");
    expect(HISTORICAL_LEARNING_I18N_KEYS.similarity).toBe("incident.qa.historical_learning_similarity");
    expect(HISTORICAL_LEARNING_I18N_KEYS.patternSummary).toBe(
      "incident.qa.historical_learning_pattern_summary",
    );
  });

  it("builds read-only preview payload", () => {
    const payload = buildSimilarIncidentPreviewPayload(
      {
        title: "Checkout Regression #41",
        similarity_score: 0.91,
        summary: "Checkout failures clustered.",
        timestampText: "Jun 8",
      },
      t,
    );
    expect(payload.title).toBe("Checkout Regression #41");
    expect(payload.fields[0].value).toBe("91%");
    expect(payload.readOnlyNote).toBe(HISTORICAL_LEARNING_I18N_KEYS.readOnlyNote);
  });
});
