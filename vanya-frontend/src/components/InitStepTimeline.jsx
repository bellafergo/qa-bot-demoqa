// src/components/InitStepTimeline.jsx
import React from "react";
import { Link } from "react-router-dom";
import { useLang } from "../i18n/LangContext";
import {
  isSmokeQueued,
  sortInitSteps,
  stepStatusBadgeClass,
  stepStatusIcon,
} from "../utils/initStepUtils";

const STEP_LABEL_KEYS = {
  catalog: "init.step.catalog",
  knowledge: "init.step.knowledge",
  smoke_run: "init.step.smoke_run",
  readiness: "init.step.readiness",
};

export default function InitStepTimeline({ result }) {
  const { t } = useLang();
  if (!result?.steps?.length) return null;

  const steps = sortInitSteps(result.steps);
  const smokeQueued = isSmokeQueued(result);

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 10 }}>
        {t("init.timeline.title")}
      </div>
      <ol style={{ margin: 0, padding: 0, listStyle: "none", display: "flex", flexDirection: "column", gap: 0 }}>
        {steps.map((s, idx) => (
          <li
            key={s.step}
            style={{
              display: "flex",
              gap: 12,
              alignItems: "flex-start",
              padding: "10px 0",
              borderBottom: idx < steps.length - 1 ? "1px solid var(--border)" : "none",
            }}
          >
            <span
              aria-hidden
              style={{
                width: 26,
                height: 26,
                borderRadius: "50%",
                display: "flex",
                alignItems: "center",
                justifyContent: "center",
                fontSize: 13,
                fontWeight: 700,
                flexShrink: 0,
                background: "var(--surface)",
                border: "1px solid var(--border)",
                color: s.status === "ok" ? "var(--green)" : s.status === "failed" ? "var(--red)" : "var(--text-3)",
              }}
            >
              {stepStatusIcon(s.status)}
            </span>
            <div style={{ flex: 1, minWidth: 0 }}>
              <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
                <span style={{ fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>
                  {t(STEP_LABEL_KEYS[s.step] || s.step)}
                </span>
                <span className={`badge ${stepStatusBadgeClass(s.status)}`} style={{ fontSize: 10 }}>
                  {t(`init.step.status.${s.status}`)}
                </span>
              </div>
              {s.message ? (
                <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4, lineHeight: 1.45 }}>
                  {s.message}
                </div>
              ) : null}
            </div>
          </li>
        ))}
      </ol>
      {smokeQueued ? (
        <div
          className="alert"
          style={{
            marginTop: 12,
            fontSize: 12,
            color: "var(--text-2)",
            background: "var(--accent-light, var(--surface))",
            borderColor: "var(--accent-border, var(--border))",
            lineHeight: 1.5,
          }}
        >
          {t("init.smoke_queued")}{" "}
          <Link to="/runs" style={{ color: "var(--accent)", fontWeight: 500 }}>
            {t("init.view_runs")}
          </Link>
        </div>
      ) : null}
    </div>
  );
}
