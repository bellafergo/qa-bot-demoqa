import React from "react";
import { useLang } from "../../i18n/LangContext";

export default function RiskBadge({ level }) {
  const { t } = useLang();
  if (!level) return null;
  const v = String(level).toLowerCase();
  if (v === "high")   return <span className="badge badge-red">{t("runs.risk.high")}</span>;
  if (v === "medium") return <span className="badge badge-orange">{t("runs.risk.medium")}</span>;
  return <span className="badge badge-green">{t("runs.risk.low")}</span>;
}
