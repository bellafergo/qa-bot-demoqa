import React from "react";

export function fmtTs(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short", day: "numeric", hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

export function severityBadge(sev) {
  const v = String(sev || "info").toLowerCase();
  if (v === "critical") return "badge badge-red";
  if (v === "high") return "badge badge-orange";
  if (v === "medium") return "badge badge-orange";
  if (v === "low") return "badge badge-blue";
  return "badge badge-gray";
}

export function reproducedLabel(rep, t) {
  const v = String(rep || "unknown").toLowerCase();
  if (v === "true") return t("incident.reproduced.yes");
  if (v === "false") return t("incident.reproduced.no");
  return t("incident.reproduced.unknown");
}

export function confidencePct(v) {
  const n = Number(v);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function hypothesisBasisLabel(basis, t) {
  if (basis === "evidence") return t("incident.qa.basis.evidence");
  if (basis === "assumption") return t("incident.qa.basis.assumption");
  return t("incident.qa.basis.inference");
}

export function strengthBadge(strength) {
  const v = String(strength || "medium").toLowerCase();
  if (v === "high") return "badge badge-red";
  if (v === "low") return "badge badge-gray";
  return "badge badge-orange";
}

export function emptyStateText(message) {
  return React.createElement(
    "p",
    { style: { fontSize: 13, color: "var(--text-3)", lineHeight: 1.6, margin: 0, fontStyle: "italic" } },
    message,
  );
}
