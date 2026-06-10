import React from "react";

export default function ProjectReadinessBadge({ label, badgeClass }) {
  return <span className={badgeClass}>{label}</span>;
}
