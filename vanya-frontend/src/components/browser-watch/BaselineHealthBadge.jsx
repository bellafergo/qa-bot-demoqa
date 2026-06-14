import React from "react";
import {
  baselineHealthBadgeClass,
  baselineHealthLabelKey,
  resolveBaselineHealthBucket,
} from "../../utils/baselineExplorerViewUtils.js";

export default function BaselineHealthBadge({ baselineSetAt, t }) {
  const bucket = resolveBaselineHealthBucket(baselineSetAt);
  return (
    <span className={`badge ${baselineHealthBadgeClass(bucket)}`} style={{ fontSize: 10, letterSpacing: "0.04em" }}>
      {t(baselineHealthLabelKey(bucket))}
    </span>
  );
}
