import React from "react";

export default function RunTypeBadge({ runType }) {
  if (!runType || runType === "ui") return null;
  if (runType === "desktop")
    return <span className="badge badge-blue" style={{ fontSize: 10, letterSpacing: "0.04em" }}>⊞ DESKTOP</span>;
  if (runType === "api")
    return <span className="badge badge-gray" style={{ fontSize: 10, letterSpacing: "0.04em" }}>⌥ API</span>;
  return null;
}
