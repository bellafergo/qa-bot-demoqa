import React from "react";

export default function BackendBadge({ backend }) {
  if (!backend) return null;
  if (backend === "mock")
    return <span className="badge badge-orange" style={{ fontSize: 10 }}>◎ mock</span>;
  return <span className="badge badge-green" style={{ fontSize: 10 }}>● real</span>;
}
