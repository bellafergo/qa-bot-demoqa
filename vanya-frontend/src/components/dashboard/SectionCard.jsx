import React from "react";
import { Link } from "react-router-dom";

export default function SectionCard({ title, link, linkLabel, children }) {
  return (
    <div className="card" style={{ padding: 0, overflow: "hidden" }}>
      <div style={{
        padding: "16px 22px", borderBottom: "1px solid var(--border)",
        display: "flex", alignItems: "center", justifyContent: "space-between",
      }}>
        <div className="section-title" style={{ margin: 0 }}>{title}</div>
        {link && (
          <Link to={link} style={{ fontSize: 12, color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>
            {linkLabel || "View all →"}
          </Link>
        )}
      </div>
      {children}
    </div>
  );
}
