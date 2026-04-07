import React from "react";

export default function EmptyState({ icon, title, description, action, className = "" }) {
  return (
    <div className={`zu-empty-state ${className}`.trim()}>
      {icon != null && <div className="zu-empty-state__icon" aria-hidden>{icon}</div>}
      <div className="zu-empty-state__title">{title}</div>
      {description ? <div className="zu-empty-state__desc">{description}</div> : null}
      {action ? <div className="zu-empty-state__action">{action}</div> : null}
    </div>
  );
}
