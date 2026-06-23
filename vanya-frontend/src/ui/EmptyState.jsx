import React from "react";
import { Link } from "react-router-dom";

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

export function LoadingState({ message, className = "" }) {
  return (
    <div
      className={`zu-empty-state zu-empty-state--loading ${className}`.trim()}
      role="status"
      aria-live="polite"
    >
      <div className="zu-empty-state__spinner" aria-hidden />
      <div className="zu-empty-state__title">{message}</div>
    </div>
  );
}

export function ErrorState({
  title,
  description,
  onRetry,
  retryLabel,
  className = "",
}) {
  return (
    <div className={`zu-empty-state zu-empty-state--error ${className}`.trim()} role="alert">
      {title ? <div className="zu-empty-state__title">{title}</div> : null}
      {description ? <div className="zu-empty-state__desc">{description}</div> : null}
      {onRetry ? (
        <div className="zu-empty-state__action">
          <button type="button" className="btn btn-secondary btn-sm" onClick={onRetry}>
            {retryLabel}
          </button>
        </div>
      ) : null}
    </div>
  );
}

export function EmptyStateLinkAction({ to, label }) {
  return (
    <Link to={to} className="btn btn-primary btn-sm" style={{ textDecoration: "none" }}>
      {label}
    </Link>
  );
}
