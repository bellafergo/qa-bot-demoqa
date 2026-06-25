import React from "react";
import { Link } from "react-router-dom";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";
import { SkeletonCard } from "../ui/Skeleton.jsx";

/**
 * Consistent empty, error, loading, and capability-gated states for dashboard sections.
 */
export default function DashboardSectionState({ state, onRetry, children, compact = false }) {
  if (!state) return children ?? null;

  if (state.loading) {
    return <SkeletonCard lines={4} />;
  }

  if (state.error) {
    return (
      <div className="alert alert-error" style={{ fontSize: 13, margin: 0 }}>
        {state.error}
        {onRetry ? (
          <button
            type="button"
            className="btn btn-secondary btn-sm"
            style={{ marginLeft: 8 }}
            onClick={onRetry}
          >
            {state.retryLabel}
          </button>
        ) : null}
      </div>
    );
  }

  if (state.capabilityState) {
    return <CapabilityStateCard state={state.capabilityState} compact={compact} />;
  }

  if (state.empty) {
    return (
      <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
          {state.emptyMessage}
        </p>
        {state.emptyCta ? (
          <Link
            to={state.emptyCta.path}
            style={{ fontSize: 13, color: "var(--accent)", fontWeight: 600, textDecoration: "none", width: "fit-content" }}
          >
            {state.emptyCta.label} →
          </Link>
        ) : null}
      </div>
    );
  }

  return children ?? null;
}
