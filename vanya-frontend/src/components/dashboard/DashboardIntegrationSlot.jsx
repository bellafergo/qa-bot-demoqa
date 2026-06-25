import React from "react";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";
import DashboardSectionState from "./DashboardSectionState.jsx";
import { CAPABILITY_STATE } from "../../utils/capabilityStateViewUtils.js";

export default function DashboardIntegrationSlot({
  section,
  vm,
  contentComponent,
  onRetry,
  compactWhenGated = true,
}) {
  if (!section?.show || !vm) return null;

  const isIntegrationGated = section.capabilityState?.state === CAPABILITY_STATE.INTEGRATION_REQUIRED;

  if (compactWhenGated && isIntegrationGated) {
    return <CapabilityStateCard state={section.capabilityState} compact />;
  }

  const Content = contentComponent;

  return (
    <div>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
        {vm.title}
      </div>
      <DashboardSectionState state={section} onRetry={onRetry} compact={compactWhenGated}>
        {Content ? <Content vm={vm} /> : null}
      </DashboardSectionState>
    </div>
  );
}
