import React, { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import {
  getBrowserInspectionWatch,
  getClusters,
  getTestRun,
} from "../../api";
import { useLang } from "../../i18n/LangContext.jsx";
import { useProject } from "../../context/ProjectContext.jsx";
import CorrelationDrilldownModal from "./CorrelationDrilldownModal.jsx";
import {
  buildDrilldownModalPayload,
  buildDrilldownNavigation,
  getDrilldownActionLabelKey,
  getNonNavigableDrilldownLabelKey,
  hasDrilldownMetadata,
  isNavigableDrilldownType,
  isNonNavigableDrilldownType,
  shouldShowDrilldownUnavailable,
  shouldUseDrilldownModalFallback,
} from "../../utils/correlationDrilldownUtils.js";
import { verifyDrilldownEntity } from "../../utils/correlationDrilldownVerify.js";

async function verifyEntity(entityType, entityId, projectId) {
  return verifyDrilldownEntity(entityType, entityId, projectId, {
    getTestRun,
    getBrowserInspectionWatch,
    getClusters,
  });
}

export default function EvidenceCorrelationDrilldownCell({ item }) {
  const { t } = useLang();
  const navigate = useNavigate();
  const { currentProject } = useProject();
  const [busy, setBusy] = useState(false);
  const [modalOpen, setModalOpen] = useState(false);
  const [modalPayload, setModalPayload] = useState(null);
  const [unavailableMessage, setUnavailableMessage] = useState("");

  const entityType = String(item?.related_entity_type || "").trim();

  const openModal = useCallback((message = "") => {
    setUnavailableMessage(message);
    setModalPayload(buildDrilldownModalPayload(item));
    setModalOpen(true);
  }, [item]);

  const handleNavigate = useCallback(async () => {
    const entityId = String(item?.related_entity_id || "").trim();
    const target = buildDrilldownNavigation(item);
    if (!target || target.kind !== "navigate") return;

    setBusy(true);
    try {
      const exists = await verifyEntity(entityType, entityId, currentProject?.id);
      if (!exists) {
        if (shouldUseDrilldownModalFallback(item)) {
          openModal(t("incident.qa.drilldown.entity_unavailable"));
        } else {
          openModal(t("incident.qa.drilldown.entity_unavailable"));
        }
        return;
      }
      if (target.state) {
        navigate(target.path, { state: target.state });
      } else {
        navigate(target.path);
      }
    } finally {
      setBusy(false);
    }
  }, [currentProject?.id, entityType, item, navigate, openModal, t]);

  if (isNonNavigableDrilldownType(entityType)) {
    return (
      <span style={{ fontSize: 12, color: "var(--text-3)", fontStyle: "italic" }}>
        {t(getNonNavigableDrilldownLabelKey(entityType))}
      </span>
    );
  }

  if (shouldShowDrilldownUnavailable(item)) {
    return (
      <span style={{ fontSize: 12, color: "var(--text-3)" }}>
        {t("incident.qa.drilldown.unavailable")}
      </span>
    );
  }

  if (!isNavigableDrilldownType(entityType) || !hasDrilldownMetadata(item)) {
    return (
      <span style={{ fontSize: 12, color: "var(--text-3)" }}>
        {t("incident.qa.drilldown.unavailable")}
      </span>
    );
  }

  const labelKey = getDrilldownActionLabelKey(entityType);

  return (
    <>
      <button
        type="button"
        className="btn btn-secondary btn-sm"
        title={t("incident.qa.drilldown.navigation_tooltip")}
        disabled={busy}
        onClick={() => {
          handleNavigate();
        }}
        style={{ fontSize: 11, padding: "2px 8px" }}
      >
        {busy ? t("common.working") : t(labelKey)}
      </button>
      <CorrelationDrilldownModal
        open={modalOpen}
        title={modalPayload ? t(modalPayload.titleKey) : ""}
        fields={modalPayload?.fields || []}
        unavailableMessage={unavailableMessage}
        onClose={() => {
          setModalOpen(false);
          setUnavailableMessage("");
        }}
      />
    </>
  );
}
