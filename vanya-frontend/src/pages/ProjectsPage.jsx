// src/pages/ProjectsPage.jsx
import React, { useState } from "react";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useLang } from "../i18n/LangContext.jsx";
import { useProject } from "../context/ProjectContext.jsx";
import ProjectModal from "../components/ProjectModal.jsx";
import ConfirmDialog from "../components/ConfirmDialog.jsx";
import { apiErrorMessage } from "../api.js";

export default function ProjectsPage() {
  const { t } = useLang();
  const navigate = useNavigate();
  const [searchParams, setSearchParams] = useSearchParams();
  const openCreateFromQuery = searchParams.get("new") === "1";

  const { projects, setCurrentProject, deleteProject, reloadProjects, loading, error } = useProject();

  const [modalOpen, setModalOpen] = useState(false);
  const [modalMode, setModalMode] = useState("create");
  const [editTarget, setEditTarget] = useState(null);
  const [deleteErr, setDeleteErr] = useState("");
  const [deleteTarget, setDeleteTarget] = useState(null);
  const [deleteBusy, setDeleteBusy] = useState(false);

  function clearNewQuery() {
    if (!openCreateFromQuery) return;
    const next = new URLSearchParams(searchParams);
    next.delete("new");
    setSearchParams(next, { replace: true });
  }

  const modalVisible = modalOpen || openCreateFromQuery;
  const effectiveModalMode =
    modalOpen && modalMode === "edit" ? "edit" : openCreateFromQuery ? "create" : modalMode;

  function openCreate() {
    clearNewQuery();
    setModalMode("create");
    setEditTarget(null);
    setModalOpen(true);
  }

  function openEdit(p) {
    clearNewQuery();
    setModalMode("edit");
    setEditTarget(p);
    setModalOpen(true);
  }

  function closeModal() {
    setModalOpen(false);
    clearNewQuery();
  }

  async function handleOpen(p) {
    setCurrentProject(p);
    navigate("/dashboard");
  }

  function requestDeleteProject(p) {
    setDeleteErr("");
    setDeleteTarget(p);
  }

  async function confirmDeleteProject() {
    if (!deleteTarget || deleteBusy) return;
    setDeleteBusy(true);
    try {
      await deleteProject(deleteTarget.id);
      setDeleteTarget(null);
    } catch (e) {
      setDeleteErr(apiErrorMessage(e));
      setDeleteTarget(null);
    } finally {
      setDeleteBusy(false);
    }
  }

  return (
    <div className="page-wrap" style={{ width: "100%", maxWidth: "none", paddingBottom: 48 }}>
      <div className="page-header">
        <h1 className="page-title">{t("projects.page_title")}</h1>
        <p className="page-subtitle">{t("projects.page_subtitle")}</p>
      </div>

      {error && !loading && (
        <div className="card" style={{ marginBottom: 16, borderColor: "var(--orange-border)" }}>
          <div style={{ fontSize: 13, color: "var(--orange-text)" }}>{error}</div>
          <button type="button" className="btn btn-secondary btn-sm" style={{ marginTop: 10 }} onClick={() => reloadProjects()}>
            {t("projects.retry")}
          </button>
        </div>
      )}

      {deleteErr && (
        <div
          className="card"
          style={{
            marginBottom: 16,
            borderColor: "var(--red-border)",
            background: "var(--red-bg)",
            fontSize: 13,
            color: "var(--red-text)",
          }}
        >
          {deleteErr}
          <button type="button" className="btn btn-secondary btn-sm" style={{ marginTop: 10 }} onClick={() => setDeleteErr("")}>
            {t("projects.dismiss")}
          </button>
        </div>
      )}

      <div style={{ marginBottom: 20, display: "flex", justifyContent: "flex-end" }}>
        <button type="button" className="btn btn-primary" onClick={openCreate}>
          {t("projects.create_new")}
        </button>
      </div>

      {loading && !projects.length && (
        <div
          className="card"
          style={{
            width: "100%",
            boxSizing: "border-box",
            textAlign: "center",
            padding: "40px 36px",
            color: "var(--text-3)",
          }}
        >
          {t("projects.loading_list")}
        </div>
      )}

      {!loading && !projects.length && !error && (
        <div
          className="card"
          style={{
            width: "100%",
            boxSizing: "border-box",
            padding: "48px 40px",
            minHeight: "min(52vh, 400px)",
            display: "flex",
            flexDirection: "column",
            alignItems: "center",
            justifyContent: "center",
            textAlign: "center",
            gap: 20,
            background: "var(--surface)",
          }}
        >
          <div
            aria-hidden
            style={{
              width: 56,
              height: 56,
              borderRadius: 14,
              flexShrink: 0,
              border: "1px dashed var(--border)",
              background: "var(--surface-2)",
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              fontSize: 22,
              color: "var(--text-3)",
            }}
          >
            ◇
          </div>
          <div style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)", lineHeight: 1.35 }}>
            {t("projects.empty_title")}
          </div>
          <p
            style={{
              fontSize: 14,
              color: "var(--text-2)",
              margin: 0,
              lineHeight: 1.6,
              maxWidth: "42rem",
              padding: "0 12px",
            }}
          >
            {t("projects.empty_hint")}
          </p>
          <div style={{ paddingTop: 4 }}>
            <button type="button" className="btn btn-primary" onClick={openCreate}>
              {t("projects.create_first")}
            </button>
          </div>
        </div>
      )}

      {projects.length > 0 && (
        <div
          style={{
            display: "grid",
            gridTemplateColumns: "repeat(auto-fill, minmax(280px, 1fr))",
            gap: 16,
          }}
        >
          {projects.map((p) => (
            <div key={p.id} className="card" style={{ display: "flex", flexDirection: "column", gap: 12 }}>
              <div style={{ display: "flex", alignItems: "flex-start", gap: 12 }}>
                <span
                  aria-hidden
                  style={{
                    width: 40,
                    height: 40,
                    borderRadius: 12,
                    background: p.color || "#6366f1",
                    flexShrink: 0,
                    border: "1px solid var(--border)",
                  }}
                />
                <div style={{ minWidth: 0 }}>
                  <div style={{ fontWeight: 600, fontSize: 15, color: "var(--text-1)" }}>{p.name}</div>
                  <div style={{ fontSize: 11, color: "var(--text-3)", fontFamily: "ui-monospace, monospace" }}>
                    {p.id}
                  </div>
                </div>
              </div>
              {p.description ? (
                <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.45 }}>{p.description}</p>
              ) : (
                <p style={{ fontSize: 12, color: "var(--text-4)", margin: 0, fontStyle: "italic" }}>
                  {t("projects.no_description")}
                </p>
              )}
              {p.base_url && (
                <div style={{ fontSize: 12, color: "var(--accent)", wordBreak: "break-all" }}>{p.base_url}</div>
              )}
              <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginTop: "auto", paddingTop: 8 }}>
                <button type="button" className="btn btn-primary btn-sm" onClick={() => handleOpen(p)}>
                  {t("projects.open")}
                </button>
                <button type="button" className="btn btn-secondary btn-sm" onClick={() => openEdit(p)}>
                  {t("projects.edit")}
                </button>
                <button type="button" className="btn btn-secondary btn-sm" onClick={() => requestDeleteProject(p)}>
                  {t("projects.delete")}
                </button>
              </div>
            </div>
          ))}
        </div>
      )}

      <ProjectModal
        open={modalVisible}
        mode={effectiveModalMode}
        project={effectiveModalMode === "edit" ? editTarget : null}
        onClose={closeModal}
      />

      <ConfirmDialog
        open={!!deleteTarget}
        title={t("projects.delete_dialog_title")}
        description={deleteTarget ? t("projects.confirm_delete", { name: deleteTarget.name }) : ""}
        confirmLabel={t("common.delete")}
        danger
        busy={deleteBusy}
        onCancel={() => !deleteBusy && setDeleteTarget(null)}
        onConfirm={confirmDeleteProject}
      />
    </div>
  );
}
