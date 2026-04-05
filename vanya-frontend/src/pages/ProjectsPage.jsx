// src/pages/ProjectsPage.jsx
import React, { useState } from "react";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useLang } from "../i18n/LangContext.jsx";
import { useProject } from "../context/ProjectContext.jsx";
import ProjectModal from "../components/ProjectModal.jsx";
import { apiErrorMessage } from "../api.js";

export default function ProjectsPage() {
  const { t } = useLang();
  const navigate = useNavigate();
  const [searchParams, setSearchParams] = useSearchParams();
  const openCreateFromQuery = searchParams.get("new") === "1";

  const { projects, currentProject, setCurrentProject, deleteProject, reloadProjects, loading, error } =
    useProject();

  const [modalOpen, setModalOpen] = useState(false);
  const [modalMode, setModalMode] = useState("create");
  const [editTarget, setEditTarget] = useState(null);
  const [deleteErr, setDeleteErr] = useState("");

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

  async function handleDelete(p) {
    setDeleteErr("");
    const ok = window.confirm(
      t("projects.confirm_delete", { name: p.name }),
    );
    if (!ok) return;
    try {
      await deleteProject(p.id);
      if (currentProject?.id === p.id) {
        /* context reloadProjects inside deleteProject updates selection */
      }
    } catch (e) {
      setDeleteErr(apiErrorMessage(e));
    }
  }

  return (
    <div className="page-wrap" style={{ maxWidth: 960, paddingBottom: 48 }}>
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
        <div className="card" style={{ textAlign: "center", padding: 32, color: "var(--text-3)" }}>
          {t("projects.loading_list")}
        </div>
      )}

      {!loading && !projects.length && !error && (
        <div className="card" style={{ textAlign: "center", padding: 40 }}>
          <div
            aria-hidden
            style={{
              width: 56,
              height: 56,
              borderRadius: 14,
              margin: "0 auto 16px",
              border: "1px dashed var(--border)",
              background: "var(--surface-2, rgba(0,0,0,0.04))",
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              fontSize: 22,
              color: "var(--text-3)",
            }}
          >
            ◇
          </div>
          <div style={{ fontSize: 16, fontWeight: 600, marginBottom: 8 }}>{t("projects.empty_title")}</div>
          <p style={{ fontSize: 14, color: "var(--text-2)", marginBottom: 20 }}>{t("projects.empty_hint")}</p>
          <button type="button" className="btn btn-primary" onClick={openCreate}>
            {t("projects.create_first")}
          </button>
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
                <button type="button" className="btn btn-secondary btn-sm" onClick={() => handleDelete(p)}>
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
    </div>
  );
}
