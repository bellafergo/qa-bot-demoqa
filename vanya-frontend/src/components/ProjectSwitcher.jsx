// src/components/ProjectSwitcher.jsx
import React, { useEffect, useRef, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useLang } from "../i18n/LangContext.jsx";
import { useProject } from "../context/ProjectContext.jsx";

function ColorDot({ color, size = 10 }) {
  return (
    <span
      aria-hidden
      style={{
        width: size,
        height: size,
        borderRadius: "50%",
        background: color || "#6366f1",
        flexShrink: 0,
        border: "1px solid rgba(255,255,255,0.2)",
      }}
    />
  );
}

export default function ProjectSwitcher() {
  const { t } = useLang();
  const navigate = useNavigate();
  const { projects, currentProject, setCurrentProject, loading, error } = useProject();
  const [open, setOpen] = useState(false);
  const rootRef = useRef(null);

  useEffect(() => {
    if (!open) return;
    function onDoc(e) {
      if (rootRef.current && !rootRef.current.contains(e.target)) {
        setOpen(false);
      }
    }
    document.addEventListener("mousedown", onDoc);
    return () => document.removeEventListener("mousedown", onDoc);
  }, [open]);

  function goProjects({ openCreate = false } = {}) {
    setOpen(false);
    if (openCreate) navigate("/projects?new=1");
    else navigate("/projects");
  }

  function selectProject(p) {
    setCurrentProject(p);
    setOpen(false);
  }

  const multi = projects.length > 1;
  const hasAny = projects.length > 0;

  return (
    <div ref={rootRef} className="project-switcher">
      {loading && (
        <div className="project-switcher-row project-switcher-row--muted">
          <span className="project-switcher-loading">{t("projects.switcher_loading")}</span>
        </div>
      )}

      {!loading && error && (
        <div className="project-switcher-row project-switcher-row--warn" title={error}>
          <span className="project-switcher-loading">{t("projects.switcher_offline")}</span>
        </div>
      )}

      {!loading && !error && !hasAny && (
        <button type="button" className="project-switcher-cta" onClick={() => goProjects({ openCreate: true })}>
          <ColorDot color="#94a3b8" size={10} />
          <span>{t("projects.switcher_create_project")}</span>
        </button>
      )}

      {!loading && !error && hasAny && currentProject && !multi && (
        <div className="project-switcher-single">
          <div className="project-switcher-row project-switcher-row--static">
            <ColorDot color={currentProject.color} />
            <span className="project-switcher-name">{currentProject.name}</span>
          </div>
          <div className="project-switcher-subactions">
            <button type="button" className="project-switcher-link" onClick={() => goProjects({ openCreate: true })}>
              {t("projects.switcher_new_project")}
            </button>
            <span className="project-switcher-sep">·</span>
            <button type="button" className="project-switcher-link" onClick={() => goProjects()}>
              {t("projects.switcher_view_all")}
            </button>
          </div>
        </div>
      )}

      {!loading && !error && hasAny && currentProject && multi && (
        <>
          <button
            type="button"
            className="project-switcher-trigger"
            onClick={() => setOpen((o) => !o)}
            aria-expanded={open}
            aria-haspopup="listbox"
          >
            <ColorDot color={currentProject.color} />
            <span className="project-switcher-name">{currentProject.name}</span>
            <span className="project-switcher-chevron" aria-hidden>
              {open ? "▴" : "▾"}
            </span>
          </button>
          {open && (
            <div className="project-switcher-dropdown" role="listbox">
              {projects.map((p) => (
                <button
                  key={p.id}
                  type="button"
                  role="option"
                  aria-selected={p.id === currentProject.id}
                  className={`project-switcher-option${p.id === currentProject.id ? " project-switcher-option--active" : ""}`}
                  onClick={() => selectProject(p)}
                >
                  <ColorDot color={p.color} />
                  <span>{p.name}</span>
                </button>
              ))}
              <div className="project-switcher-divider" />
              <button type="button" className="project-switcher-option" onClick={() => goProjects({ openCreate: true })}>
                {t("projects.switcher_new_project_plus")}
              </button>
              <button type="button" className="project-switcher-option" onClick={() => goProjects()}>
                {t("projects.switcher_view_all_arrow")}
              </button>
            </div>
          )}
        </>
      )}

      {!loading && !error && hasAny && !currentProject && (
        <button type="button" className="project-switcher-cta" onClick={() => goProjects()}>
          {t("projects.switcher_pick_project")}
        </button>
      )}
    </div>
  );
}
