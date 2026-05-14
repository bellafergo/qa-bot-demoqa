// src/components/ProjectModal.jsx
import React, { useEffect, useRef, useState } from "react";
import { useLang } from "../i18n/LangContext.jsx";
import { useProject } from "../context/ProjectContext.jsx";
import { apiErrorMessage } from "../api.js";
import { isValidProjectSlug, suggestProjectIdFromName } from "../lib/projectSlug.js";

const PRESET_COLORS = [
  "#6366f1",
  "#2563eb",
  "#059669",
  "#d97706",
  "#dc2626",
  "#7c3aed",
  "#db2777",
  "#0d9488",
];

function readLoginProfile(proj) {
  const lp = proj?.settings?.login_profile;
  return lp && typeof lp === "object" ? lp : {};
}

function variablePresentLabel(proj, key) {
  const v = proj?.settings?.variables?.[key];
  if (v && typeof v === "object" && v.present) {
    if (v.sensitive) return "set";
    const pr = String(v.preview || "").trim();
    return pr || "set";
  }
  return "";
}

export default function ProjectModal({ open, mode, project, onClose }) {
  const { t } = useLang();
  const { createProject, updateProject } = useProject();

  const [name, setName] = useState("");
  const [slug, setSlug] = useState("");
  const [description, setDescription] = useState("");
  const [color, setColor] = useState(PRESET_COLORS[0]);
  const [baseUrl, setBaseUrl] = useState("");
  const [loginUrl, setLoginUrl] = useState("");
  /** Empty by default so PATCH can send only `settings.variables` without inventing a login_profile. */
  const [emailSel, setEmailSel] = useState("");
  const [passSel, setPassSel] = useState("");
  const [submitSel, setSubmitSel] = useState("");
  const [successText, setSuccessText] = useState("");
  const [successUrl, setSuccessUrl] = useState("");
  const [varEmail, setVarEmail] = useState("");
  const [varPassword, setVarPassword] = useState("");
  const [varHintEmail, setVarHintEmail] = useState("");
  const [submitErr, setSubmitErr] = useState("");
  const [saving, setSaving] = useState(false);
  const slugEditedRef = useRef(false);
  const githubTouchedRef = useRef(false);

  const [ghEnabled, setGhEnabled] = useState(false);
  const [ghRepoUrl, setGhRepoUrl] = useState("");
  const [ghOwner, setGhOwner] = useState("");
  const [ghRepo, setGhRepo] = useState("");
  const [ghBranch, setGhBranch] = useState("");
  const [ghInstallation, setGhInstallation] = useState("");
  const [ghToken, setGhToken] = useState("");
  const [ghTokenHint, setGhTokenHint] = useState("");

  useEffect(() => {
    if (!open) return;
    setSubmitErr("");
    slugEditedRef.current = false;
    githubTouchedRef.current = false;
    if (mode === "edit" && project) {
      setName(project.name || "");
      setSlug(project.id || "");
      setDescription(project.description || "");
      setColor(project.color || PRESET_COLORS[0]);
      setBaseUrl(project.base_url || "");
      const lp = readLoginProfile(project);
      setLoginUrl(lp.login_url || "");
      setEmailSel((lp.email_selector || "").trim());
      setPassSel((lp.password_selector || "").trim());
      setSubmitSel((lp.submit_selector || "").trim());
      setSuccessText(lp.success_text || "");
      setSuccessUrl(lp.success_url_contains || lp.success_url || "");
      setVarEmail("");
      setVarHintEmail(variablePresentLabel(project, "EMAIL"));
      setVarPassword("");
      const g = project?.settings?.github;
      setGhEnabled(Boolean(g?.enabled));
      setGhRepoUrl(typeof g?.repo_url === "string" ? g.repo_url : "");
      setGhOwner(typeof g?.owner === "string" ? g.owner : "");
      setGhRepo(typeof g?.repo === "string" ? g.repo : "");
      setGhBranch(typeof g?.default_branch === "string" ? g.default_branch : "");
      setGhInstallation(typeof g?.installation_id === "string" ? g.installation_id : "");
      setGhToken("");
      const tok = g?.github_token;
      setGhTokenHint(tok && typeof tok === "object" && tok.present ? "set" : "");
      slugEditedRef.current = true;
    } else {
      setName("");
      setSlug("");
      setDescription("");
      setColor(PRESET_COLORS[0]);
      setBaseUrl("");
      setLoginUrl("");
      setEmailSel("");
      setPassSel("");
      setSubmitSel("");
      setSuccessText("");
      setSuccessUrl("");
      setVarEmail("");
      setVarHintEmail("");
      setVarPassword("");
      setGhEnabled(false);
      setGhRepoUrl("");
      setGhOwner("");
      setGhRepo("");
      setGhBranch("");
      setGhInstallation("");
      setGhToken("");
      setGhTokenHint("");
    }
  }, [open, mode, project]);

  useEffect(() => {
    if (!open || mode !== "create") return;
    if (slugEditedRef.current) return;
    setSlug(suggestProjectIdFromName(name));
  }, [name, open, mode]);

  if (!open) return null;

  const title =
    mode === "create" ? t("projects.modal_create_title") : t("projects.modal_edit_title");

  function buildOptionalSettings() {
    const login_profile = {};
    if (loginUrl.trim()) login_profile.login_url = loginUrl.trim();
    if (emailSel.trim()) login_profile.email_selector = emailSel.trim();
    if (passSel.trim()) login_profile.password_selector = passSel.trim();
    if (submitSel.trim()) login_profile.submit_selector = submitSel.trim();
    if (successText.trim()) login_profile.success_text = successText.trim();
    if (successUrl.trim()) login_profile.success_url_contains = successUrl.trim();

    const variables = {};
    if (varEmail.trim()) variables.EMAIL = varEmail.trim();
    if (varPassword.trim()) variables.PASSWORD = varPassword.trim();

    const settings = {};
    if (Object.keys(login_profile).length) settings.login_profile = login_profile;
    if (Object.keys(variables).length) settings.variables = variables;

    if (githubTouchedRef.current) {
      const github = { enabled: Boolean(ghEnabled) };
      if (ghRepoUrl.trim()) github.repo_url = ghRepoUrl.trim();
      if (ghOwner.trim()) github.owner = ghOwner.trim();
      if (ghRepo.trim()) github.repo = ghRepo.trim();
      if (ghBranch.trim()) github.default_branch = ghBranch.trim();
      if (ghInstallation.trim()) github.installation_id = ghInstallation.trim();
      if (ghToken.trim()) github.github_token = ghToken.trim();
      settings.github = github;
    }

    return settings;
  }

  async function handleSubmit(e) {
    e.preventDefault();
    setSubmitErr("");
    const id = String(slug || "").trim().toLowerCase();
    const nm = String(name || "").trim();
    if (!nm) {
      setSubmitErr(t("projects.err_name_required"));
      return;
    }
    if (!isValidProjectSlug(id)) {
      setSubmitErr(t("projects.err_slug_invalid"));
      return;
    }
    if (githubTouchedRef.current && ghEnabled && (!ghOwner.trim() || !ghRepo.trim())) {
      setSubmitErr(t("projects.github_err_need_owner_repo"));
      return;
    }
    const optSettings = buildOptionalSettings();
    setSaving(true);
    try {
      if (mode === "create") {
        await createProject({
          id,
          name: nm,
          description: description.trim() || "",
          color: color || PRESET_COLORS[0],
          base_url: baseUrl.trim() || null,
          ...(Object.keys(optSettings).length ? { settings: optSettings } : {}),
        });
      } else if (project?.id) {
        await updateProject(project.id, {
          name: nm,
          description: description.trim() || "",
          color: color || PRESET_COLORS[0],
          base_url: baseUrl.trim() || null,
          ...(Object.keys(optSettings).length ? { settings: optSettings } : {}),
        });
      }
      onClose();
    } catch (err) {
      setSubmitErr(apiErrorMessage(err));
    } finally {
      setSaving(false);
    }
  }

  return (
    <div
      className="project-modal-backdrop"
      role="presentation"
      onMouseDown={(e) => {
        if (e.target === e.currentTarget) onClose();
      }}
    >
      <div className="project-modal card" role="dialog" aria-labelledby="project-modal-title">
        <h2 id="project-modal-title" className="section-title" style={{ marginBottom: 16 }}>
          {title}
        </h2>
        <form onSubmit={handleSubmit}>
          <label className="project-modal-label">
            {t("projects.field_name")}
            <input
              className="input"
              value={name}
              onChange={(e) => setName(e.target.value)}
              autoComplete="off"
              required
            />
          </label>
          <label className="project-modal-label">
            {t("projects.field_slug")}
            <input
              className="input"
              style={{ fontFamily: "ui-monospace, monospace" }}
              value={slug}
              onChange={(e) => {
                slugEditedRef.current = true;
                setSlug(e.target.value.toLowerCase());
              }}
              disabled={mode === "edit"}
              title={mode === "edit" ? t("projects.slug_locked_hint") : undefined}
            />
            <span className="project-modal-hint">{t("projects.field_slug_hint")}</span>
          </label>
          <label className="project-modal-label">
            {t("projects.field_description")}
            <textarea
              className="input"
              rows={3}
              value={description}
              onChange={(e) => setDescription(e.target.value)}
            />
          </label>
          <div className="project-modal-label">
            <span>{t("projects.field_color")}</span>
            <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginTop: 6 }}>
              {PRESET_COLORS.map((c) => (
                <button
                  key={c}
                  type="button"
                  title={c}
                  onClick={() => setColor(c)}
                  style={{
                    width: 28,
                    height: 28,
                    borderRadius: "50%",
                    border:
                      color === c ? "2px solid var(--text-1)" : "2px solid var(--border)",
                    background: c,
                    cursor: "pointer",
                    padding: 0,
                  }}
                />
              ))}
            </div>
          </div>
          <label className="project-modal-label">
            {t("projects.field_base_url")}
            <input
              className="input"
              value={baseUrl}
              onChange={(e) => setBaseUrl(e.target.value)}
              placeholder="https://"
              autoComplete="off"
            />
          </label>

          <fieldset
            style={{
              border: "1px solid var(--border)",
              borderRadius: "var(--r-sm)",
              padding: "12px 14px",
              marginTop: 16,
              marginBottom: 8,
            }}
          >
            <legend style={{ fontSize: 13, fontWeight: 600, padding: "0 6px" }}>
              {t("projects.github_section")}
            </legend>
            <p className="project-modal-hint" style={{ marginBottom: 12 }}>
              {t("projects.github_section_hint")}
            </p>
            <label style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 12, fontSize: 13 }}>
              <input
                type="checkbox"
                checked={ghEnabled}
                onChange={(e) => {
                  githubTouchedRef.current = true;
                  setGhEnabled(e.target.checked);
                }}
              />
              {t("projects.github_enabled")}
            </label>
            <label className="project-modal-label">
              {t("projects.github_repo_url")}
              <input
                className="input"
                value={ghRepoUrl}
                onChange={(e) => {
                  githubTouchedRef.current = true;
                  setGhRepoUrl(e.target.value);
                }}
                placeholder="https://github.com/org/repo"
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.github_owner")}
              <input
                className="input"
                style={{ fontFamily: "ui-monospace, monospace", fontSize: 13 }}
                value={ghOwner}
                onChange={(e) => {
                  githubTouchedRef.current = true;
                  setGhOwner(e.target.value);
                }}
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.github_repo")}
              <input
                className="input"
                style={{ fontFamily: "ui-monospace, monospace", fontSize: 13 }}
                value={ghRepo}
                onChange={(e) => {
                  githubTouchedRef.current = true;
                  setGhRepo(e.target.value);
                }}
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.github_branch")}
              <input
                className="input"
                value={ghBranch}
                onChange={(e) => {
                  githubTouchedRef.current = true;
                  setGhBranch(e.target.value);
                }}
                placeholder="main"
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.github_installation")}
              <input
                className="input"
                value={ghInstallation}
                onChange={(e) => {
                  githubTouchedRef.current = true;
                  setGhInstallation(e.target.value);
                }}
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.github_token")}
              {mode === "edit" && ghTokenHint ? (
                <span className="project-modal-hint" style={{ display: "block", marginBottom: 4 }}>
                  {t("projects.github_token_hint_set")}
                </span>
              ) : null}
              <input
                className="input"
                type="password"
                value={ghToken}
                onChange={(e) => {
                  githubTouchedRef.current = true;
                  setGhToken(e.target.value);
                }}
                autoComplete="new-password"
                placeholder={mode === "edit" ? t("projects.password_unchanged_hint") : ""}
              />
            </label>
          </fieldset>

          <fieldset
            style={{
              border: "1px solid var(--border)",
              borderRadius: "var(--r-sm)",
              padding: "12px 14px",
              marginTop: 16,
              marginBottom: 8,
            }}
          >
            <legend style={{ fontSize: 13, fontWeight: 600, padding: "0 6px" }}>
              {t("projects.login_section")}
            </legend>
            <p className="project-modal-hint" style={{ marginBottom: 12 }}>
              {t("projects.login_section_hint")}
            </p>
            <label className="project-modal-label">
              {t("projects.login_url")}
              <input
                className="input"
                value={loginUrl}
                onChange={(e) => setLoginUrl(e.target.value)}
                placeholder="/login"
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.login_email_selector")}
              <input
                className="input"
                style={{ fontFamily: "ui-monospace, monospace", fontSize: 13 }}
                value={emailSel}
                onChange={(e) => setEmailSel(e.target.value)}
                placeholder={t("projects.ph_email_selector")}
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.login_password_selector")}
              <input
                className="input"
                style={{ fontFamily: "ui-monospace, monospace", fontSize: 13 }}
                value={passSel}
                onChange={(e) => setPassSel(e.target.value)}
                placeholder={t("projects.ph_password_selector")}
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.login_submit_selector")}
              <input
                className="input"
                style={{ fontFamily: "ui-monospace, monospace", fontSize: 13 }}
                value={submitSel}
                onChange={(e) => setSubmitSel(e.target.value)}
                placeholder={t("projects.ph_submit_selector")}
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.login_success_text")}
              <input
                className="input"
                value={successText}
                onChange={(e) => setSuccessText(e.target.value)}
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.login_success_url")}
              <input
                className="input"
                value={successUrl}
                onChange={(e) => setSuccessUrl(e.target.value)}
                placeholder="dashboard"
                autoComplete="off"
              />
            </label>
            <label className="project-modal-label">
              {t("projects.var_email")}
              {mode === "edit" && varHintEmail ? (
                <span className="project-modal-hint" style={{ display: "block", marginBottom: 4 }}>
                  {t("projects.var_keep_hint", { value: String(varHintEmail) })}
                </span>
              ) : null}
              <input
                className="input"
                value={varEmail}
                onChange={(e) => setVarEmail(e.target.value)}
                autoComplete="off"
                placeholder={t("projects.var_leave_blank_keep")}
              />
            </label>
            <label className="project-modal-label">
              {t("projects.var_password")}
              <input
                className="input"
                type="password"
                value={varPassword}
                onChange={(e) => setVarPassword(e.target.value)}
                autoComplete="new-password"
                placeholder={mode === "edit" ? t("projects.password_unchanged_hint") : ""}
              />
            </label>
          </fieldset>

          {submitErr && (
            <div
              style={{
                fontSize: 13,
                color: "var(--red-text)",
                marginBottom: 12,
                padding: "8px 10px",
                background: "var(--red-bg)",
                borderRadius: "var(--r-xs)",
                border: "1px solid var(--red-border)",
              }}
            >
              {submitErr}
            </div>
          )}
          <div style={{ display: "flex", gap: 10, justifyContent: "flex-end", marginTop: 8 }}>
            <button type="button" className="btn btn-secondary" onClick={onClose} disabled={saving}>
              {t("projects.cancel")}
            </button>
            <button type="submit" className="btn btn-primary" disabled={saving}>
              {saving ? "…" : mode === "create" ? t("projects.create_btn") : t("projects.save_btn")}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
