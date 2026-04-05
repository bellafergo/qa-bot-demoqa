// src/lib/projectSlug.js
/** Backend slug: ^[a-z0-9][a-z0-9_-]{0,62}$ */

const SLUG_RE = /^[a-z0-9][a-z0-9_-]{0,62}$/;

export function suggestProjectIdFromName(name) {
  let s = String(name || "")
    .toLowerCase()
    .trim()
    .replace(/\s+/g, "-")
    .replace(/[^a-z0-9_-]/g, "")
    .replace(/^[^a-z0-9]+/, "");
  if (!s) s = "project";
  return s.slice(0, 63);
}

export function isValidProjectSlug(id) {
  return SLUG_RE.test(String(id || "").trim().toLowerCase());
}
