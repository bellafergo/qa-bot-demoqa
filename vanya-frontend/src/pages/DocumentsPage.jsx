// src/pages/DocumentsPage.jsx
/**
 * DocumentsPage — Upload documents for RAG context + semantic search.
 * POST /documents/upload  |  GET /documents/query
 */
import React, { useState, useCallback } from "react";
import { useLang } from "../i18n/LangContext";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

export default function DocumentsPage() {
  const { t } = useLang();
  const [file, setFile]           = useState(null);
  const [threadId, setThreadId]   = useState("");
  const [tags, setTags]           = useState("");
  const [uploading, setUploading] = useState(false);
  const [uploadResult, setUploadResult] = useState(null);
  const [uploadError, setUploadError]   = useState("");

  const [query, setQuery]               = useState("");
  const [queryThreadId, setQueryThreadId] = useState("");
  const [querying, setQuerying]         = useState(false);
  const [snippets, setSnippets]         = useState([]);
  const [queryError, setQueryError]     = useState("");

  const handleFileChange = (e) => {
    const f = e.target.files?.[0] || null;
    setFile(f);
    setUploadResult(null);
    setUploadError("");
  };

  const handleUpload = useCallback(async () => {
    if (!file) return;
    setUploading(true);
    setUploadError("");
    setUploadResult(null);
    try {
      const formData = new FormData();
      formData.append("file", file);
      if (threadId.trim()) formData.append("thread_id", threadId.trim());
      if (tags.trim())     formData.append("tags", tags.trim());
      const res = await fetch(`${API_BASE}/documents/upload`, { method: "POST", body: formData });
      const data = await res.json();
      if (!res.ok) {
        setUploadError(data?.detail || data?.error || `HTTP ${res.status}`);
      } else {
        setUploadResult(data);
        setFile(null);
        const fi = document.getElementById("doc-file-input");
        if (fi) fi.value = "";
      }
    } catch (e) {
      setUploadError(e?.message || "Network error");
    } finally { setUploading(false); }
  }, [file, threadId, tags]);

  const handleQuery = useCallback(async () => {
    if (!query.trim()) return;
    setQuerying(true);
    setQueryError("");
    setSnippets([]);
    try {
      const params = new URLSearchParams({ query: query.trim() });
      if (queryThreadId.trim()) params.append("thread_id", queryThreadId.trim());
      const res = await fetch(`${API_BASE}/documents/query?${params}`, { method: "GET" });
      const data = await res.json();
      if (!res.ok) {
        setQueryError(data?.detail || data?.error || `HTTP ${res.status}`);
      } else {
        setSnippets(data?.snippets || []);
        if (!data?.snippets?.length) setQueryError("No matching documents found.");
      }
    } catch (e) {
      setQueryError(e?.message || "Network error");
    } finally { setQuerying(false); }
  }, [query, queryThreadId]);

  return (
    <div className="page-wrap">
      {/* ── Page header ─────────────────────────────────── */}
      <div className="page-header">
        <h1 className="page-title">{t("docs.title")}</h1>
        <p className="page-subtitle">{t("docs.subtitle")}</p>
      </div>

      {/* ── Upload card ─────────────────────────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("docs.upload.title")}</div>

        <div style={{ display: "grid", gap: 12 }}>
          {/* File picker */}
          <label style={{
            display: "flex",
            alignItems: "center",
            gap: 12,
            padding: "12px 14px",
            border: "2px dashed var(--border)",
            borderRadius: "var(--r-sm)",
            cursor: "pointer",
            background: "var(--surface-2)",
            transition: "border-color 0.15s",
          }}
          onMouseEnter={e => e.currentTarget.style.borderColor = "var(--accent)"}
          onMouseLeave={e => e.currentTarget.style.borderColor = "var(--border)"}
          >
            <span style={{ fontSize: 20 }}>📄</span>
            <div style={{ flex: 1, minWidth: 0 }}>
              <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text)" }}>
                {file ? file.name : t("docs.upload.choose")}
              </div>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>
                {file ? `${(file.size / 1024).toFixed(1)} KB` : t("docs.upload.formats")}
              </div>
            </div>
            <input
              id="doc-file-input"
              type="file"
              accept=".pdf,.txt,.docx,.md"
              onChange={handleFileChange}
              style={{ display: "none" }}
            />
          </label>

          {/* Optional fields */}
          <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
            <input
              className="input"
              value={threadId}
              onChange={e => setThreadId(e.target.value)}
              placeholder={t("docs.upload.thread_ph")}
              style={{ flex: 1, minWidth: 160 }}
            />
            <input
              className="input"
              value={tags}
              onChange={e => setTags(e.target.value)}
              placeholder={t("docs.upload.tags_ph")}
              style={{ flex: 1, minWidth: 160 }}
            />
          </div>

          <div>
            <button
              className="btn btn-primary"
              onClick={handleUpload}
              disabled={uploading || !file}
            >
              {uploading ? t("docs.upload.uploading") : t("docs.upload.btn")}
            </button>
          </div>
        </div>

        {uploadError && (
          <div className="alert alert-error" style={{ marginTop: 12 }}>{uploadError}</div>
        )}
        {uploadResult?.ok && (
          <div className="alert alert-success" style={{ marginTop: 12 }}>
            {t("docs.upload.success")}
            {uploadResult.doc?.filename && <span> — <strong>{uploadResult.doc.filename}</strong></span>}
            {uploadResult.doc?.chunks   && <span> ({uploadResult.doc.chunks} chunks)</span>}
          </div>
        )}
      </div>

      {/* ── Query card ──────────────────────────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("docs.search.title")}</div>

        <div style={{ display: "flex", gap: 10, flexWrap: "wrap", marginBottom: 10 }}>
          <input
            className="input"
            value={query}
            onChange={e => setQuery(e.target.value)}
            onKeyDown={e => e.key === "Enter" && handleQuery()}
            placeholder={t("docs.search.placeholder")}
            style={{ flex: 2, minWidth: 200 }}
          />
          <input
            className="input"
            value={queryThreadId}
            onChange={e => setQueryThreadId(e.target.value)}
            placeholder={t("docs.search.thread_ph")}
            style={{ flex: 1, minWidth: 140 }}
          />
          <button
            className="btn btn-secondary"
            onClick={handleQuery}
            disabled={querying || !query.trim()}
          >
            {querying ? t("docs.search.searching") : t("docs.search.btn")}
          </button>
        </div>

        {queryError && (
          <div className="alert alert-warn" style={{ marginTop: 8 }}>{queryError}</div>
        )}
      </div>

      {/* ── Results ─────────────────────────────────────── */}
      {snippets.length > 0 && (
        <div>
          <div style={{
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            marginBottom: 12,
          }}>
            <div className="section-title" style={{ margin: 0 }}>
              {t("docs.results.title")}
            </div>
            <span className="badge badge-accent">{snippets.length} {snippets.length !== 1 ? t("docs.results.snippets") : t("docs.results.snippet")}</span>
          </div>

          <div style={{ display: "grid", gap: 12 }}>
            {snippets.map((s, i) => (
              <div className="card" key={i} style={{ padding: "14px 16px" }}>
                <div style={{
                  display: "flex",
                  justifyContent: "space-between",
                  alignItems: "flex-start",
                  marginBottom: 8,
                  gap: 8,
                }}>
                  <div style={{ fontWeight: 700, fontSize: 13, color: "var(--text)" }}>
                    {s.filename || t("docs.results.unknown")}
                  </div>
                  <div style={{ display: "flex", gap: 6, flexShrink: 0 }}>
                    {s.chunk_index !== undefined && (
                      <span className="badge badge-gray">chunk #{s.chunk_index}</span>
                    )}
                    {s.score !== undefined && (
                      <span className="badge badge-accent">{(s.score * 100).toFixed(0)}% match</span>
                    )}
                  </div>
                </div>
                <p style={{
                  margin: 0,
                  fontSize: 13,
                  lineHeight: 1.6,
                  color: "var(--text-2)",
                  whiteSpace: "pre-wrap",
                }}>
                  {s.text?.slice(0, 500)}{s.text?.length > 500 ? "…" : ""}
                </p>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}
