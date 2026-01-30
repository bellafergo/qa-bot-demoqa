// src/pages/DocumentsPage.jsx
/**
 * DocumentsPage - Document upload and list view
 *
 * Uses the backend endpoints:
 * - POST /documents/upload
 * - GET /documents/query
 * - GET /documents/list (if available)
 */
import React, { useState, useCallback } from "react";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

export default function DocumentsPage() {
  const [file, setFile] = useState(null);
  const [threadId, setThreadId] = useState("");
  const [tags, setTags] = useState("");

  const [uploading, setUploading] = useState(false);
  const [uploadResult, setUploadResult] = useState(null);
  const [uploadError, setUploadError] = useState("");

  // Query state
  const [query, setQuery] = useState("");
  const [queryThreadId, setQueryThreadId] = useState("");
  const [querying, setQuerying] = useState(false);
  const [snippets, setSnippets] = useState([]);
  const [queryError, setQueryError] = useState("");

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
      if (threadId.trim()) {
        formData.append("thread_id", threadId.trim());
      }
      if (tags.trim()) {
        formData.append("tags", tags.trim());
      }

      const res = await fetch(`${API_BASE}/documents/upload`, {
        method: "POST",
        body: formData,
      });

      const data = await res.json();
      if (!res.ok) {
        setUploadError(data?.detail || data?.error || `HTTP ${res.status}`);
      } else {
        setUploadResult(data);
        setFile(null);
        // Reset file input
        const fileInput = document.getElementById("doc-file-input");
        if (fileInput) fileInput.value = "";
      }
    } catch (e) {
      setUploadError(e?.message || "Network error");
    } finally {
      setUploading(false);
    }
  }, [file, threadId, tags]);

  const handleQuery = useCallback(async () => {
    if (!query.trim()) return;

    setQuerying(true);
    setQueryError("");
    setSnippets([]);

    try {
      const params = new URLSearchParams({ query: query.trim() });
      if (queryThreadId.trim()) {
        params.append("thread_id", queryThreadId.trim());
      }

      const res = await fetch(`${API_BASE}/documents/query?${params}`, {
        method: "GET",
      });

      const data = await res.json();
      if (!res.ok) {
        setQueryError(data?.detail || data?.error || `HTTP ${res.status}`);
      } else {
        setSnippets(data?.snippets || []);
        if (!data?.snippets?.length) {
          setQueryError("No matching documents found.");
        }
      }
    } catch (e) {
      setQueryError(e?.message || "Network error");
    } finally {
      setQuerying(false);
    }
  }, [query, queryThreadId]);

  return (
    <div style={{ padding: 20, maxWidth: 900, margin: "0 auto" }}>
      <h2 style={{ marginBottom: 16, fontWeight: 800, color: "white" }}>
        Documents
      </h2>

      <p style={{ marginBottom: 24, opacity: 0.75, color: "white", fontSize: 14 }}>
        Upload documents (PDF, TXT, DOCX) to use as context in chat conversations.
        Vanya uses RAG to find relevant information from your documents.
      </p>

      {/* Upload Section */}
      <section
        style={{
          padding: 20,
          borderRadius: 12,
          border: "1px solid rgba(255,255,255,0.12)",
          background: "rgba(0,0,0,0.2)",
          marginBottom: 24,
        }}
      >
        <h3 style={{ margin: "0 0 16px", fontWeight: 700, color: "white" }}>
          Upload Document
        </h3>

        <div style={{ display: "grid", gap: 12 }}>
          <input
            id="doc-file-input"
            type="file"
            accept=".pdf,.txt,.docx,.md"
            onChange={handleFileChange}
            style={{
              padding: 10,
              borderRadius: 10,
              border: "1px solid rgba(255,255,255,0.14)",
              background: "rgba(0,0,0,0.25)",
              color: "white",
            }}
          />

          <div style={{ display: "flex", gap: 12, flexWrap: "wrap" }}>
            <input
              value={threadId}
              onChange={(e) => setThreadId(e.target.value)}
              placeholder="Thread ID (optional)"
              style={{
                flex: 1,
                minWidth: 180,
                padding: "10px 12px",
                borderRadius: 10,
                border: "1px solid rgba(255,255,255,0.14)",
                background: "rgba(0,0,0,0.25)",
                color: "white",
                outline: "none",
              }}
            />

            <input
              value={tags}
              onChange={(e) => setTags(e.target.value)}
              placeholder="Tags (comma-separated)"
              style={{
                flex: 1,
                minWidth: 180,
                padding: "10px 12px",
                borderRadius: 10,
                border: "1px solid rgba(255,255,255,0.14)",
                background: "rgba(0,0,0,0.25)",
                color: "white",
                outline: "none",
              }}
            />
          </div>

          <button
            onClick={handleUpload}
            disabled={uploading || !file}
            style={{
              padding: "10px 20px",
              borderRadius: 10,
              border: "1px solid rgba(78,107,255,0.4)",
              background: "rgba(78,107,255,0.3)",
              color: "white",
              cursor: uploading || !file ? "not-allowed" : "pointer",
              fontWeight: 700,
              opacity: !file ? 0.6 : 1,
            }}
          >
            {uploading ? "Uploading..." : "Upload"}
          </button>
        </div>

        {/* Upload error */}
        {uploadError && (
          <div
            style={{
              marginTop: 12,
              padding: "10px 14px",
              borderRadius: 10,
              background: "rgba(255,60,60,0.15)",
              border: "1px solid rgba(255,60,60,0.3)",
              color: "#ff6b6b",
              fontSize: 13,
            }}
          >
            {uploadError}
          </div>
        )}

        {/* Upload success */}
        {uploadResult?.ok && (
          <div
            style={{
              marginTop: 12,
              padding: "10px 14px",
              borderRadius: 10,
              background: "rgba(82,196,26,0.15)",
              border: "1px solid rgba(82,196,26,0.3)",
              color: "#52c41a",
              fontSize: 13,
            }}
          >
            Document uploaded successfully!
            {uploadResult.doc?.filename && (
              <span style={{ marginLeft: 8 }}>
                ({uploadResult.doc.filename})
              </span>
            )}
            {uploadResult.doc?.chunks && (
              <span style={{ marginLeft: 8 }}>
                - {uploadResult.doc.chunks} chunks created
              </span>
            )}
          </div>
        )}
      </section>

      {/* Query Section */}
      <section
        style={{
          padding: 20,
          borderRadius: 12,
          border: "1px solid rgba(255,255,255,0.12)",
          background: "rgba(0,0,0,0.2)",
        }}
      >
        <h3 style={{ margin: "0 0 16px", fontWeight: 700, color: "white" }}>
          Search Documents
        </h3>

        <div style={{ display: "grid", gap: 12 }}>
          <div style={{ display: "flex", gap: 12, flexWrap: "wrap" }}>
            <input
              value={query}
              onChange={(e) => setQuery(e.target.value)}
              placeholder="Search query..."
              onKeyDown={(e) => e.key === "Enter" && handleQuery()}
              style={{
                flex: 2,
                minWidth: 200,
                padding: "10px 12px",
                borderRadius: 10,
                border: "1px solid rgba(255,255,255,0.14)",
                background: "rgba(0,0,0,0.25)",
                color: "white",
                outline: "none",
              }}
            />

            <input
              value={queryThreadId}
              onChange={(e) => setQueryThreadId(e.target.value)}
              placeholder="Thread ID (optional)"
              style={{
                flex: 1,
                minWidth: 150,
                padding: "10px 12px",
                borderRadius: 10,
                border: "1px solid rgba(255,255,255,0.14)",
                background: "rgba(0,0,0,0.25)",
                color: "white",
                outline: "none",
              }}
            />
          </div>

          <button
            onClick={handleQuery}
            disabled={querying || !query.trim()}
            style={{
              padding: "10px 20px",
              borderRadius: 10,
              border: "1px solid rgba(78,107,255,0.4)",
              background: "rgba(78,107,255,0.3)",
              color: "white",
              cursor: querying || !query.trim() ? "not-allowed" : "pointer",
              fontWeight: 700,
              opacity: !query.trim() ? 0.6 : 1,
              width: "fit-content",
            }}
          >
            {querying ? "Searching..." : "Search"}
          </button>
        </div>

        {/* Query error */}
        {queryError && (
          <div
            style={{
              marginTop: 12,
              padding: "10px 14px",
              borderRadius: 10,
              background: "rgba(255,165,0,0.15)",
              border: "1px solid rgba(255,165,0,0.3)",
              color: "#ffa500",
              fontSize: 13,
            }}
          >
            {queryError}
          </div>
        )}

        {/* Snippets */}
        {snippets.length > 0 && (
          <div style={{ marginTop: 16 }}>
            <h4 style={{ margin: "0 0 12px", fontWeight: 700, color: "white" }}>
              Results ({snippets.length})
            </h4>

            <div style={{ display: "grid", gap: 12 }}>
              {snippets.map((s, i) => (
                <div
                  key={i}
                  style={{
                    padding: 14,
                    borderRadius: 10,
                    border: "1px solid rgba(255,255,255,0.1)",
                    background: "rgba(0,0,0,0.25)",
                  }}
                >
                  <div
                    style={{
                      display: "flex",
                      justifyContent: "space-between",
                      alignItems: "center",
                      marginBottom: 8,
                    }}
                  >
                    <span style={{ fontWeight: 700, color: "white", fontSize: 13 }}>
                      {s.filename || "Unknown file"}
                    </span>
                    {s.chunk_index !== undefined && (
                      <span style={{ fontSize: 11, opacity: 0.6, color: "white" }}>
                        Chunk #{s.chunk_index}
                      </span>
                    )}
                  </div>
                  <p
                    style={{
                      margin: 0,
                      fontSize: 13,
                      lineHeight: 1.5,
                      color: "white",
                      opacity: 0.85,
                      whiteSpace: "pre-wrap",
                    }}
                  >
                    {s.text?.slice(0, 500)}
                    {s.text?.length > 500 ? "..." : ""}
                  </p>
                </div>
              ))}
            </div>
          </div>
        )}
      </section>
    </div>
  );
}
