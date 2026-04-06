// src/pages/ChatPage.jsx
/**
 * ChatPage — thread sidebar (light) + dark chat canvas (preserves chat.jsx styles)
 */
import React, {
  useEffect, useRef, useState, useCallback,
} from "react";
import {
  apiErrorMessage,
  chatRun, getThread, listThreads,
  createThread as apiCreateThread,
  deleteThread as apiDeleteThread,
} from "../api";
import Sidebar from "../components/Sidebar";
import Chat from "../chat";
import ConfirmDialog from "../components/ConfirmDialog.jsx";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";

// ---- API guardrails ----
const safeFn = (fn, name) => {
  if (typeof fn !== "function") throw new Error(`API function "${name}" is not a function`);
  return fn;
};
const safeListThreads   = safeFn(listThreads,   "listThreads");
const safeGetThread     = safeFn(getThread,     "getThread");
const safeChatRun       = safeFn(chatRun,       "chatRun");
const safeCreateThread  = safeFn(apiCreateThread, "createThread");
const safeDeleteThread  = safeFn(apiDeleteThread, "deleteThread");

// ---- Helpers ----
const escapeHtml = (s) => {
  if (s == null) return "";
  return String(s)
    .replaceAll("&", "&amp;").replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;").replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
};
const formatText = (text) => {
  if (typeof text !== "string") return "";
  return escapeHtml(text)
    .replace(/\*\*(.*?)\*\*/g, "<b>$1</b>")
    .replace(/\n/g, "<br/>");
};
const shortId = (id) => (id ? `${String(id).slice(0, 8)}…` : "");

export default function ChatPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const [messages, setMessages] = useState([]);
  const [input, setInput]       = useState("");
  const [uiError, setUiError]   = useState("");
  const [isSending, setIsSending]           = useState(false);
  const [isThreadsLoading, setIsThreadsLoading] = useState(false);
  const sidebarBusy = isSending || isThreadsLoading;

  const [sessionId, setSessionId] = useState(() => {
    try { return localStorage.getItem("vanya_session_id") || null; } catch { return null; }
  });
  const [threadId, setThreadId] = useState(() => {
    try { return localStorage.getItem("vanya_thread_id") || null; } catch { return null; }
  });
  const [threads, setThreads]         = useState([]);
  const [isSidebarOpen, setIsSidebarOpen] = useState(true);
  const [deleteThreadId, setDeleteThreadId] = useState(null);
  const [deleteBusy, setDeleteBusy] = useState(false);

  const chatEndRef = useRef(null);
  const scrollToBottom = useCallback(() => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, []);
  useEffect(() => { scrollToBottom(); }, [messages, scrollToBottom]);

  const setWelcome = useCallback(() => {
    setMessages([{
      role: "bot",
      content: t("chat.welcome"),
      meta: { mode: "welcome" },
    }]);
  }, [t]);

  const normalizeThreads = useCallback((list) => {
    const arr = Array.isArray(list) ? list : [];
    return arr
      .map(t => ({ ...t, id: t?.id || t?.thread_id || null }))
      .filter(t => !!t.id)
      .sort((a, b) => {
        const da = a?.updated_at ? new Date(a.updated_at).getTime() : 0;
        const db = b?.updated_at ? new Date(b.updated_at).getTime() : 0;
        return db - da;
      });
  }, []);

  const refreshThreads = useCallback(async () => {
    setIsThreadsLoading(true);
    setUiError("");
    try {
      const list = await safeListThreads();
      const normalized = normalizeThreads(list || []);
      setThreads(normalized);
      return normalized;
    } catch (e) {
      setUiError(`${t("chat.error.load_threads")} ${apiErrorMessage(e)}`);
      setThreads([]);
      return [];
    } finally { setIsThreadsLoading(false); }
  }, [normalizeThreads, t]);

  const mapBackendMessagesToUI = useCallback((items) => {
    if (!Array.isArray(items) || items.length === 0) return null;
    const ui = [];
    for (const m of items) {
      const roleRaw = String(m?.role || m?.speaker || "").toLowerCase();
      if (roleRaw === "system") continue;
      const role = roleRaw === "assistant" || roleRaw === "bot" ? "bot" : "user";
      const content =
        typeof m?.content === "string" ? m.content :
        typeof m?.text    === "string" ? m.text :
        m?.message && typeof m.message === "string" ? m.message : "";
      const meta = m?.meta ?? m?.meta_json ?? {};
      const hasRenderableMeta = !!meta?.runner || !!meta?.docArtifacts || !!meta?.artifacts || !!meta?.mode;
      if (!content && !hasRenderableMeta) continue;
      ui.push({ role, content, meta });
    }
    return ui.length ? ui : null;
  }, []);

  const loadThread = useCallback(async (id, { refreshSidebar = false } = {}) => {
    const tid = String(id || "").trim();
    if (!tid) return;
    setUiError("");
    setThreadId(tid);
    try { localStorage.setItem("vanya_thread_id", tid); } catch { /* storage unavailable */ }
    setIsThreadsLoading(true);
    try {
      const data = await safeGetThread(tid);
      const backendMsgs = data?.messages || data?.items || data?.history || data?.thread?.messages;
      const ui = mapBackendMessagesToUI(backendMsgs);
      if (ui) setMessages(ui); else setWelcome();
      if (refreshSidebar) await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`${t("chat.error.load_chat")} ${apiErrorMessage(e)}`);
      setWelcome();
    } finally { setIsThreadsLoading(false); }
  }, [mapBackendMessagesToUI, refreshThreads, setWelcome, t]);

  const handleNew = useCallback(async () => {
    if (sidebarBusy) return;
    setUiError("");
    setIsThreadsLoading(true);
    try {
      const data = await safeCreateThread();
      const id = data?.id || data?.thread_id;
      if (!id) throw new Error("Backend did not return id/thread_id when creating thread.");
      setThreadId(id);
      try { localStorage.setItem("vanya_thread_id", id); } catch { /* storage unavailable */ }
      setWelcome();
      await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`${t("chat.error.create_chat")} ${apiErrorMessage(e)}`);
    } finally { setIsThreadsLoading(false); }
  }, [refreshThreads, setWelcome, sidebarBusy, t]);

  const requestDeleteThread = useCallback((id) => {
    if (sidebarBusy) return;
    const tid = String(id || "").trim();
    if (!tid) return;
    setDeleteThreadId(tid);
  }, [sidebarBusy]);

  const confirmDeleteThread = useCallback(async () => {
    const tid = deleteThreadId;
    if (!tid || deleteBusy) return;
    setDeleteBusy(true);
    setUiError("");
    setIsThreadsLoading(true);
    try {
      await safeDeleteThread(tid);
      await refreshThreads();
      if (String(tid) === String(threadId)) {
        setThreadId(null);
        try { localStorage.removeItem("vanya_thread_id"); } catch { /* storage unavailable */ }
        setWelcome();
      }
      setDeleteThreadId(null);
    } catch (e) {
      setUiError(`${t("chat.error.delete_chat")} ${apiErrorMessage(e)}`);
    } finally {
      setDeleteBusy(false);
      setIsThreadsLoading(false);
    }
  }, [deleteBusy, deleteThreadId, refreshThreads, setWelcome, threadId, t]);

  const handleSelect = useCallback(async (id) => {
    if (sidebarBusy) return;
    await loadThread(id, { refreshSidebar: false });
  }, [loadThread, sidebarBusy]);

  const handleSend = useCallback(async () => {
    const prompt = (input || "").trim();
    if (!prompt || isSending) return;
    setUiError("");
    setIsSending(true);
    setMessages(prev => [...prev, { role: "user", content: prompt, meta: {} }]);
    setInput("");
    try {
      const resp = await safeChatRun(prompt, threadId || null, { session_id: sessionId });
      const newThreadId  = resp?.thread_id  || resp?.threadId  || threadId  || null;
      const newSessionId = resp?.session_id || resp?.sessionId || sessionId || null;
      if (newThreadId && String(newThreadId) !== String(threadId)) {
        setThreadId(newThreadId);
        try { localStorage.setItem("vanya_thread_id", newThreadId); } catch { /* storage unavailable */ }
      }
      if (newSessionId && String(newSessionId) !== String(sessionId)) {
        setSessionId(newSessionId);
        try { localStorage.setItem("vanya_session_id", newSessionId); } catch { /* storage unavailable */ }
      }
      const answer =
        typeof resp?.answer === "string" ? resp.answer :
        typeof resp?.text   === "string" ? resp.text : "";
      setMessages(prev => [...prev, {
        role: "bot",
        content: answer || t("chat.done"),
        meta: { ...(resp || {}), mode: resp?.mode || "chat_only", runner: resp?.runner || null },
      }]);
      await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`${t("chat.error.send")} ${apiErrorMessage(e)}`);
      setMessages(prev => [...prev, {
        role: "bot",
        content: t("chat.error.send_retry"),
        meta: { mode: "error" },
      }]);
    } finally { setIsSending(false); }
  }, [input, isSending, refreshThreads, sessionId, threadId, t]);

  useEffect(() => {
    (async () => {
      await refreshThreads().catch(() => {});
      if (threadId) await loadThread(threadId).catch(() => setWelcome());
      else setWelcome();
    })();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <div style={{ display: "flex", height: "100%", width: "100%", overflow: "hidden" }}>

      {/* ── Thread sidebar (light) ──────────────────────── */}
      <div style={{
        width: isSidebarOpen ? "var(--thread-w)" : 0,
        minWidth: isSidebarOpen ? "var(--thread-w)" : 0,
        overflow: "hidden",
        transition: "width 200ms ease, min-width 200ms ease",
        flexShrink: 0,
      }}>
        {isSidebarOpen && (
          <Sidebar
            threads={threads}
            activeId={threadId}
            onNew={handleNew}
            onSelect={handleSelect}
            onDelete={requestDeleteThread}
            isLoading={sidebarBusy}
          />
        )}
      </div>

      {/* ── Chat canvas (light) ────────────────────────────── */}
      <div style={{
        flex: 1,
        display: "flex",
        flexDirection: "column",
        minWidth: 0,
        background: "var(--chat-bg)",
      }}>
        {/* Chat toolbar */}
        <div style={{
          height: 46,
          padding: "0 16px",
          borderBottom: "1px solid var(--border)",
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          gap: 10,
          flexShrink: 0,
          background: "var(--surface)",
        }}>
          <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
            {/* Toggle sidebar */}
            <button
              onClick={() => setIsSidebarOpen(s => !s)}
              title={isSidebarOpen ? t("chat.toolbar.hide_history") : t("chat.toolbar.show_history")}
              style={{
                width: 28, height: 28,
                borderRadius: 6,
                border: "1px solid var(--border)",
                background: "var(--surface)",
                color: "var(--text-2)",
                cursor: "pointer",
                fontSize: 12,
                display: "flex", alignItems: "center", justifyContent: "center",
                flexShrink: 0,
              }}
            >
              {isSidebarOpen ? "⟪" : "⟫"}
            </button>

            <span style={{
              fontSize: 11,
              color: "var(--text-3)",
              fontFamily: "ui-monospace, monospace",
            }}>
              {threadId ? `thread:${shortId(threadId)}` : t("chat.toolbar.no_thread")}
            </span>
          </div>

          {/* Error badge */}
          {uiError && (
            <div className="alert alert-error" style={{
              padding: "4px 10px",
              fontSize: 12,
              maxWidth: 380,
              overflow: "hidden",
              textOverflow: "ellipsis",
              whiteSpace: "nowrap",
            }} title={uiError}>
              {uiError}
            </div>
          )}
        </div>

        {/* Chat messages + input (handled by chat.jsx) */}
        <div style={{ flex: 1, overflow: "auto" }}>
          <Chat
            key={threadId || "no-thread"}
            messages={Array.isArray(messages) ? messages : []}
            input={input}
            setInput={setInput}
            handleSend={handleSend}
            isLoading={isSending}
            sessionId={sessionId}
            threadId={threadId}
            formatText={formatText}
            chatEndRef={chatEndRef}
            projectId={currentProject?.id || "default"}
          />
        </div>
      </div>

      <ConfirmDialog
        open={!!deleteThreadId}
        title={t("chat.delete_confirm_title")}
        description={t("chat.delete_confirm")}
        confirmLabel={t("common.delete")}
        danger
        busy={deleteBusy}
        onCancel={() => !deleteBusy && setDeleteThreadId(null)}
        onConfirm={confirmDeleteThread}
      />
    </div>
  );
}
