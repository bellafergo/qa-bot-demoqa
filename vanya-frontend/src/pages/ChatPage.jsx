// src/pages/ChatPage.jsx
/**
 * ChatPage - Contains the full chat logic extracted from the original App.jsx
 * This maintains all existing functionality with the thread sidebar + chat UI.
 */
import React, {
  useEffect,
  useRef,
  useState,
  useCallback,
  useMemo,
} from "react";
import {
  chatRun,
  getThread,
  listThreads,
  createThread as apiCreateThread,
  deleteThread as apiDeleteThread,
} from "../api";
import Sidebar from "../components/Sidebar";
import Chat from "../chat";

// ---- API guardrails ----
const safeFn = (fn, name) => {
  if (typeof fn !== "function") {
    throw new Error(`API function "${name}" is not a function`);
  }
  return fn;
};

const safeListThreads = safeFn(listThreads, "listThreads");
const safeGetThread = safeFn(getThread, "getThread");
const safeChatRun = safeFn(chatRun, "chatRun");
const safeCreateThread = safeFn(apiCreateThread, "createThread");
const safeDeleteThread = safeFn(apiDeleteThread, "deleteThread");

// ---- Helpers ----
const escapeHtml = (s) => {
  if (s == null) return "";
  return String(s)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
};

const formatText = (text) => {
  if (typeof text !== "string") return "";
  const safe = escapeHtml(text);
  return safe.replace(/\*\*(.*?)\*\*/g, "<b>$1</b>").replace(/\n/g, "<br/>");
};

const shortId = (id) => (id ? `${String(id).slice(0, 8)}…` : "");

export default function ChatPage() {
  // ---- UI state ----
  const [messages, setMessages] = useState([]);
  const [input, setInput] = useState("");
  const [uiError, setUiError] = useState("");

  // Busy flags
  const [isSending, setIsSending] = useState(false);
  const [isThreadsLoading, setIsThreadsLoading] = useState(false);
  const sidebarBusy = isSending || isThreadsLoading;

  // ---- Session + Thread state ----
  const [sessionId, setSessionId] = useState(() => {
    try {
      return localStorage.getItem("vanya_session_id") || null;
    } catch {
      return null;
    }
  });

  const [threadId, setThreadId] = useState(() => {
    try {
      return localStorage.getItem("vanya_thread_id") || null;
    } catch {
      return null;
    }
  });

  const [threads, setThreads] = useState([]);
  const [isSidebarOpen, setIsSidebarOpen] = useState(true);

  const chatEndRef = useRef(null);

  const scrollToBottom = useCallback(() => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, []);

  useEffect(() => {
    scrollToBottom();
  }, [messages, scrollToBottom]);

  const setWelcome = useCallback(() => {
    setMessages([
      {
        role: "bot",
        content:
          "Hola, soy **Vanya**, tu Agente de QA inteligente. ¿En qué puedo ayudarte hoy con tus pruebas?",
        meta: { mode: "welcome" },
      },
    ]);
  }, []);

  // ---- Threads helpers ----
  const normalizeThreads = useCallback((list) => {
    const arr = Array.isArray(list) ? list : [];
    const cleaned = arr
      .map((t) => ({
        ...t,
        id: t?.id || t?.thread_id || null,
      }))
      .filter((t) => !!t.id);

    cleaned.sort((a, b) => {
      const da = a?.updated_at ? new Date(a.updated_at).getTime() : 0;
      const db = b?.updated_at ? new Date(b.updated_at).getTime() : 0;
      return db - da;
    });

    return cleaned;
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
      setUiError(
        `Failed to load threads. Detail: ${String(e?.message || e)}`
      );
      setThreads([]);
      return [];
    } finally {
      setIsThreadsLoading(false);
    }
  }, [normalizeThreads]);

  // ---- Map backend messages to UI ----
  const mapBackendMessagesToUI = useCallback((items) => {
    if (!Array.isArray(items) || items.length === 0) return null;

    const ui = [];
    for (const m of items) {
      const roleRaw = String(m?.role || m?.speaker || "").toLowerCase();
      if (roleRaw === "system") continue;

      const role =
        roleRaw === "assistant" || roleRaw === "bot" ? "bot" : "user";

      const content =
        typeof m?.content === "string"
          ? m.content
          : typeof m?.text === "string"
          ? m.text
          : m?.message && typeof m.message === "string"
          ? m.message
          : "";

      const meta = m?.meta ?? m?.meta_json ?? {};

      const hasRenderableMeta =
        !!meta?.runner ||
        !!meta?.docArtifacts ||
        !!meta?.artifacts ||
        !!meta?.mode;

      if (!content && !hasRenderableMeta) continue;

      ui.push({ role, content, meta });
    }

    return ui.length ? ui : null;
  }, []);

  // ---- Load one thread ----
  const loadThread = useCallback(
    async (id, { refreshSidebar = false } = {}) => {
      const tid = String(id || "").trim();
      if (!tid) return;

      setUiError("");
      setThreadId(tid);
      try {
        localStorage.setItem("vanya_thread_id", tid);
      } catch {}

      setIsThreadsLoading(true);
      try {
        const data = await safeGetThread(tid);

        const backendMsgs =
          data?.messages ||
          data?.items ||
          data?.history ||
          data?.thread?.messages;

        const ui = mapBackendMessagesToUI(backendMsgs);

        if (ui) setMessages(ui);
        else setWelcome();

        if (refreshSidebar) await refreshThreads().catch(() => {});
      } catch (e) {
        setUiError(
          `Failed to load chat. Detail: ${String(e?.message || e)}`
        );
        setWelcome();
      } finally {
        setIsThreadsLoading(false);
      }
    },
    [mapBackendMessagesToUI, refreshThreads, setWelcome]
  );

  // ---- Create thread ----
  const handleNew = useCallback(async () => {
    if (sidebarBusy) return;
    setUiError("");
    setIsThreadsLoading(true);
    try {
      const data = await safeCreateThread();
      const id = data?.id || data?.thread_id;
      if (!id)
        throw new Error("Backend did not return id/thread_id when creating thread.");

      setThreadId(id);
      try {
        localStorage.setItem("vanya_thread_id", id);
      } catch {}

      setWelcome();
      await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`Failed to create chat. Detail: ${String(e?.message || e)}`);
    } finally {
      setIsThreadsLoading(false);
    }
  }, [refreshThreads, setWelcome, sidebarBusy]);

  // ---- Delete thread ----
  const handleDelete = useCallback(
    async (id) => {
      if (sidebarBusy) return;
      const tid = String(id || "").trim();
      if (!tid) return;

      const ok = window.confirm("Delete this chat? This cannot be undone.");
      if (!ok) return;

      setUiError("");
      setIsThreadsLoading(true);
      try {
        await safeDeleteThread(tid);
        await refreshThreads();

        if (String(tid) === String(threadId)) {
          setThreadId(null);
          try {
            localStorage.removeItem("vanya_thread_id");
          } catch {}
          setWelcome();
        }
      } catch (e) {
        setUiError(`Failed to delete chat. Detail: ${String(e?.message || e)}`);
      } finally {
        setIsThreadsLoading(false);
      }
    },
    [refreshThreads, setWelcome, sidebarBusy, threadId]
  );

  // ---- Select thread ----
  const handleSelect = useCallback(
    async (id) => {
      if (sidebarBusy) return;
      await loadThread(id, { refreshSidebar: false });
    },
    [loadThread, sidebarBusy]
  );

  // ---- Send chat ----
  const handleSend = useCallback(async () => {
    const prompt = (input || "").trim();
    if (!prompt || isSending) return;

    setUiError("");
    setIsSending(true);

    setMessages((prev) => [...prev, { role: "user", content: prompt, meta: {} }]);
    setInput("");

    try {
      const resp = await safeChatRun(prompt, threadId || null, {
        session_id: sessionId,
      });

      const newThreadId = resp?.thread_id || resp?.threadId || threadId || null;
      const newSessionId = resp?.session_id || resp?.sessionId || sessionId || null;

      if (newThreadId && String(newThreadId) !== String(threadId)) {
        setThreadId(newThreadId);
        try {
          localStorage.setItem("vanya_thread_id", newThreadId);
        } catch {}
      }

      if (newSessionId && String(newSessionId) !== String(sessionId)) {
        setSessionId(newSessionId);
        try {
          localStorage.setItem("vanya_session_id", newSessionId);
        } catch {}
      }

      const answer =
        typeof resp?.answer === "string"
          ? resp.answer
          : typeof resp?.text === "string"
          ? resp.text
          : "";

      setMessages((prev) => [
        ...prev,
        {
          role: "bot",
          content: answer || "Done.",
          meta: {
            ...(resp || {}),
            mode: resp?.mode || "chat_only",
            runner: resp?.runner || null,
          },
        },
      ]);

      await refreshThreads().catch(() => {});
    } catch (e) {
      setUiError(`Error sending message. Detail: ${String(e?.message || e)}`);
      setMessages((prev) => [
        ...prev,
        {
          role: "bot",
          content: "Error sending your message. Please try again.",
          meta: { mode: "error" },
        },
      ]);
    } finally {
      setIsSending(false);
    }
  }, [input, isSending, refreshThreads, sessionId, threadId]);

  // ---- Initial load ----
  useEffect(() => {
    (async () => {
      await refreshThreads().catch(() => {});
      if (threadId) {
        await loadThread(threadId).catch(() => setWelcome());
      } else {
        setWelcome();
      }
    })();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // ---- Error badge ----
  const errorBadge = useMemo(() => {
    if (!uiError) return null;
    return (
      <div
        style={{
          padding: "6px 10px",
          borderRadius: 10,
          background: "rgba(255,0,0,0.10)",
          border: "1px solid rgba(255,0,0,0.25)",
          fontSize: 12,
          maxWidth: 400,
          overflow: "hidden",
          textOverflow: "ellipsis",
          whiteSpace: "nowrap",
          color: "white",
        }}
        title={uiError}
      >
        {uiError}
      </div>
    );
  }, [uiError]);

  // ---- Layout ----
  return (
    <div style={{ display: "flex", height: "100%", width: "100%" }}>
      {/* Thread Sidebar */}
      <div
        style={{
          width: isSidebarOpen ? 280 : 0,
          overflow: "hidden",
          transition: "width 200ms ease",
          borderRight: isSidebarOpen
            ? "1px solid rgba(255,255,255,0.08)"
            : "none",
        }}
      >
        {isSidebarOpen && (
          <Sidebar
            threads={threads}
            activeId={threadId}
            onNew={handleNew}
            onSelect={handleSelect}
            onDelete={handleDelete}
            isLoading={sidebarBusy}
          />
        )}
      </div>

      {/* Chat Area */}
      <div
        style={{
          flex: 1,
          display: "flex",
          flexDirection: "column",
          minWidth: 0,
        }}
      >
        {/* Top bar */}
        <div
          style={{
            height: 48,
            padding: "0 12px",
            borderBottom: "1px solid rgba(255,255,255,0.08)",
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            gap: 10,
          }}
        >
          <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
            <button
              onClick={() => setIsSidebarOpen((s) => !s)}
              style={{
                width: 32,
                height: 32,
                borderRadius: 8,
                border: "1px solid rgba(255,255,255,0.12)",
                background: "rgba(0,0,0,0.25)",
                color: "white",
                cursor: "pointer",
                fontSize: 14,
              }}
              title={isSidebarOpen ? "Hide history" : "Show history"}
            >
              {isSidebarOpen ? "⟪" : "⟫"}
            </button>

            <span style={{ fontSize: 13, opacity: 0.7, color: "white" }}>
              {threadId ? `Thread: ${shortId(threadId)}` : "No thread selected"}
            </span>
          </div>

          {errorBadge}
        </div>

        {/* Chat component */}
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
          />
        </div>
      </div>
    </div>
  );
}
