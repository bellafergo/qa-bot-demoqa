import { useEffect, useState, useRef } from "react";
import { apiGet, apiPost } from "../api";

export default function Chat({ threadId }) {
  const [thread, setThread] = useState(null);
  const [input, setInput] = useState("");
  const [loading, setLoading] = useState(false);
  const bottomRef = useRef(null);

  useEffect(() => {
    if (!threadId) {
      setThread(null);
      return;
    }

    apiGet(`/threads/${threadId}`).then(setThread);
  }, [threadId]);

  async function send() {
    if (!input.trim() || !threadId) return;

    setLoading(true);

    await apiPost("/chat_run", {
      prompt: input,
      thread_id: threadId,
      headless: true,
    });

    setInput("");

    const updated = await apiGet(`/threads/${threadId}`);
    setThread(updated);
    setLoading(false);

    setTimeout(() => bottomRef.current?.scrollIntoView(), 50);
  }

  if (!threadId) {
    return <div style={{ padding: 20 }}>Selecciona un chat</div>;
  }

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column" }}>
      <div style={{ flex: 1, overflowY: "auto", padding: 16 }}>
        {thread?.messages?.map((m, i) => (
          <div
            key={i}
            style={{
              textAlign: m.role === "user" ? "right" : "left",
              marginBottom: 12,
            }}
          >
            <div
              style={{
                display: "inline-block",
                padding: 10,
                borderRadius: 8,
                background: m.role === "user" ? "#e6f0ff" : "#f4f4f4",
              }}
            >
              <b>{m.role}</b>
              <div>{m.content}</div>
            </div>
          </div>
        ))}
        <div ref={bottomRef} />
      </div>

      <div style={{ display: "flex", padding: 12, gap: 8 }}>
        <input
          value={input}
          onChange={(e) => setInput(e.target.value)}
          placeholder="Escribe aquí…"
          style={{ flex: 1 }}
          onKeyDown={(e) => e.key === "Enter" && send()}
        />
        <button onClick={send} disabled={loading}>
          {loading ? "..." : "Enviar"}
        </button>
      </div>
    </div>
  );
}