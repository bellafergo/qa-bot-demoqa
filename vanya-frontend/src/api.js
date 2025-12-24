// src/api.js
const API_BASE =
  (import.meta.env.VITE_API_BASE_URL || "").trim() ||
  (import.meta.env.VITE_API_BASE || "").trim() ||
  "https://qa-bot-demoqa.onrender.com"; // fallback seguro

async function safeReadBody(res) {
  const text = await res.text();
  if (!text) return { text: "", json: null };
  try {
    return { text, json: JSON.parse(text) };
  } catch {
    return { text, json: null };
  }
}

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

// Retry solo para errores típicos de cold start / gateway
async function fetchWithRetry(url, options, retries = 2) {
  let lastErr;
  for (let i = 0; i <= retries; i++) {
    try {
      const res = await fetch(url, options);
      if ([502, 503, 504].includes(res.status) && i < retries) {
        await sleep(600 * (i + 1));
        continue;
      }
      return res;
    } catch (e) {
      lastErr = e;
      if (i < retries) {
        await sleep(600 * (i + 1));
        continue;
      }
      throw lastErr;
    }
  }
  throw lastErr || new Error("fetch failed");
}

async function handleJsonOrThrow(res) {
  const { text, json } = await safeReadBody(res);
  if (!res.ok) {
    const detail =
      (json && (json.detail || json.message)) ||
      text ||
      `${res.status} ${res.statusText}`;
    throw new Error(detail);
  }
  return json;
}

export async function apiGet(path) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "GET",
    headers: { "Content-Type": "application/json" },
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

export async function apiDelete(path) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "DELETE",
    headers: { "Content-Type": "application/json" },
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

export async function apiPost(path, body) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body || {}),
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

// ========= Convenience =========
export const listThreads = () => apiGet("/threads");
export const createThread = () => apiPost("/threads", {});
export const getThread = (id) => apiGet(`/threads/${id}`);
export const deleteThread = (id) => apiDelete(`/threads/${id}`);

export const chatRun = (prompt, thread_id, extra = {}) =>
  apiPost("/chat_run", { prompt, thread_id, headless: true, ...extra });