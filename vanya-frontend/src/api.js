// src/api.js (o vanya-frontend/src/api.js)

// ✅ Soporta ambos nombres de env y deja fallback seguro
export const API_BASE =
  (import.meta?.env?.VITE_API_BASE_URL || "").trim() ||
  (import.meta?.env?.VITE_API_BASE || "").trim() ||
  "https://qa-bot-demoqa.onrender.com";

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

// ✅ Retry SOLO para errores típicos de cold start / gateway / red
async function fetchWithRetry(url, options, retries = 2) {
  let lastErr;
  for (let i = 0; i <= retries; i++) {
    try {
      const res = await fetch(url, options);

      // gateway/cold start: reintenta con backoff corto
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

// ✅ Normaliza errores del backend
function pickErrorMessage({ res, text, json }) {
  // intenta detail/message primero
  const detail =
    (json && (json.detail || json.message || json.error)) ||
    text ||
    `${res.status} ${res.statusText}`;

  // recorta para que no reviente UI con HTML gigante
  return String(detail).slice(0, 4000);
}

export async function apiGet(path) {
  const res = await fetchWithRetry(`${API_BASE}${path}`, {
    method: "GET",
    headers: {
      Accept: "application/json",
    },
  });

  const { text, json } = await safeReadBody(res);

  if (!res.ok) {
    throw new Error(pickErrorMessage({ res, text, json }));
  }

  // Si no vino JSON (raro), regresamos null para que el caller lo maneje
  return json;
}

export const deleteThread = (id) =>
  fetchWithRetry(`${API_BASE}/threads/${id}`, { method: "DELETE" }).then(async (res) => {
    const { text, json } = await safeReadBody(res);
    if (!res.ok) {
      const detail = (json && (json.detail || json.message)) || text || `${res.status} ${res.statusText}`;
      throw new Error(detail);
    }
    return json;
  });

export async function apiPost(path, body = {}) {
  const res = await fetchWithRetry(`${API_BASE}${path}`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Accept: "application/json",
    },
    body: JSON.stringify(body ?? {}),
  });

  const { text, json } = await safeReadBody(res);

  if (!res.ok) {
    throw new Error(pickErrorMessage({ res, text, json }));
  }

  return json;
}

// ========= Convenience functions =========

// GET /threads -> suele regresar array directo
export const listThreads = async () => {
  const data = await apiGet("/threads");
  return Array.isArray(data) ? data : data?.threads || [];
};

// POST /threads -> regresa {id} o {thread_id}
export const createThread = () => apiPost("/threads", {});

// GET /threads/{id}
export const getThread = (id) => apiGet(`/threads/${id}`);

// POST /chat_run
// Mantengo tu firma, headless true por default, y permite override vía extra
export const chatRun = (prompt, thread_id, extra = {}) =>
  apiPost("/chat_run", {
    prompt,
    thread_id,
    headless: extra?.headless ?? true,
    ...extra,
  });