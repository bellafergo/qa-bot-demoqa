const API_BASE =
  (import.meta.env.VITE_API_BASE_URL || "").trim() ||
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
      // si es gateway/cold start, reintenta
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

export async function apiGet(path) {
  const res = await fetchWithRetry(`${API_BASE}${path}`, {
    method: "GET",
  });

  const { text, json } = await safeReadBody(res);

  if (!res.ok) {
    // intenta mostrar el detail del backend si existe
    const detail =
      (json && (json.detail || json.message)) ||
      text ||
      `${res.status} ${res.statusText}`;
    throw new Error(detail);
  }

  return json; // en GET normalmente siempre hay JSON
}

export async function apiPost(path, body = {}) {
  const res = await fetchWithRetry(`${API_BASE}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    // si body es null -> manda {}
    body: JSON.stringify(body ?? {}),
  });

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

// ========= Convenience functions =========
export const listThreads = () => apiGet("/threads");

// POST /threads realmente no requiere body
export const createThread = () => apiPost("/threads", {});

export const getThread = (id) => apiGet(`/threads/${id}`);

// Mantengo tu firma, pero agrego thread_id y headless por default
export const chatRun = (prompt, thread_id, extra = {}) =>
  apiPost("/chat_run", { prompt, thread_id, headless: true, ...extra });