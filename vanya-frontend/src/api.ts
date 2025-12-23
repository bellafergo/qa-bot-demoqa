export const API_BASE = "https://qa-bot-demoqa.onrender.com";

export async function apiGet<T>(path: string): Promise<T> {
  const r = await fetch(`${API_BASE}${path}`);
  if (!r.ok) throw new Error(await r.text());
  return r.json();
}

export async function apiPost<T>(path: string, body?: any): Promise<T> {
  const r = await fetch(`${API_BASE}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: body ? JSON.stringify(body) : "{}",
  });
  if (!r.ok) throw new Error(await r.text());
  return r.json();
}