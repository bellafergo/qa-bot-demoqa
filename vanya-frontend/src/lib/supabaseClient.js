// src/lib/supabaseClient.js
import { createClient } from "@supabase/supabase-js";

const url = (import.meta.env.VITE_SUPABASE_URL || "").trim();
const anon = (import.meta.env.VITE_SUPABASE_ANON_KEY || "").trim();

export const supabaseConfigured = Boolean(url && anon);

/** Singleton browser client — PKCE + URL session detection for OAuth return. */
export const supabase = supabaseConfigured
  ? createClient(url, anon, {
      auth: {
        flowType: "pkce",
        detectSessionInUrl: true,
        persistSession: true,
        autoRefreshToken: true,
      },
    })
  : null;
