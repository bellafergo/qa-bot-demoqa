// src/auth/AuthContext.jsx
import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import { useNavigate } from "react-router-dom";
import { supabase, supabaseConfigured } from "../lib/supabaseClient.js";
import {
  completeSsoAuthCallback,
  getSsoAuthLoginUrl,
  setAccessTokenGetter,
  setUnauthorizedHandler,
} from "../api.js";

export const SSO_SESSION_STORAGE_KEY = "vanya_sso_session";
export const SSO_PROVIDER_PENDING_KEY = "vanya_pending_sso_provider";

function readStoredSsoSession() {
  try {
    const raw = localStorage.getItem(SSO_SESSION_STORAGE_KEY);
    if (!raw) return null;
    const parsed = JSON.parse(raw);
    if (!parsed?.access_token) return null;
    return parsed;
  } catch {
    return null;
  }
}

const AuthContext = createContext(null);

export function AuthProvider({ children }) {
  const [session, setSession] = useState(null);
  const [loading, setLoading] = useState(true);
  const navigate = useNavigate();
  const signingOutRef = useRef(false);
  const sessionRef = useRef(null);
  sessionRef.current = session;

  const handleUnauthorized = useCallback(() => {
    if (signingOutRef.current) return;
    signingOutRef.current = true;
    if (supabase) {
      supabase.auth.signOut().catch(() => {});
    }
    localStorage.removeItem(SSO_SESSION_STORAGE_KEY);
    setSession(null);
    navigate("/login", { replace: true });
    signingOutRef.current = false;
  }, [navigate]);

  useLayoutEffect(() => {
    setAccessTokenGetter(() => sessionRef.current?.access_token ?? null);
    return () => setAccessTokenGetter(() => null);
  }, []);

  useEffect(() => {
    setUnauthorizedHandler(handleUnauthorized);
    return () => setUnauthorizedHandler(null);
  }, [handleUnauthorized]);

  useEffect(() => {
    const storedSso = readStoredSsoSession();
    if (storedSso) {
      setSession(storedSso);
      setLoading(false);
      return undefined;
    }

    if (!supabase) {
      setLoading(false);
      return undefined;
    }

    supabase.auth.getSession().then(({ data }) => {
      setSession(data.session ?? null);
      setLoading(false);
    });
    const { data: sub } = supabase.auth.onAuthStateChange((_event, sess) => {
      if (sess) {
        localStorage.removeItem(SSO_SESSION_STORAGE_KEY);
      }
      setSession(sess ?? null);
    });
    return () => sub.subscription.unsubscribe();
  }, []);

  const signInWithGoogle = useCallback(async () => {
    if (!supabase) return { error: new Error("Supabase not configured") };
    const redirectTo = `${window.location.origin}/auth/callback`;
    const { error } = await supabase.auth.signInWithOAuth({
      provider: "google",
      options: { redirectTo },
    });
    return { error };
  }, []);

  const signInWithEnterpriseSso = useCallback(async (provider) => {
    try {
      const data = await getSsoAuthLoginUrl(provider);
      if (!data?.login_url) {
        return { error: new Error("SSO login URL was not returned.") };
      }
      sessionStorage.setItem(SSO_PROVIDER_PENDING_KEY, String(provider).toUpperCase());
      window.location.assign(data.login_url);
      return { error: null };
    } catch (err) {
      return { error: err };
    }
  }, []);

  const completeEnterpriseSsoCallback = useCallback(async (provider, code, state) => {
    const data = await completeSsoAuthCallback(provider, code, state);
    const shaped = {
      access_token: data.access_token,
      user: {
        id: data.user_id,
        email: null,
      },
      authMethod: "sso",
      provider: data.provider_type,
      session_id: data.session_id,
    };
    localStorage.setItem(SSO_SESSION_STORAGE_KEY, JSON.stringify(shaped));
    sessionStorage.removeItem(SSO_PROVIDER_PENDING_KEY);
    setSession(shaped);
    return shaped;
  }, []);

  const signOut = useCallback(async () => {
    if (supabase) await supabase.auth.signOut();
    localStorage.removeItem(SSO_SESSION_STORAGE_KEY);
    sessionStorage.removeItem(SSO_PROVIDER_PENDING_KEY);
    setSession(null);
    navigate("/login", { replace: true });
  }, [navigate]);

  const value = useMemo(
    () => ({
      session,
      user: session?.user ?? null,
      loading,
      supabaseConfigured,
      authMethod: session?.authMethod || (session ? "local" : null),
      signInWithGoogle,
      signInWithEnterpriseSso,
      completeEnterpriseSsoCallback,
      signOut,
    }),
    [session, loading, signInWithGoogle, signInWithEnterpriseSso, completeEnterpriseSsoCallback, signOut],
  );

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}

export function useAuth() {
  const ctx = useContext(AuthContext);
  if (!ctx) throw new Error("useAuth must be used within AuthProvider");
  return ctx;
}
