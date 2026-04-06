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
  setAccessTokenGetter,
  setUnauthorizedHandler,
} from "../api.js";

const AuthContext = createContext(null);

export function AuthProvider({ children }) {
  const [session, setSession] = useState(null);
  const [loading, setLoading] = useState(true);
  const navigate = useNavigate();
  const signingOutRef = useRef(false);
  /** Always read in api.js token getter — avoids races vs child useEffect (e.g. ProjectProvider). */
  const sessionRef = useRef(null);
  sessionRef.current = session;

  const handleUnauthorized = useCallback(() => {
    if (signingOutRef.current) return;
    signingOutRef.current = true;
    if (supabase) {
      supabase.auth.signOut().catch(() => {});
    }
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
    if (!supabase) {
      setLoading(false);
      return;
    }
    supabase.auth.getSession().then(({ data }) => {
      setSession(data.session ?? null);
      setLoading(false);
    });
    const { data: sub } = supabase.auth.onAuthStateChange((_event, sess) => {
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

  const signOut = useCallback(async () => {
    if (supabase) await supabase.auth.signOut();
    setSession(null);
    navigate("/login", { replace: true });
  }, [navigate]);

  const value = useMemo(
    () => ({
      session,
      user: session?.user ?? null,
      loading,
      supabaseConfigured,
      signInWithGoogle,
      signOut,
    }),
    [session, loading, signInWithGoogle, signOut],
  );

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}

export function useAuth() {
  const ctx = useContext(AuthContext);
  if (!ctx) throw new Error("useAuth must be used within AuthProvider");
  return ctx;
}
