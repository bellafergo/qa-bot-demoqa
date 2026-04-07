// src/context/ThemeContext.jsx
// Preference: dark | light | system — persisted in localStorage.
// Resolved theme on <html data-theme="dark|light"> drives CSS variables.
import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";

export const THEME_STORAGE_KEY = "vanya-theme-preference";

/** @typedef {"dark" | "light" | "system"} ThemePreference */

function readStoredPreference() {
  try {
    const raw = localStorage.getItem(THEME_STORAGE_KEY);
    if (raw === "dark" || raw === "light" || raw === "system") return raw;
  } catch {
    /* ignore */
  }
  return "dark";
}

function resolvePreference(pref) {
  if (pref === "dark" || pref === "light") return pref;
  if (typeof window === "undefined" || !window.matchMedia) return "dark";
  return window.matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}

function applyDomTheme(resolved) {
  if (typeof document === "undefined") return;
  document.documentElement.setAttribute("data-theme", resolved);
}

const ThemeContext = createContext(null);

export function ThemeProvider({ children }) {
  const [preference, setPreferenceState] = useState(readStoredPreference);
  const [resolvedTheme, setResolvedTheme] = useState(() =>
    typeof window !== "undefined" ? resolvePreference(readStoredPreference()) : "dark",
  );

  useEffect(() => {
    const r = resolvePreference(preference);
    setResolvedTheme(r);
    applyDomTheme(r);
  }, [preference]);

  useEffect(() => {
    if (preference !== "system") return undefined;
    const mq = window.matchMedia("(prefers-color-scheme: dark)");
    const onChange = () => {
      const r = mq.matches ? "dark" : "light";
      setResolvedTheme(r);
      applyDomTheme(r);
    };
    mq.addEventListener("change", onChange);
    return () => mq.removeEventListener("change", onChange);
  }, [preference]);

  const setPreference = useCallback((next) => {
    const v = next === "light" || next === "system" ? next : "dark";
    setPreferenceState(v);
    try {
      localStorage.setItem(THEME_STORAGE_KEY, v);
    } catch {
      /* ignore */
    }
    const r = resolvePreference(v);
    setResolvedTheme(r);
    applyDomTheme(r);
  }, []);

  const value = useMemo(
    () => ({
      preference,
      setPreference,
      resolvedTheme,
    }),
    [preference, setPreference, resolvedTheme],
  );

  return (
    <ThemeContext.Provider value={value}>{children}</ThemeContext.Provider>
  );
}

export function useTheme() {
  const ctx = useContext(ThemeContext);
  if (!ctx) {
    throw new Error("useTheme must be used within ThemeProvider");
  }
  return ctx;
}
