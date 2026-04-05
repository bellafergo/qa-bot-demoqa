// src/i18n/LangContext.jsx
// Minimal i18n context — no external library required.
// Provides: lang, setLang, t(key)
// Persists language choice to localStorage under "vanya_lang".

import React, { createContext, useContext, useState, useCallback } from "react";
import { translations, DEFAULT_LANG, SUPPORTED_LANGS } from "./translations";

const LangContext = createContext(null);

const STORAGE_KEY = "vanya_lang";

function getStoredLang() {
  try {
    const v = localStorage.getItem(STORAGE_KEY);
    return SUPPORTED_LANGS.includes(v) ? v : DEFAULT_LANG;
  } catch {
    return DEFAULT_LANG;
  }
}

export function LangProvider({ children }) {
  const [lang, setLangState] = useState(getStoredLang);

  const setLang = useCallback((l) => {
    if (!SUPPORTED_LANGS.includes(l)) return;
    setLangState(l);
    try {
      localStorage.setItem(STORAGE_KEY, l);
    } catch {
      void 0;
    }
  }, []);

  // t(key, vars?) — optional {{placeholders}} replaced from vars
  const t = useCallback((key, vars) => {
    let s =
      (translations[lang] || {})[key] ||
      (translations[DEFAULT_LANG] || {})[key] ||
      key;
    if (vars && typeof s === "string") {
      for (const [k, v] of Object.entries(vars)) {
        s = s.split(`{{${k}}}`).join(String(v));
      }
    }
    return s;
  }, [lang]);

  return (
    <LangContext.Provider value={{ lang, setLang, t }}>
      {children}
    </LangContext.Provider>
  );
}

/* eslint-disable react-refresh/only-export-components -- hook is tied to LangProvider in this module */
export function useLang() {
  const ctx = useContext(LangContext);
  if (!ctx) throw new Error("useLang must be used inside LangProvider");
  return ctx;
}
