// src/context/ToastContext.jsx
import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import { useLang } from "../i18n/LangContext.jsx";
import { setApiErrorNotifier } from "../api.js";

const ToastContext = createContext(null);

export function ToastProvider({ children }) {
  const { t } = useLang();
  const [toasts, setToasts] = useState([]);

  const showToast = useCallback((message, variant = "error") => {
    if (message == null || String(message).trim() === "") return;
    const id = `${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    setToasts((prev) => [...prev, { id, message: String(message), variant }]);
    window.setTimeout(() => {
      setToasts((prev) => prev.filter((x) => x.id !== id));
    }, 6500);
  }, []);

  useEffect(() => {
    setApiErrorNotifier(({ status, detail }) => {
      const msg =
        (detail && String(detail).trim()) ||
        (status === 403
          ? t("http.forbidden")
          : status === 429
            ? t("http.rate_limit")
            : status >= 500
              ? t("http.server_error")
              : t("http.error", { status }));
      showToast(msg, status === 429 ? "warning" : "error");
    });
    return () => setApiErrorNotifier(null);
  }, [t, showToast]);

  const value = useMemo(() => ({ showToast }), [showToast]);

  return (
    <ToastContext.Provider value={value}>
      {children}
      <div
        role="status"
        aria-live="polite"
        style={{
          position: "fixed",
          bottom: 20,
          right: 20,
          zIndex: 10050,
          display: "flex",
          flexDirection: "column",
          gap: 10,
          maxWidth: "min(420px, calc(100vw - 32px))",
          pointerEvents: "none",
        }}
      >
        {toasts.map((toast) => (
          <div
            key={toast.id}
            className={
              toast.variant === "success"
                ? "alert"
                : toast.variant === "warning"
                  ? "alert alert-warn"
                  : "alert alert-error"
            }
            style={{
              pointerEvents: "auto",
              margin: 0,
              boxShadow: "0 10px 40px rgba(0,0,0,0.18)",
              fontSize: 13,
              lineHeight: 1.45,
              ...(toast.variant === "success"
                ? {
                    borderColor: "var(--green-border)",
                    background: "var(--green-bg)",
                    color: "var(--green-text)",
                  }
                : {}),
            }}
          >
            {toast.message}
          </div>
        ))}
      </div>
    </ToastContext.Provider>
  );
}

// Hook colocated with provider (HMR limitation is acceptable here).
// eslint-disable-next-line react-refresh/only-export-components -- useToast must live next to ToastProvider
export function useToast() {
  const ctx = useContext(ToastContext);
  if (!ctx) {
    return { showToast: () => {} };
  }
  return ctx;
}
