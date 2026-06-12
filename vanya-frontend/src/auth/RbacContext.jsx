// src/auth/RbacContext.jsx
import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import { getSecurityMe } from "../api.js";
import { useAuth } from "./AuthContext.jsx";

const RbacContext = createContext(null);

export function RbacProvider({ children }) {
  const { session } = useAuth();
  const [context, setContext] = useState(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (!session) {
      setContext(null);
      return;
    }
    let cancelled = false;
    setLoading(true);
    getSecurityMe()
      .then((data) => {
        if (!cancelled) setContext(data);
      })
      .catch(() => {
        if (!cancelled) setContext(null);
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });
    return () => { cancelled = true; };
  }, [session]);

  const permissions = useMemo(
    () => new Set(context?.permissions || []),
    [context],
  );

  const hasPermission = useCallback(
    (permission) => {
      if (!context?.enforcement_enabled) return true;
      if (permissions.has("ADMIN")) return true;
      return permissions.has(permission);
    },
    [context, permissions],
  );

  const canWrite = useCallback(() => {
    if (!context?.enforcement_enabled) return true;
    return context?.role_name !== "VIEWER";
  }, [context]);

  const value = useMemo(
    () => ({
      loading,
      roleName: context?.role_name || null,
      email: context?.email || null,
      permissions: context?.permissions || [],
      enforcementEnabled: Boolean(context?.enforcement_enabled),
      hasPermission,
      canWrite,
    }),
    [loading, context, hasPermission, canWrite],
  );

  return (
    <RbacContext.Provider value={value}>
      {children}
    </RbacContext.Provider>
  );
}

export function useRbac() {
  const ctx = useContext(RbacContext);
  if (!ctx) {
    return {
      loading: false,
      roleName: null,
      email: null,
      permissions: [],
      enforcementEnabled: false,
      hasPermission: () => true,
      canWrite: () => true,
    };
  }
  return ctx;
}
