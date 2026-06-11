import React, { createContext, useCallback, useContext, useEffect, useMemo, useState } from "react";
import { getMyPermissions, apiErrorMessage } from "../api";
import { useAuth } from "../auth/AuthContext.jsx";

const PermissionsContext = createContext({
  permissions: [],
  roleName: "VIEWER",
  userId: "anonymous",
  loading: true,
  error: "",
  refresh: async () => {},
});

export function PermissionsProvider({ children }) {
  const { session } = useAuth();
  const [permissions, setPermissions] = useState([]);
  const [roleName, setRoleName] = useState("VIEWER");
  const [userId, setUserId] = useState("anonymous");
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const refresh = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const data = await getMyPermissions();
      setPermissions(data?.permissions || []);
      setRoleName(data?.role_name || "VIEWER");
      setUserId(data?.user_id || "anonymous");
    } catch (err) {
      setPermissions([]);
      setRoleName("VIEWER");
      setUserId("anonymous");
      setError(apiErrorMessage(err));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    refresh();
  }, [session, refresh]);

  const value = useMemo(
    () => ({ permissions, roleName, userId, loading, error, refresh }),
    [permissions, roleName, userId, loading, error, refresh],
  );

  return (
    <PermissionsContext.Provider value={value}>
      {children}
    </PermissionsContext.Provider>
  );
}

export function usePermissions() {
  return useContext(PermissionsContext);
}
