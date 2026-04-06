// src/context/ProjectContext.jsx
import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import {
  listProjects as apiListProjects,
  createProject as apiCreateProject,
  updateProject as apiUpdateProject,
  deleteProject as apiDeleteProject,
  apiErrorMessage,
} from "../api.js";
import { useAuth } from "../auth/AuthContext.jsx";
import { PROJECT_STORAGE_KEY } from "./projectStorage.js";

/** @typedef {{ id: string, name: string, description?: string, color?: string, base_url?: string|null, created_at?: string, updated_at?: string }} Project */

const ProjectContext = createContext(null);

function pickCurrentAfterLoad(list, preferId, previousId, storedId) {
  if (preferId) {
    const hit = list.find((p) => p.id === preferId);
    if (hit) return hit;
  }
  if (previousId) {
    const hit = list.find((p) => p.id === previousId);
    if (hit) return hit;
  }
  if (storedId) {
    const hit = list.find((p) => p.id === storedId);
    if (hit) return hit;
  }
  if (list.length > 0) return list[0];
  return null;
}

export function ProjectProvider({ children }) {
  const { session, loading: authLoading } = useAuth();
  /** @type {[Project[], function]} */
  const [projects, setProjects] = useState([]);
  /** @type {[Project | null, function]} */
  const [currentProject, setCurrentProjectState] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  const currentIdRef = useRef(null);
  useEffect(() => {
    currentIdRef.current = currentProject?.id ?? null;
  }, [currentProject?.id]);

  const persistId = useCallback((id) => {
    if (id) {
      try {
        localStorage.setItem(PROJECT_STORAGE_KEY, id);
      } catch {
        /* ignore */
      }
    } else {
      try {
        localStorage.removeItem(PROJECT_STORAGE_KEY);
      } catch {
        /* ignore */
      }
    }
  }, []);

  const setCurrentProject = useCallback(
    (project) => {
      setCurrentProjectState(project ?? null);
      persistId(project?.id ?? null);
    },
    [persistId],
  );

  /** @param {string} [preferIdAfter] — e.g. newly created project id */
  const reloadProjects = useCallback(async (preferIdAfter) => {
    setLoading(true);
    setError(null);
    let stored = "";
    try {
      stored = (localStorage.getItem(PROJECT_STORAGE_KEY) || "").trim();
    } catch {
      stored = "";
    }
    try {
      const raw = await apiListProjects();
      const listNorm = Array.isArray(raw) ? raw : [];
      setProjects(listNorm);
      setError(null);

      const prev = currentIdRef.current;
      const next = pickCurrentAfterLoad(
        listNorm,
        preferIdAfter || null,
        prev,
        stored,
      );
      setCurrentProjectState(next);
      persistId(next?.id ?? null);
    } catch (e) {
      const msg = apiErrorMessage(e);
      setError(msg);
      setProjects([]);
      setCurrentProjectState(null);
      persistId(null);
    } finally {
      setLoading(false);
    }
  }, [persistId]);

  useEffect(() => {
    if (authLoading) return;
    if (!session?.access_token) {
      setLoading(false);
      setProjects([]);
      setCurrentProjectState(null);
      persistId(null);
      setError(null);
      return;
    }
    reloadProjects();
  }, [authLoading, session?.access_token, reloadProjects, persistId]);

  const createProject = useCallback(
    async (payload) => {
      const created = await apiCreateProject(payload);
      await reloadProjects(created?.id || null);
      return created;
    },
    [reloadProjects],
  );

  const updateProject = useCallback(
    async (projectId, payload) => {
      const updated = await apiUpdateProject(projectId, payload);
      await reloadProjects();
      return updated;
    },
    [reloadProjects],
  );

  const deleteProject = useCallback(
    async (projectId) => {
      await apiDeleteProject(projectId);
      await reloadProjects();
    },
    [reloadProjects],
  );

  const value = useMemo(
    () => ({
      projects,
      currentProject,
      setCurrentProject,
      reloadProjects,
      createProject,
      updateProject,
      deleteProject,
      loading,
      error,
    }),
    [
      projects,
      currentProject,
      setCurrentProject,
      reloadProjects,
      createProject,
      updateProject,
      deleteProject,
      loading,
      error,
    ],
  );

  return <ProjectContext.Provider value={value}>{children}</ProjectContext.Provider>;
}

/* eslint-disable react-refresh/only-export-components -- hook is tied to ProjectProvider in this module */
export function useProject() {
  const ctx = useContext(ProjectContext);
  if (!ctx) {
    throw new Error("useProject must be used within ProjectProvider");
  }
  return ctx;
}
