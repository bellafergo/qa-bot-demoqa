-- =============================================================================
-- Vanya — Supabase / PostgreSQL schema (derived from repo usage)
-- =============================================================================
-- Run in Supabase → SQL Editor → paste → Run.
--
-- SCOPE
--   Part 1: Tablas usadas por el cliente Supabase (PostgREST) en el backend.
--            Sin esto, check_supabase_health() falla al leer `threads` y
--            /meta devuelve supabase_ok: false aunque las credenciales sean válidas.
--   Part 2 (opcional): Paridad con SQLAlchemy en services/db/* que hoy vive en
--            SQLite (VANYA_SQLITE_PATH). Solo créalas si migrás el catálogo a
--            esta misma base; el backend NO las usa en Supabase por defecto.
--
-- NOTAS SQLite → Postgres
--   - JSON en SQLAlchemy (drafts) → JSONB aquí.
--   - Timestamps: drafts usa DateTime(timezone=True) → TIMESTAMPTZ.
--   - test_cases / test_runs guardan muchas fechas como TEXT ISO (igual que SQLite).
--   - document_chunks.embedding: el código envía vectores como array; JSONB evita
--     depender de la extensión pgvector. Para búsqueda vectorial nativa, migrá a
--     tipo vector(n) y habilitá la extensión en Supabase.
--   - Bucket de Storage `documents`: crear en Dashboard (Storage) si usás RAG;
--     este script no crea buckets.
--
-- =============================================================================
-- PART 1 — Tablas requeridas para Supabase (API Python: supabase_store, etc.)
-- =============================================================================

CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Chats: services/supabase_store.py (health check lee esta tabla primero)
CREATE TABLE IF NOT EXISTS public.threads (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  title      TEXT NOT NULL DEFAULT 'New chat',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_threads_updated_at ON public.threads (updated_at DESC);

-- Mensajes: columna `meta` (JSONB), no `meta_json` (ese nombre es solo en db.py SQLite)
CREATE TABLE IF NOT EXISTS public.messages (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  thread_id  UUID NOT NULL REFERENCES public.threads (id) ON DELETE CASCADE,
  role       TEXT NOT NULL,
  content    TEXT NOT NULL DEFAULT '',
  meta       JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_messages_thread_id ON public.messages (thread_id);
CREATE INDEX IF NOT EXISTS idx_messages_thread_created ON public.messages (thread_id, created_at DESC);

-- Runs ligados al hilo (runner JSON); distinto de test_runs del catálogo SQLite
CREATE TABLE IF NOT EXISTS public.runs (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  thread_id  UUID NOT NULL REFERENCES public.threads (id) ON DELETE CASCADE,
  status     TEXT NOT NULL DEFAULT 'unknown',
  runner     JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_runs_thread_id ON public.runs (thread_id);

-- RAG: services/document_store.py
CREATE TABLE IF NOT EXISTS public.documents (
  id             UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  workspace_id   TEXT,
  thread_id      TEXT,
  filename       TEXT NOT NULL,
  content_type   TEXT,
  size_bytes     BIGINT,
  storage_bucket TEXT NOT NULL,
  storage_path   TEXT NOT NULL,
  sha256         TEXT,
  tags           JSONB NOT NULL DEFAULT '[]'::jsonb,
  source         TEXT,
  raw_text       TEXT,
  extracted_at   TEXT,
  created_at     TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_documents_thread_id ON public.documents (thread_id);
CREATE INDEX IF NOT EXISTS idx_documents_sha256 ON public.documents (sha256);

CREATE TABLE IF NOT EXISTS public.document_chunks (
  id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  doc_id       UUID NOT NULL REFERENCES public.documents (id) ON DELETE CASCADE,
  chunk_index  INTEGER NOT NULL DEFAULT 0,
  text         TEXT NOT NULL DEFAULT '',
  char_start   INTEGER,
  char_end     INTEGER,
  tokens_est   INTEGER,
  embedding    JSONB,
  created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_document_chunks_doc_id ON public.document_chunks (doc_id);

-- Auditoría de ejecuciones QA: services/run_store_supabase.py (upsert on_conflict evidence_id)
CREATE TABLE IF NOT EXISTS public.qa_runs (
  evidence_id       TEXT PRIMARY KEY,
  status            TEXT NOT NULL DEFAULT 'failed',
  duration_ms       INTEGER,
  mode              TEXT,
  persona           TEXT,
  domain            TEXT,
  flow              TEXT,
  tags              JSONB NOT NULL DEFAULT '[]'::jsonb,
  error_summary     TEXT,
  failed_step_index INTEGER,
  steps_count       INTEGER,
  evidence_url      TEXT,
  report_url        TEXT,
  meta              JSONB NOT NULL DEFAULT '{}'::jsonb,
  result            JSONB NOT NULL DEFAULT '{}'::jsonb,
  git_sha           TEXT,
  runner_version    TEXT,
  updated_at        TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_qa_runs_status ON public.qa_runs (status);

-- Canonical run_id (application identity) — GET /runs/{run_id} + Evidence Library
-- evidence_id remains the table PK for legacy upserts; run_id is the stable execution id.
ALTER TABLE public.qa_runs ADD COLUMN IF NOT EXISTS run_id TEXT;

UPDATE public.qa_runs r
SET run_id = COALESCE(
  NULLIF(btrim(r.run_id), ''),
  NULLIF(btrim(r.result->>'run_id'), ''),
  NULLIF(btrim(r.meta->>'run_id'), ''),
  r.evidence_id
)
WHERE r.run_id IS NULL OR btrim(COALESCE(r.run_id, '')) = '';

CREATE UNIQUE INDEX IF NOT EXISTS uq_qa_runs_run_id
  ON public.qa_runs (run_id)
  WHERE run_id IS NOT NULL AND btrim(run_id) <> '';

-- Memoria por dominio: services/memory_store.py (upsert on_conflict domain)
CREATE TABLE IF NOT EXISTS public.domain_memory (
  domain TEXT PRIMARY KEY,
  data   JSONB NOT NULL DEFAULT '{}'::jsonb
);

-- =============================================================================
-- PART 2 — OPCIONAL: catálogo / jobs (paridad ORM en services/db/*, SQLite hoy)
-- =============================================================================
-- El proceso en Render suele usar ./data/vanya.db para estas tablas.
-- Crear aquí solo si configurás el engine SQLAlchemy contra este Postgres.

CREATE TABLE IF NOT EXISTS public.test_cases (
  id               TEXT PRIMARY KEY,
  test_case_id     TEXT NOT NULL UNIQUE,
  name             TEXT NOT NULL,
  module           TEXT,
  type             TEXT,
  priority         TEXT,
  status           TEXT NOT NULL DEFAULT 'active',
  test_type        TEXT NOT NULL DEFAULT 'ui',
  project_id       TEXT NOT NULL DEFAULT 'default',
  version          INTEGER NOT NULL DEFAULT 1,
  tags_json        TEXT NOT NULL DEFAULT '[]',
  base_url         TEXT,
  steps_json       TEXT NOT NULL DEFAULT '[]',
  assertions_json  TEXT NOT NULL DEFAULT '[]',
  created_from     TEXT,
  source_run_id    TEXT,
  created_at       TEXT NOT NULL,
  updated_at     TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_test_cases_status ON public.test_cases (status);
CREATE INDEX IF NOT EXISTS idx_test_cases_project_id ON public.test_cases (project_id);

-- Paridad con SQLite: tests creados desde run (dedupe por source_run_id)
ALTER TABLE public.test_cases ADD COLUMN IF NOT EXISTS created_from TEXT;
ALTER TABLE public.test_cases ADD COLUMN IF NOT EXISTS source_run_id TEXT;

CREATE UNIQUE INDEX IF NOT EXISTS uq_test_cases_source_run_id
  ON public.test_cases (source_run_id)
  WHERE source_run_id IS NOT NULL AND btrim(source_run_id) <> '';

CREATE INDEX IF NOT EXISTS idx_test_cases_source_run_id_lookup ON public.test_cases (source_run_id);

CREATE TABLE IF NOT EXISTS public.test_runs (
  run_id             TEXT PRIMARY KEY,
  test_case_id       TEXT NOT NULL,
  test_name          TEXT,
  executed_at        TEXT NOT NULL,
  environment        TEXT,
  status             TEXT NOT NULL,
  duration_ms        INTEGER,
  evidence_url       TEXT,
  report_url         TEXT,
  evidence_id        TEXT,
  logs_json          TEXT NOT NULL DEFAULT '[]',
  steps_result_json  TEXT NOT NULL DEFAULT '[]',
  meta_json          TEXT NOT NULL DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_test_runs_test_case_id ON public.test_runs (test_case_id);

CREATE TABLE IF NOT EXISTS public.orchestrator_jobs (
  job_id              TEXT PRIMARY KEY,
  job_type            TEXT NOT NULL,
  status              TEXT NOT NULL,
  test_case_ids_json  TEXT NOT NULL DEFAULT '[]',
  total_count         INTEGER NOT NULL DEFAULT 0,
  completed_count     INTEGER NOT NULL DEFAULT 0,
  passed_count        INTEGER NOT NULL DEFAULT 0,
  failed_count        INTEGER NOT NULL DEFAULT 0,
  error_count         INTEGER NOT NULL DEFAULT 0,
  run_ids_json        TEXT NOT NULL DEFAULT '[]',
  results_json        TEXT NOT NULL DEFAULT '[]',
  error_message       TEXT,
  environment         TEXT,
  created_at          TEXT NOT NULL,
  started_at          TEXT,
  finished_at         TEXT,
  retry_count         INTEGER NOT NULL DEFAULT 0,
  skipped_count       INTEGER NOT NULL DEFAULT 0,
  scheduling_notes    TEXT,
  context_json        TEXT,
  parent_job_id       TEXT
);

CREATE TABLE IF NOT EXISTS public.projects (
  id            TEXT PRIMARY KEY,
  name          TEXT NOT NULL,
  description   TEXT NOT NULL DEFAULT '',
  color         TEXT NOT NULL DEFAULT '#6366f1',
  base_url      TEXT,
  settings_json TEXT,
  created_at    TEXT NOT NULL,
  updated_at    TEXT NOT NULL
);

-- Existing Supabase projects created before settings_json: add column idempotently
ALTER TABLE public.projects ADD COLUMN IF NOT EXISTS settings_json TEXT;

CREATE TABLE IF NOT EXISTS public.drafts (
  draft_id         TEXT PRIMARY KEY,
  name             TEXT NOT NULL,
  module           TEXT NOT NULL DEFAULT 'unknown',
  rationale        TEXT NOT NULL DEFAULT '',
  confidence       TEXT NOT NULL DEFAULT 'medium',
  source           TEXT NOT NULL DEFAULT 'manual',
  status           TEXT NOT NULL DEFAULT 'draft',
  steps_json       JSONB NOT NULL DEFAULT '[]'::jsonb,
  assertions_json  JSONB NOT NULL DEFAULT '[]'::jsonb,
  meta_json        JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at       TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_drafts_status ON public.drafts (status);

CREATE TABLE IF NOT EXISTS public.test_versions (
  id               TEXT PRIMARY KEY,
  test_case_id     TEXT NOT NULL,
  version_number   INTEGER NOT NULL,
  name             TEXT NOT NULL DEFAULT '',
  module           TEXT NOT NULL DEFAULT '',
  type             TEXT NOT NULL DEFAULT 'smoke',
  priority         TEXT NOT NULL DEFAULT 'medium',
  status           TEXT NOT NULL DEFAULT 'active',
  test_type        TEXT NOT NULL DEFAULT 'ui',
  steps_json       TEXT NOT NULL DEFAULT '[]',
  assertions_json  TEXT NOT NULL DEFAULT '[]',
  tags_json        TEXT NOT NULL DEFAULT '[]',
  base_url         TEXT,
  source           TEXT NOT NULL DEFAULT 'manual',
  change_note      TEXT NOT NULL DEFAULT '',
  created_at       TEXT NOT NULL,
  CONSTRAINT uq_test_versions_tc_ver UNIQUE (test_case_id, version_number)
);

CREATE INDEX IF NOT EXISTS idx_test_versions_test_case_id ON public.test_versions (test_case_id);

-- =============================================================================
-- PostgREST: exponer tablas al API (schema public ya está en API por defecto)
-- =============================================================================
-- Si alguna tabla no aparece en el cliente, en Supabase: Database → Tables
-- y verificá que esté en schema `public`. Service role bypass RLS; para `anon`
-- harían falta políticas (fuera de alcance de este script).
