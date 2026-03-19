# services/db/draft_repository.py
"""
SQLAlchemy ORM model and repository for persistent draft tests.

Drafts live in the `drafts` table within vanya.db (same SQLite file as the
test catalog), following the exact same pattern as catalog_repository.py.

Lifecycle:
  draft → (edited, ai-suggested) → approved  (→ test_cases table)
                                 → discarded  (soft-delete, kept for audit)
"""
from __future__ import annotations

import logging
import uuid
from datetime import datetime, timezone
from typing import List, Optional

from sqlalchemy import Column, DateTime, String, Text, JSON

from models.draft_models import Draft
from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.draft_repo")


# ── ORM model ─────────────────────────────────────────────────────────────────

def _utcnow() -> datetime:
    return datetime.now(timezone.utc)


def _new_id() -> str:
    return str(uuid.uuid4())[:8]


class DraftRow(Base):
    __tablename__ = "drafts"

    draft_id        = Column(String, primary_key=True, default=_new_id)
    name            = Column(String,  nullable=False)
    module          = Column(String,  nullable=False, default="unknown")
    rationale       = Column(Text,    default="")
    confidence      = Column(String,  default="medium")   # low | medium | high
    source          = Column(String,  default="manual")   # manual | coverage_gap | pr_analysis | ai
    status          = Column(String,  default="draft", index=True)  # draft | approved | discarded
    steps_json      = Column(JSON,    default=list)
    assertions_json = Column(JSON,    default=list)
    meta_json       = Column(JSON,    default=dict)
    created_at      = Column(DateTime(timezone=True), default=_utcnow)
    updated_at      = Column(DateTime(timezone=True), default=_utcnow, onupdate=_utcnow)


# ── Conversion ────────────────────────────────────────────────────────────────

def _to_draft(row: DraftRow) -> Draft:
    return Draft(
        draft_id   = row.draft_id,
        name       = row.name or "",
        module     = row.module or "unknown",
        rationale  = row.rationale or "",
        confidence = row.confidence or "medium",
        source     = row.source or "manual",
        status     = row.status or "draft",
        steps      = row.steps_json or [],
        assertions = row.assertions_json or [],
        meta       = row.meta_json or {},
        created_at = row.created_at,
        updated_at = row.updated_at,
    )


# ── Repository ────────────────────────────────────────────────────────────────

class DraftRepository:

    def list_drafts(self, status: Optional[str] = None) -> List[Draft]:
        with get_session() as s:
            q = s.query(DraftRow)
            if status:
                q = q.filter(DraftRow.status == status)
            rows = q.order_by(DraftRow.created_at.desc()).all()
            return [_to_draft(r) for r in rows]

    def get_draft(self, draft_id: str) -> Optional[Draft]:
        with get_session() as s:
            row = s.query(DraftRow).filter(DraftRow.draft_id == draft_id).first()
            return _to_draft(row) if row else None

    def create_draft(self, data: dict) -> Draft:
        with get_session() as s:
            row = DraftRow(
                draft_id        = _new_id(),
                name            = data.get("name", "Untitled"),
                module          = data.get("module", "unknown"),
                rationale       = data.get("rationale", ""),
                confidence      = data.get("confidence", "medium"),
                source          = data.get("source", "manual"),
                status          = "draft",
                steps_json      = data.get("steps", []),
                assertions_json = data.get("assertions", []),
                meta_json       = data.get("meta", {}),
                created_at      = _utcnow(),
                updated_at      = _utcnow(),
            )
            s.add(row)
            s.flush()
            return _to_draft(row)

    def update_draft(self, draft_id: str, data: dict) -> Optional[Draft]:
        with get_session() as s:
            row = s.query(DraftRow).filter(DraftRow.draft_id == draft_id).first()
            if not row:
                return None
            if data.get("name")       is not None: row.name            = data["name"]
            if data.get("module")     is not None: row.module          = data["module"]
            if data.get("rationale")  is not None: row.rationale       = data["rationale"]
            if data.get("confidence") is not None: row.confidence      = data["confidence"]
            if data.get("steps")      is not None: row.steps_json      = data["steps"]
            if data.get("assertions") is not None: row.assertions_json = data["assertions"]
            row.updated_at = _utcnow()
            s.flush()
            return _to_draft(row)

    def delete_draft(self, draft_id: str) -> bool:
        with get_session() as s:
            row = s.query(DraftRow).filter(DraftRow.draft_id == draft_id).first()
            if not row:
                return False
            s.delete(row)
            return True

    def create_drafts_batch(self, items: list) -> dict:
        """
        Save multiple drafts in a single call.
        Not atomic — each item is attempted independently.
        Returns { saved, errors, saved_count, error_count }.
        """
        saved, errors = [], []
        for i, data in enumerate(items):
            try:
                draft = self.create_draft(data)
                saved.append({"index": i, "name": data.get("name", ""), "draft_id": draft.draft_id})
            except Exception as exc:
                errors.append({"index": i, "name": data.get("name", ""), "error": str(exc)})
        return {
            "saved":       saved,
            "errors":      errors,
            "saved_count": len(saved),
            "error_count": len(errors),
        }

    def set_status(self, draft_id: str, status: str) -> Optional[Draft]:
        with get_session() as s:
            row = s.query(DraftRow).filter(DraftRow.draft_id == draft_id).first()
            if not row:
                return None
            row.status     = status
            row.updated_at = _utcnow()
            s.flush()
            return _to_draft(row)


# ── Singleton ─────────────────────────────────────────────────────────────────

draft_repo = DraftRepository()
