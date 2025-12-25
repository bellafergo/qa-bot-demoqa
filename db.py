import os
import uuid
from datetime import datetime, timezone

from sqlalchemy import (
    create_engine,
    Column,
    String,
    Text,
    DateTime,
    ForeignKey,
    JSON,
)
from sqlalchemy.orm import declarative_base, sessionmaker, relationship

DATABASE_URL = os.getenv("DATABASE_URL", "").strip()
if not DATABASE_URL:
    DATABASE_URL = "sqlite:///./local.db"

engine = create_engine(
    DATABASE_URL,
    pool_pre_ping=True,
    future=True,
)

SessionLocal = sessionmaker(bind=engine, autoflush=False, autocommit=False, future=True)
Base = declarative_base()


def utcnow():
    # datetime UTC con timezone
    return datetime.now(timezone.utc)


class Thread(Base):
    __tablename__ = "threads"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    title = Column(String, nullable=False, default="New chat")
    created_at = Column(DateTime(timezone=True), nullable=False, default=utcnow)
    updated_at = Column(DateTime(timezone=True), nullable=False, default=utcnow, onupdate=utcnow)

    # ✅ delete-orphan + passive_deletes para borrado en cascada consistente
    messages = relationship(
        "Message",
        back_populates="thread",
        cascade="all, delete-orphan",
        passive_deletes=True,
    )


class Message(Base):
    __tablename__ = "messages"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    thread_id = Column(
        String,
        ForeignKey("threads.id", ondelete="CASCADE"),
        nullable=False,
        index=True,
    )
    role = Column(String, nullable=False)  # user | assistant | system
    content = Column(Text, nullable=False, default="")
    created_at = Column(DateTime(timezone=True), nullable=False, default=utcnow)

    # ✅ NUEVO: aquí guardas runner/doc/mode/screenshot para que aparezca en historial
    # Ej:
    # {"mode":"execute","runner":{"status":"passed","screenshot_b64":"...","steps":[...]}}
    meta_json = Column(JSON, nullable=True)

    thread = relationship("Thread", back_populates="messages")


def init_db():
    Base.metadata.create_all(bind=engine)