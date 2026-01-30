import os
from redis import Redis
from rq import Queue

REDIS_URL = (os.getenv("REDIS_URL") or "").strip()
if not REDIS_URL:
    raise RuntimeError("REDIS_URL is required")

def get_redis() -> Redis:
    return Redis.from_url(REDIS_URL)

def get_queue(name: str = "vanya") -> Queue:
    # timeout por job (ej. 30 min)
    return Queue(name, connection=get_redis(), default_timeout=60 * 30)
