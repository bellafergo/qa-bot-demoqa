import json
from pathlib import Path

BASE = Path("evidence/domain_memory")

def load_memory(domain: str) -> dict:
    path = BASE / f"{domain}.json"
    if path.exists():
        return json.loads(path.read_text())
    return {}

def save_memory(domain: str, patch: dict):
    BASE.mkdir(parents=True, exist_ok=True)
    path = BASE / f"{domain}.json"
    data = load_memory(domain)
    data.update(patch)
    path.write_text(json.dumps(data, indent=2))
