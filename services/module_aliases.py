# services/module_aliases.py
"""
Bilingual EN↔ES aliases for path token ↔ catalog module matching.

Used by change_impact_service when literal/substring/singular scoring fails.
"""
from __future__ import annotations

from typing import Dict, FrozenSet, Set

# Each group lists equivalent normalized terms (lowercase, no accents).
_ALIAS_GROUPS: tuple[FrozenSet[str], ...] = (
    frozenset({"candidates", "candidatos", "candidate", "candidato"}),
    frozenset({"vacancies", "vacantes", "vacancy", "vacante"}),
    frozenset({"proposals", "propuestas", "proposal", "propuesta"}),
    frozenset({"auth", "autenticacion", "authentication", "autenticación"}),
    frozenset({"dashboard", "tablero", "dashboards"}),
    frozenset({"companies", "empresas", "company", "empresa"}),
    frozenset({"contacts", "contactos", "contact", "contacto"}),
    frozenset({"opportunities", "oportunidades", "opportunity", "oportunidad"}),
)

_TERM_TO_GROUP: Dict[str, FrozenSet[str]] = {}
for _group in _ALIAS_GROUPS:
    for _term in _group:
        _TERM_TO_GROUP[_term] = _group


def _strip_accents(s: str) -> str:
    """Minimal accent fold for Spanish module names (e.g. autenticación)."""
    return (
        s.replace("á", "a")
        .replace("é", "e")
        .replace("í", "i")
        .replace("ó", "o")
        .replace("ú", "u")
        .replace("ñ", "n")
    )


def _singular(token: str) -> str:
    t = token.lower()
    if len(t) > 4 and t.endswith("ies"):
        return t[:-3] + "y"
    if len(t) > 3 and t.endswith("s"):
        return t[:-1]
    return t


def alias_forms(term: str) -> Set[str]:
    """All known equivalent forms for a token or module name."""
    raw = (term or "").strip().lower()
    if not raw:
        return set()
    folded = _strip_accents(raw)
    variants = {raw, folded, _singular(raw), _singular(folded)}
    out: Set[str] = set()
    for v in variants:
        group = _TERM_TO_GROUP.get(v)
        if group:
            out.update(group)
        else:
            out.add(v)
    return out


def alias_equivalent(a: str, b: str) -> bool:
    """True when both terms belong to the same bilingual alias group."""
    if not a or not b:
        return False
    return bool(alias_forms(a) & alias_forms(b))
