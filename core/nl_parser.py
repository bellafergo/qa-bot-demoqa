# core/nl_parser.py
"""
Natural language step parser with confidence scoring and ambiguity detection.

This module improves upon the raw regex parser in execute_engine.py by:
  - providing a typed ParsedIntent / NLParseResult output
  - scoring confidence per step and overall
  - surfacing ambiguities explicitly
  - generating fallback selector suggestions from the target text

It is intentionally kept pure (no I/O, no LLM calls, no Playwright) so it
can be used in pre-execution validation, test planning, and unit tests.

Usage
-----
    from core.nl_parser import parse_natural_language

    result = parse_natural_language("Click the Login button and verify the dashboard appears")
    for step in result.steps:
        print(step.action, step.target, step.confidence)
"""
from __future__ import annotations

import re
import unicodedata
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


# ── Output models ──────────────────────────────────────────────────────────────

class ParsedIntent(BaseModel):
    """One step parsed from natural language."""
    intent:               str            # "navigate" | "interact" | "assert" | "wait" | "unknown"
    action:               str            # canonical runner action name
    confidence:           float          # 0.0 – 1.0
    target:               Optional[str] = None   # element label / selector hint
    value:                Optional[str] = None   # fill value or URL
    # action-specific fields (mirrors NormalizedStep / StepSpec for compatibility)
    url:                  Optional[str] = None   # for goto steps
    text:                 Optional[str] = None   # for assert_text_contains
    key:                  Optional[str] = None   # for press
    ms:                   Optional[int] = None   # for wait_ms
    ambiguities:          List[str]      = Field(default_factory=list)
    fallback_suggestions: List[Dict[str, Any]] = Field(default_factory=list)
    raw_fragment:         str            = ""     # source text that produced this step


class NLParseResult(BaseModel):
    """Full parse result for a multi-step prompt."""
    steps:              List[ParsedIntent]
    overall_confidence: float
    notes:              List[str]        = Field(default_factory=list)
    raw_prompt:         str
    ambiguous:          bool             = False


# ── Regex patterns (ordered: most specific first) ─────────────────────────────

# goto / navigate
_RE_GOTO = re.compile(
    r'\b(?:go\s+to|navigate\s+to|open|visit|load)\s+'
    r'(?:the\s+)?(?:page\s+|url\s+|site\s+)?'
    r'(?P<url>https?://\S+|/[\w/\-?=&%#.]+|\w[\w\-./]+)',
    re.IGNORECASE,
)
_RE_GOTO_URL_BARE = re.compile(
    r'\b(?:url|page)\s+(?:is\s+|:\s*)?(?P<url>https?://\S+)',
    re.IGNORECASE,
)

# fill / type / enter
_RE_FILL_QUOTED = re.compile(
    r'\b(?:fill\s+in|fill|type|enter|input|set)\s+'
    r'(?:the\s+)?(?P<target>[\w\s\-]+?)\s+'
    r'(?:field\s+)?(?:with|to|as|=)\s+'
    r'["\'](?P<value>[^"\']+)["\']',
    re.IGNORECASE,
)
_RE_FILL_UNQUOTED = re.compile(
    r'\b(?:fill\s+in|fill|type|enter|input)\s+'
    r'["\'](?P<value>[^"\']+)["\']\s+'
    r'(?:in(?:to)?\s+)?(?:the\s+)?(?P<target>[\w\s\-]+?)(?:\s+field)?$',
    re.IGNORECASE,
)
_RE_FILL_INTO = re.compile(
    r'\b(?:type|enter|input)\s+'
    r'["\'](?P<value>[^"\']+)["\']\s+'
    r'(?:in(?:to)?\s+|for\s+)?(?:the\s+)?(?P<target>[\w\s\-]+)',
    re.IGNORECASE,
)

# click
_RE_CLICK = re.compile(
    r'\b(?:click|press|tap|select|choose|hit)\s+'
    r'(?:on\s+)?(?:the\s+)?(?P<target>[\w\s\-"\'()]+?)(?:\s+button|\s+link|\s+tab|\s+option)?'
    r'(?:\s+and\b|\s+then\b|$)',
    re.IGNORECASE,
)

# assert_visible
_RE_ASSERT_VISIBLE = re.compile(
    r'\b(?:verify|check|assert|confirm|ensure|validate)\s+'
    r'(?:that\s+)?(?:the\s+)?(?P<target>[\w\s\-"\'()]+?)'
    r'\s+(?:is\s+)?(?:visible|appears?|displays?|shows?|present)',
    re.IGNORECASE,
)

# assert_not_visible
_RE_ASSERT_NOT_VISIBLE = re.compile(
    r'\b(?:verify|check|assert|confirm|ensure)\s+'
    r'(?:that\s+)?(?:the\s+)?(?P<target>[\w\s\-"\'()]+?)'
    r'\s+(?:is\s+)?(?:not\s+visible|hidden|disappears?|gone)',
    re.IGNORECASE,
)

# assert_text_contains
_RE_ASSERT_TEXT = re.compile(
    r'\b(?:verify|check|assert|confirm|ensure|see)\s+'
    r'(?:that\s+)?(?:the\s+)?(?:text\s+)?'
    r'["\'](?P<text>[^"\']+)["\']'
    r'\s+(?:is\s+)?(?:visible|appears?|present|shows?)',
    re.IGNORECASE,
)
_RE_ASSERT_TEXT_ALT = re.compile(
    r'\b(?:see|should\s+see|check\s+for)\s+'
    r'["\'](?P<text>[^"\']+)["\']',
    re.IGNORECASE,
)

# assert_url_contains
_RE_ASSERT_URL = re.compile(
    r'\b(?:url|page)\s+(?:should\s+)?(?:contain|include|be|equals?)\s+'
    r'["\']?(?P<url>[^"\']+)["\']?',
    re.IGNORECASE,
)
_RE_ASSERT_URL_ALT = re.compile(
    r'\bredirects?\s+to\s+["\']?(?P<url>[^"\']+)["\']?',
    re.IGNORECASE,
)

# wait_ms
_RE_WAIT = re.compile(
    r'\b(?:wait|pause|sleep)\s+(?:for\s+)?(?P<ms>\d+)\s*(?:ms|milliseconds?|seconds?|s)\b',
    re.IGNORECASE,
)

# press key
_RE_PRESS = re.compile(
    r'\b(?:press|hit|type)\s+(?:the\s+)?(?P<key>Enter|Tab|Escape|Space|ArrowUp|ArrowDown|ArrowLeft|ArrowRight|Backspace|Delete)\b',
    re.IGNORECASE,
)


# ── Helpers ────────────────────────────────────────────────────────────────────

def _clean(s: str) -> str:
    """Normalize whitespace and strip quoted wrapping."""
    s = unicodedata.normalize("NFKC", s).strip()
    s = re.sub(r'\s+', ' ', s)
    # strip surrounding quotes
    if len(s) >= 2 and s[0] in ('"', "'") and s[-1] == s[0]:
        s = s[1:-1]
    return s.strip()


def _label_to_fallbacks(label: str) -> List[Dict[str, Any]]:
    """
    Generate fallback selector suggestions from a human-readable element label.
    E.g. "Login button" → [{type:"text", value:"Login"}, {type:"role", value:{role:"button", name:"Login"}}]
    """
    label = _clean(label)
    if not label:
        return []

    suggestions: List[Dict[str, Any]] = []

    # strip trailing type words (button, link, field, input, tab, checkbox)
    stripped = re.sub(
        r'\s+(?:button|link|field|input|tab|checkbox|option|menu|dropdown)$',
        '', label, flags=re.IGNORECASE
    ).strip()

    # text match (always useful)
    suggestions.append({"type": "text", "value": stripped or label})

    # role inference
    lower = label.lower()
    if "button" in lower:
        suggestions.append({"type": "role", "value": {"role": "button", "name": stripped}})
    elif "link" in lower:
        suggestions.append({"type": "role", "value": {"role": "link", "name": stripped}})
    elif "field" in lower or "input" in lower:
        suggestions.append({"type": "label", "value": stripped})
        suggestions.append({"type": "placeholder", "value": stripped})
    elif "checkbox" in lower:
        suggestions.append({"type": "role", "value": {"role": "checkbox", "name": stripped}})
    elif "tab" in lower:
        suggestions.append({"type": "role", "value": {"role": "tab", "name": stripped}})
    else:
        # generic label fallback
        suggestions.append({"type": "label", "value": stripped or label})

    return suggestions


def _to_ms(match_str: str, unit_str: str) -> int:
    """Convert wait value + unit to milliseconds."""
    v = int(match_str)
    u = (unit_str or "ms").lower().strip("s")
    if u in ("second",):
        return v * 1000
    return v  # default ms


# ── Per-sentence parser ────────────────────────────────────────────────────────

def _parse_fragment(fragment: str) -> Optional[ParsedIntent]:
    """
    Try to extract one ParsedIntent from a short text fragment.
    Returns None if no pattern matches.
    """
    frag = fragment.strip()
    if not frag:
        return None

    # 1. press key
    m = _RE_PRESS.search(frag)
    if m:
        key = m.group("key").capitalize()
        return ParsedIntent(
            intent="interact", action="press",
            key=key, target=None,
            confidence=0.90,
            raw_fragment=frag,
        )

    # 2. wait
    m = _RE_WAIT.search(frag)
    if m:
        unit = frag[m.end()-2:m.end()].strip()
        try:
            ms = _to_ms(m.group("ms"), unit)
        except Exception:
            ms = 1000
        return ParsedIntent(
            intent="wait", action="wait_ms",
            ms=ms, target=None,
            confidence=0.92,
            raw_fragment=frag,
        )

    # 3. goto (URL explicit)
    m = _RE_GOTO.search(frag) or _RE_GOTO_URL_BARE.search(frag)
    if m:
        url = m.group("url").rstrip(".,;)")
        return ParsedIntent(
            intent="navigate", action="goto",
            url=url, value=url, target=None,
            confidence=0.95,
            raw_fragment=frag,
        )

    # 4. assert URL
    m = _RE_ASSERT_URL.search(frag) or _RE_ASSERT_URL_ALT.search(frag)
    if m:
        url = _clean(m.group("url"))
        return ParsedIntent(
            intent="assert", action="assert_url_contains",
            value=url, target=None,
            confidence=0.88,
            raw_fragment=frag,
        )

    # 5. assert text (quoted)
    m = _RE_ASSERT_TEXT.search(frag) or _RE_ASSERT_TEXT_ALT.search(frag)
    if m:
        text = _clean(m.group("text"))
        return ParsedIntent(
            intent="assert", action="assert_text_contains",
            text=text, target=None,
            confidence=0.87,
            raw_fragment=frag,
        )

    # 6. assert NOT visible  ← must be checked BEFORE assert_visible to avoid
    #    greedy target match consuming "not" in "is not visible"
    m = _RE_ASSERT_NOT_VISIBLE.search(frag)
    if m:
        target_raw = _clean(m.group("target"))
        return ParsedIntent(
            intent="assert", action="assert_not_visible",
            target=target_raw,
            confidence=0.82,
            fallback_suggestions=_label_to_fallbacks(target_raw),
            raw_fragment=frag,
        )

    # 7. assert visible
    m = _RE_ASSERT_VISIBLE.search(frag)
    if m:
        target_raw = _clean(m.group("target"))
        return ParsedIntent(
            intent="assert", action="assert_visible",
            target=target_raw,
            confidence=0.82,
            fallback_suggestions=_label_to_fallbacks(target_raw),
            raw_fragment=frag,
        )

    # 8. fill (with quoted value, target first)
    m = _RE_FILL_QUOTED.search(frag)
    if m:
        target_raw = _clean(m.group("target"))
        value = _clean(m.group("value"))
        return ParsedIntent(
            intent="interact", action="fill",
            target=target_raw, value=value,
            confidence=0.90,
            fallback_suggestions=_label_to_fallbacks(target_raw),
            raw_fragment=frag,
        )

    # 9. fill (quoted value first, then target)
    m = _RE_FILL_UNQUOTED.search(frag) or _RE_FILL_INTO.search(frag)
    if m:
        target_raw = _clean(m.group("target"))
        value = _clean(m.group("value"))
        return ParsedIntent(
            intent="interact", action="fill",
            target=target_raw, value=value,
            confidence=0.85,
            fallback_suggestions=_label_to_fallbacks(target_raw),
            raw_fragment=frag,
        )

    # 10. click
    m = _RE_CLICK.search(frag)
    if m:
        target_raw = _clean(m.group("target"))
        # reject overly long or vague targets
        if len(target_raw) > 60 or target_raw.lower() in ("it", "that", "the", "this"):
            ambiguities = [f"Could not infer element from: '{target_raw}'"]
            return ParsedIntent(
                intent="interact", action="click",
                target=None, confidence=0.40,
                ambiguities=ambiguities,
                raw_fragment=frag,
            )
        return ParsedIntent(
            intent="interact", action="click",
            target=target_raw,
            confidence=0.85,
            fallback_suggestions=_label_to_fallbacks(target_raw),
            raw_fragment=frag,
        )

    return None


# ── Sentence splitter ──────────────────────────────────────────────────────────

_RE_SPLIT = re.compile(
    r'(?<=[.!?])\s+|'        # sentence boundary
    r'\s+(?:then|and then|after that|next|finally)\s+|'  # connectors
    r',\s+(?:then|and)\s+',  # comma + connector
    re.IGNORECASE,
)


def _split_prompt(prompt: str) -> List[str]:
    """Split a multi-step prompt into individual action fragments."""
    parts = _RE_SPLIT.split(prompt.strip())
    cleaned = []
    for p in parts:
        p = p.strip()
        # Strip trailing sentence-boundary punctuation so regex anchors ($) work
        p = p.rstrip(".,;!?")
        p = p.strip()
        if p:
            cleaned.append(p)
    return cleaned


# ── Public API ─────────────────────────────────────────────────────────────────

def parse_natural_language(
    prompt: str,
    context: Optional[Dict[str, Any]] = None,
) -> NLParseResult:
    """
    Parse a natural language prompt into a list of typed ParsedIntent steps.

    Parameters
    ----------
    prompt  : Natural language description of a test scenario.
    context : Optional dict — may contain {"base_url": ..., "module": ...} hints.

    Returns
    -------
    NLParseResult with per-step confidence scores and overall quality assessment.
    """
    context = context or {}
    fragments = _split_prompt(prompt)
    steps: List[ParsedIntent] = []
    notes: List[str] = []
    unmatched: List[str] = []

    for frag in fragments:
        intent = _parse_fragment(frag)
        if intent is not None:
            # Inject base_url for goto steps that have a relative path
            if intent.action == "goto" and intent.url and not intent.url.startswith("http"):
                base = context.get("base_url", "")
                if base:
                    full_url = base.rstrip("/") + "/" + intent.url.lstrip("/")
                    intent.url = full_url
                    intent.value = full_url
            steps.append(intent)
        else:
            unmatched.append(frag)

    if unmatched:
        notes.append(
            f"{len(unmatched)} fragment(s) could not be parsed: "
            + "; ".join(repr(u) for u in unmatched[:3])
        )

    # Overall confidence: mean of step confidences, penalised for unmatched
    if steps:
        mean_conf = sum(s.confidence for s in steps) / len(steps)
    else:
        mean_conf = 0.0

    # Penalty: each unmatched fragment reduces confidence
    penalty = 0.05 * len(unmatched)
    overall = max(0.0, round(min(1.0, mean_conf - penalty), 4))

    # Flag ambiguities
    all_ambiguities = [a for s in steps for a in s.ambiguities]
    ambiguous = bool(all_ambiguities) or bool(unmatched)

    if not steps:
        notes.append("No recognisable steps found — consider using the LLM fallback.")

    return NLParseResult(
        steps=steps,
        overall_confidence=overall,
        notes=notes,
        raw_prompt=prompt,
        ambiguous=ambiguous,
    )
