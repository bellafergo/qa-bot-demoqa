# core/doc_render.py
from typing import Any, Dict, List

from core.settings import settings


# ============================================================
# DOC renderer + trim
# ============================================================
def md_escape(s: str) -> str:
    return (s or "").replace("|", "\\|").replace("`", "\\`")


def truncate_code(s: str) -> str:
    s = s or ""
    if len(s) <= settings.DOC_MAX_CODE_CHARS:
        return s
    return s[: settings.DOC_MAX_CODE_CHARS] + "\n# ... (truncado por límite de tamaño)\n"


def trim_doc(doc: Dict[str, Any]) -> Dict[str, Any]:
    gherkin = doc.get("gherkin") or []
    if isinstance(gherkin, list) and len(gherkin) > settings.DOC_MAX_GHERKIN:
        doc["gherkin"] = gherkin[: settings.DOC_MAX_GHERKIN]

    tcs = doc.get("test_cases") or []
    if isinstance(tcs, list) and len(tcs) > settings.DOC_MAX_TEST_CASES:
        doc["test_cases"] = tcs[: settings.DOC_MAX_TEST_CASES]

    scr = doc.get("automation_scripts") or {}
    files = (scr.get("files") or []) if isinstance(scr, dict) else []
    if isinstance(files, list) and len(files) > settings.DOC_MAX_FILES:
        files = files[: settings.DOC_MAX_FILES]

    for f in files:
        if isinstance(f, dict) and "content" in f:
            f["content"] = truncate_code(f.get("content", ""))

    if isinstance(scr, dict):
        scr["files"] = files
        doc["automation_scripts"] = scr
    else:
        doc["automation_scripts"] = {
            "framework": "",
            "structure": "",
            "notes": [],
            "selectors_recommendation": [],
            "how_to_run": [],
            "files": [],
        }

    return doc


def render_doc_answer(doc: Dict[str, Any]) -> str:
    req = doc.get("requested") or {}
    story = doc.get("user_story") or ""
    domain = doc.get("domain") or ""
    context = doc.get("context") or ""
    assumptions = doc.get("assumptions") or []
    questions = doc.get("questions_to_clarify") or []
    invest = doc.get("invest") or {}

    out: List[str] = []
    out.append(
        "## 📌 Input\n"
        f"**Dominio:** `{md_escape(domain)}`  \n"
        f"**Historia / Requerimiento:** {md_escape(story)}"
    )
    if context:
        out.append(f"\n**Contexto:** {md_escape(context)}")

    if assumptions:
        out.append("\n## 🧩 Assumptions\n" + "\n".join([f"- {md_escape(a)}" for a in assumptions]))

    if questions:
        out.append("\n## ❓ Preguntas mínimas\n" + "\n".join([f"- {md_escape(q)}" for q in questions]))

    # ✅ INVEST
    if req.get("invest"):
        scores = invest.get("scores") if isinstance(invest, dict) else None
        if not scores and isinstance(invest, dict):
            scores = {
                k: v
                for k, v in invest.items()
                if k.lower()
                in ["independent", "negotiable", "valuable", "estimable", "small", "testable"]
            }

        total = invest.get("total") if isinstance(invest, dict) else None
        verdict = invest.get("verdict") if isinstance(invest, dict) else None
        gaps = invest.get("gaps") if isinstance(invest, dict) else None
        rewritten = invest.get("rewritten_story") if isinstance(invest, dict) else None

        out.append("\n## ✅ INVEST")
        out.append("| Criterio | Score |\n|---|---:|")
        for k in ["Independent", "Negotiable", "Valuable", "Estimable", "Small", "Testable"]:
            v = ""
            if isinstance(scores, dict):
                v = scores.get(k) or scores.get(k.lower()) or ""
            out.append(f"| {k} | {md_escape(str(v))} |")

        if total is not None:
            out.append(f"\n**Total:** {md_escape(str(total))}")
        if verdict:
            out.append(f"**Veredicto:** {md_escape(str(verdict))}")

        if gaps:
            out.append("\n### Brechas")
            for g in gaps:
                out.append(f"- {md_escape(str(g))}")

        if rewritten:
            out.append("\n### Historia reescrita")
            out.append(md_escape(str(rewritten)))

    # ✅ Test cases
    if req.get("cases"):
        tcs = doc.get("test_cases") or []
        out.append("\n## 🧪 Matriz de casos de prueba")
        out.append("| ID | Prio | Tipo | Auto | Caso |\n|---|---|---|---:|---|")
        for tc in tcs:
            out.append(
                f"| {md_escape(str(tc.get('id','')))} | {md_escape(str(tc.get('priority','')))} | "
                f"{md_escape(str(tc.get('type','')))} | "
                f"{'✅' if tc.get('automatable') else '—'} | {md_escape(str(tc.get('title','')))} |"
            )

    # ✅ Gherkin
    if req.get("gherkin"):
        gherkin = doc.get("gherkin") or []
        out.append("\n## 🥒 Criterios de aceptación (Gherkin)")
        for sc in gherkin:
            tag = sc.get("tag")
            if tag:
                out.append(f"\n@{md_escape(str(tag))}")
            out.append(f"Scenario: {md_escape(str(sc.get('scenario','')))}")
            for x in (sc.get("given") or [])[:8]:
                out.append(f"  Given {md_escape(str(x))}")
            for x in (sc.get("when") or [])[:8]:
                out.append(f"  When {md_escape(str(x))}")
            for x in (sc.get("then") or [])[:10]:
                out.append(f"  Then {md_escape(str(x))}")

    return "\n".join(out).strip()


def fallback_minimal_doc(requested, domain, context, story):
    minimal_doc = {
        "requested": requested,
        "domain": domain,
        "context": context,
        "user_story": story,
        "assumptions": ["No se proporcionaron reglas de seguridad específicas."],
        "questions_to_clarify": ["¿Qué políticas aplican? (MFA, lockout, rate-limit, captcha)?"],
        "invest": {},
        "gherkin": [],
        "test_cases": [
            {
                "id": "TC-LOGIN-001",
                "title": "Login exitoso",
                "priority": "P0",
                "type": "pos",
                "automatable": True,
                "preconditions": ["Usuario registrado y activo"],
                "steps": ["Abrir login", "Capturar email válido", "Capturar password válida", "Click ingresar"],
                "expected": "Redirige a home/cuenta y muestra sesión iniciada",
            },
            {
                "id": "TC-LOGIN-002",
                "title": "Password incorrecta",
                "priority": "P0",
                "type": "neg",
                "automatable": True,
                "preconditions": ["Usuario registrado"],
                "steps": ["Abrir login", "Email válido", "Password inválida", "Click ingresar"],
                "expected": "Mensaje de error y no inicia sesión",
            },
        ],
        "automation_scripts": {
            "framework": "",
            "structure": "",
            "notes": [],
            "selectors_recommendation": [],
            "how_to_run": [],
            "files": [],
        },
    }
    return trim_doc(minimal_doc)