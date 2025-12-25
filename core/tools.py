# core/tools.py

QA_TOOL = {
    "type": "function",
    "function": {
        "name": "run_qa_test",
        "description": "Ejecuta acciones en un navegador real para validar una web.",
        "parameters": {
            "type": "object",
            "properties": {
                "steps": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "action": {
                                "type": "string",
                                "enum": [
                                    "goto",
                                    "fill",
                                    "click",
                                    "press",
                                    "assert_visible",
                                    "assert_text_contains",
                                    "wait_ms",
                                ],
                            },
                            "url": {"type": "string"},
                            "selector": {"type": "string"},
                            "text": {"type": "string"},
                            "role": {"type": "string"},
                            "value": {"type": "string"},
                            "timeout_ms": {"type": "integer"},
                        },
                        "required": ["action"],
                    },
                }
            },
            "required": ["steps"],
        },
    },
}

QA_DOC_TOOL = {
    "type": "function",
    "function": {
        "name": "generate_qa_artifacts",
        "description": "Genera artefactos de QA estructurados para una historia de usuario (retail/POS/web).",
        "parameters": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "domain": {"type": "string", "enum": ["retail", "pos", "web"]},
                "user_story": {"type": "string"},
                "invest": {
                    "type": "object",
                    "additionalProperties": False,
                    "properties": {
                        "independent": {"type": "string"},
                        "negotiable": {"type": "string"},
                        "valuable": {"type": "string"},
                        "estimable": {"type": "string"},
                        "small": {"type": "string"},
                        "testable": {"type": "string"},
                        "score_0_10": {"type": "number"},
                    },
                    "required": [
                        "independent",
                        "negotiable",
                        "valuable",
                        "estimable",
                        "small",
                        "testable",
                        "score_0_10",
                    ],
                },
                "gherkin": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "additionalProperties": False,
                        "properties": {
                            "title": {"type": "string"},
                            "tags": {"type": "array", "items": {"type": "string"}},
                            "scenario": {"type": "string"},
                        },
                        "required": ["title", "tags", "scenario"],
                    },
                },
                "test_cases": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "additionalProperties": False,
                        "properties": {
                            "id": {"type": "string"},
                            "title": {"type": "string"},
                            "type": {
                                "type": "string",
                                "enum": ["positive", "negative", "edge", "security", "performance"],
                            },
                            "priority": {"type": "string", "enum": ["P0", "P1", "P2"]},
                            "risk": {"type": "string", "enum": ["high", "medium", "low"]},
                            "preconditions": {"type": "string"},
                            "steps": {"type": "array", "items": {"type": "string"}},
                            "expected": {"type": "string"},
                            "test_data": {"type": "string"},
                            "notes": {"type": "string"},
                        },
                        "required": ["id", "title", "type", "priority", "risk", "steps", "expected"],
                    },
                },
                "patterns": {
                    "type": "object",
                    "additionalProperties": False,
                    "properties": {
                        "recommended_smoke": {"type": "array", "items": {"type": "string"}},
                        "recommended_regression": {"type": "array", "items": {"type": "string"}},
                        "recommended_edges": {"type": "array", "items": {"type": "string"}},
                        "recommended_p0_minimal": {"type": "array", "items": {"type": "string"}},
                        "by_role": {
                            "type": "object",
                            "additionalProperties": {"type": "array", "items": {"type": "string"}},
                        },
                        "by_flow": {
                            "type": "object",
                            "additionalProperties": {"type": "array", "items": {"type": "string"}},
                        },
                    },
                    "required": ["recommended_smoke", "recommended_regression", "recommended_edges"],
                },
                "automation": {
                    "type": "object",
                    "additionalProperties": False,
                    "properties": {
                        "strategy": {"type": "string"},
                        "playwright_skeleton": {"type": "string"},
                        "selectors_guidance": {"type": "string"},
                    },
                    "required": ["strategy", "playwright_skeleton", "selectors_guidance"],
                },
                "assumptions": {"type": "array", "items": {"type": "string"}},
                "questions_to_clarify": {"type": "array", "items": {"type": "string"}},
            },
            "required": [
                "domain",
                "user_story",
                "invest",
                "gherkin",
                "test_cases",
                "patterns",
                "automation",
                "assumptions",
                "questions_to_clarify",
            ],
        },
    },
}