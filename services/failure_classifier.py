def classify_failure(error: Exception, signals: Dict[str, Any]) -> Dict[str, Any]:
    msg = str(error)

    # UI change
    if "locator_not_found" in msg or "waiting for locator" in msg:
        return {
            "type": "ui_change",
            "confidence": 0.85,
            "reason": "Selector no encontrado o DOM cambió"
        }

    # Flaky / environment
    if "Timeout" in msg or signals.get("network_errors"):
        return {
            "type": "flaky",
            "confidence": 0.75,
            "reason": "Timeout o error de red"
        }

    # Business bug
    if "assert" in msg or "expected" in msg:
        return {
            "type": "bug",
            "confidence": 0.9,
            "reason": "Falló una validación de negocio"
        }

    return {
        "type": "unknown",
        "confidence": 0.4,
        "reason": "No se pudo clasificar"
    }
