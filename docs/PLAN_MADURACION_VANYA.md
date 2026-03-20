# Plan de Maduración Vanya — Arquitecto Técnico

Plan ejecutable, pragmático e incremental. No repite diagnóstico previo.

---

# 1. Priorización ejecutiva

| Iniciativa | Problema que resuelve | Severidad | Impacto | Esfuerzo | Riesgo impl. | Dependencias | Archivos/módulos a tocar |
|------------|------------------------|-----------|---------|----------|--------------|--------------|---------------------------|
| F1: Fix step inválido expected-fail | Paso sin `action` insertado rompe runner; falsos positivos en negativos | Alta | Cierra bug crítico | S | Bajo | Ninguna | `services/execute_engine.py` |
| F2: Unificar formato screenshot (PNG) | JPEG tratado como PNG corrompe evidencia y PDF | Alta | Evidencia fiable | S | Bajo | Ninguna | `runners/screenshot.py`, `report_service.py` |
| F3: Redacción de credenciales en execute path | Fuga de credenciales en prompt/steps/logs/PDF | Alta | Seguridad | S | Bajo | Ninguna | `services/execute_engine.py`, `services/report_service.py` |
| F4: Integrar step_validator antes de ejecutar | Errores runtime por steps malformados | Media | Estabilidad | S | Bajo | Ninguna | `services/execute_engine.py`, `api/routes/planner_routes.py` |
| C1: Contrato único de steps (core/schemas) | Tres vocabularios distintos (runner/planner/validator) | Alta | Flujo text→execute cerrado | M | Medio | F4 | `core/step_validator.py`, `core/schemas.py`, `services/nl_test_planner.py` |
| C2: Compilador planner→runner steps | Planner genera acciones que runner no ejecuta | Alta | Endpoint planner funcional | M | Medio | C1 | `core/step_compiler.py` (nuevo), `api/routes/planner_routes.py`, `services/nl_test_planner.py` |
| C3: Action Registry mínimo | Un solo punto de verdad para acciones ejecutables | Media | Extensibilidad sin romper | M | Medio | C1 | `core/action_registry.py` (nuevo), `runners/generic_steps.py` |
| P1: Persistir queued/running en SQLite | Polling 404 en async; in-memory no escala | Media-Alta | UX async + multi-instance | M | Medio | Ninguna | `services/run_store.py`, `services/db/`, `app.py` get_run_evidence |
| P2: Unificar evidence pipeline (captura→upload→report) | Lógica mezclada en execute_engine; catalog no sube | Media | Evidencia consistente | L | Medio | F2 | `services/evidence_pipeline.py` (nuevo), `execute_engine.py`, `test_catalog_service.py` |
| R1: Extraer step compiler de execute_engine | God object; difícil testear/evolucionar | Media | Mantenibilidad | M | Medio | F4 | `core/step_compiler.py`, `services/execute_engine.py` |
| R2: Extraer evidence pipeline de execute_engine | Responsabilidades mezcladas | Media | Reutilización en catalog | M | Medio | P2 | `services/evidence_pipeline.py`, `services/execute_engine.py` |
| R3: Extraer step LLM generator | Fallback LLM acoplado al engine | Baja | Testabilidad LLM | S | Bajo | R1 | `services/step_llm_generator.py`, `execute_engine.py` |
| O1: Correlation ID en request→runner | Debug en producción imposible | Media | Observabilidad | S | Bajo | Ninguna | `app.py` middleware, `services/execute_engine.py`, `workers/jobs.py`, `runner` |
| E1: Evidence config por entorno | Memoria/costo alto; timeline en cada paso | Media | Costo/performance | S | Bajo | Ninguna | `runners/generic_steps.py`, `core/settings.py` |
| E2: Resolver evidence_id mismatch en async jobs | Traces/artefactos no coinciden con ID mostrado | Media | RCA fiable | S | Bajo | Ninguna | `workers/jobs.py`, `api/routes/execute.py` |

---

# 2. Qué atacar primero (top 5 ordenadas)

## 1. F1: Fix step inválido expected-fail
- **Por qué va primero:** Bug que rompe ejecuciones concretas (login negativo SauceDemo). Cambio pequeño y sin dependencias.
- **Desbloquea:** Casos negativos fiables; evita “pasar” por error.
- **Riesgo que reduce:** Falsos positivos en QA.

## 2. F2: Unificar formato screenshot (PNG)
- **Por qué va segundo:** Evidencia es core; hoy está corrupta. Cambio localizado.
- **Desbloquea:** PDF y Cloudinary correctos; base para P2.
- **Riesgo que reduce:** Pérdida de trazabilidad y confianza en evidencia.

## 3. F3: Redacción de credenciales
- **Por qué va tercero:** Riesgo de seguridad; fix acotado reutilizando `_redact` de workers.
- **Desbloquea:** Cumplimiento mínimo de seguridad; poder usar creds reales en demos sin exponerlas.
- **Riesgo que reduce:** Exposición de secretos en logs/PDF.

## 4. F4: Integrar step_validator antes de ejecutar
- **Por qué va cuarto:** Ya existe; solo falta usarlo. Previene errores antes de tocar runner.
- **Desbloquea:** C1 (contrato único) y validación en planner_routes.
- **Riesgo que reduce:** Errores runtime por steps malformados.

## 5. C2: Compilador planner→runner steps
- **Por qué va quinto:** Cierra el flujo text→plan→execute que es prioridad; depende de C1 pero se puede hacer un compilador mínimo sin refactor grande.
- **Desbloquea:** Endpoint `execute_text` funcional para todos los intents del planner.
- **Riesgo que reduce:** Fallos silenciosos o “Acción no soportada” en planner.

---

# 3. Plan de implementación en fases cortas

## Sprint 1 — Correcciones críticas (1–2 semanas)

**Objetivo:** Corregir bugs que afectan ejecución, evidencia y seguridad sin cambiar arquitectura.

**Cambios concretos:**
1. **F1** — En `execute_engine._parse_steps_from_prompt`, reemplazar `steps.insert(0, {"expected": "fail"})` por `steps[0]["expected"] = "fail"` (o el primer step con action). Eliminar pasos sin `action`.
2. **F2** — En `runners/screenshot.py`, cambiar `type="jpeg"` a `type="png"` (o mantener jpeg y en `_make_png_data_url`/cloudinary usar `data:image/jpeg` si se decide JPEG).
3. **F3** — Crear `core/redaction.py` con `redact_secrets(text)` (mover lógica de `workers/jobs._redact`). Llamar en execute_engine antes de guardar steps/prompt en store y en report_service antes de incluir en PDF.
4. **F4** — En `execute_engine.handle_execute_mode`, tras `_normalize_steps_to_target`, llamar `from core.step_validator import validate_steps`; si `not result.valid`, devolver error con `result.errors` en lugar de ejecutar. Idem en `planner_routes.execute_text_endpoint` antes de `execute_test`.

**Definición de terminado:**
- Login negativo SauceDemo pasa/falla según expectativa y no lanza error interno.
- Screenshot en PDF y Cloudinary se visualiza correctamente.
- Prompt con credenciales no aparece en PDF ni en logs sin redactar.
- Step con `action` inválido devuelve 400 con mensaje claro en vez de fallar en runner.

---

## Sprint 2 — Contrato y compilador (2 semanas)

**Objetivo:** Contrato único de steps y compilador que cierre planner→runner.

**Cambios concretos:**
1. **C1** — Definir en `core/schemas.py` `RUNNER_ACTIONS` (subset real del runner). Actualizar `core/step_validator.VALID_ACTIONS` para que coincida con lo que `generic_steps` ejecuta. Documentar en docstring.
2. **C2** — Crear `core/step_compiler.py` con `compile_to_runner_steps(plan_steps: List[dict], base_url: str) -> List[dict]`:
   - `login` → expandir a fill/click (como hoy en planner_routes).
   - `search` → goto + fill input búsqueda + click buscar (o placeholder según site).
   - `add_to_cart` → placeholder que devuelve error controlado “no implementado” o stub mínimo.
   - `screenshot` → `wait_ms` + paso no ejecutable que el runner ignora (o agregar `screenshot` al runner como no-op si se prefiere).
   - Resto: pasar si está en RUNNER_ACTIONS; si no, rechazar con error explícito.
3. En `planner_routes.execute_text_endpoint`, reemplazar `_expand_abstract_steps` por `step_compiler.compile_to_runner_steps` y validar con `validate_steps` antes de `execute_test`.
4. **E1** — En `runners/generic_steps.py`, leer `EVIDENCE_TIMELINE_ENABLED` de settings (default False en prod). Pasar a `EvidenceConfig(screenshot_timeline=...)`.
5. **E2** — En `workers/jobs.run_execute_steps_job`, pasar `evidence_id` al payload de `execute_test` si el runner lo acepta; o post-procesar el result para que `evidence_id` coincida con el job.

**Definición de terminado:**
- Planner emite solo acciones que el compilador sabe traducir o rechaza explícitamente.
- `execute_text` con "login en saucedemo" y "agregar tomate al carrito" no crashea; search/add_to_cart tienen comportamiento definido (implementado o error controlado).
- Evidencia timeline desactivada por defecto en prod.
- evidence_id en respuesta async coincide con traces/artefactos.

---

## Sprint 3 — Persistencia y evidence pipeline (2 semanas)

**Objetivo:** Persistencia unificada para runs y pipeline de evidencia reutilizable.

**Cambios concretos:**
1. **P1** — En `run_store.save_run`, cuando status es `queued` o `running`, escribir también en SQLite (nueva tabla `run_evidence_cache` o reutilizar `test_runs` con `test_case_id='_async'`). En `app.get_run_evidence`, buscar primero en run_store; si no hay, en run_history_service; si no, en nueva tabla para async.
2. **P2/R2** — Crear `services/evidence_pipeline.py`:
   - `capture_from_runner_result(result: dict) -> EvidenceBundle` (extrae b64, normaliza formato).
   - `upload_evidence(bundle, evidence_id) -> {evidence_url, report_url}` (Cloudinary screenshot + PDF).
   - `generate_report(...)` delega a report_service.
3. En `execute_engine`, reemplazar bloque de evidence/PDF por llamadas a `evidence_pipeline`. Mantener fallback PDF local si Cloudinary falla.
4. En `test_catalog_service._execute`, opcionalmente llamar `evidence_pipeline.upload_evidence` cuando `result.get("screenshot_b64")` y `HAS_CLOUDINARY`; persistir `evidence_url`/`report_url` en TestRun.

**Definición de terminado:**
- Polling a `/runs/{evidence_id}` para job async devuelve 200 con status queued/running/completed sin depender del proceso que encoló.
- execute_engine usa evidence_pipeline; catalog puede subir evidencia a Cloudinary si está configurado.
- No hay duplicación de lógica Cloudinary/PDF entre engine y otros módulos.

---

## Sprint 4 — Refactors y observabilidad (1–2 semanas)

**Objetivo:** Extraer responsabilidades de execute_engine y añadir correlation ID.

**Cambios concretos:**
1. **R1** — Crear `core/step_compiler.py` ampliado (o renombrar el de Sprint 2): mover `_parse_steps_from_prompt`, uso de `build_login_steps`, `_extract_semantic_intent`, `_extract_semantic_action`, `_ensure_has_assert`. `execute_engine` importa y llama `compile_steps_from_prompt(prompt, base_url) -> List[dict]`.
2. **R3** — Crear `services/step_llm_generator.py` con `generate_steps_llm(prompt, base_url, messages) -> List[dict] | None`. `execute_engine` lo usa como fallback cuando `compile_steps_from_prompt` retorna None.
3. **O1** — Middleware en `app.py` que genera `request_id = uuid.uuid4().hex[:12]` y lo pone en `request.state.request_id`. Propagar en `handle_execute_mode`, `workers/jobs`, y `execute_test` (como kwarg opcional `correlation_id`). Incluir en logs y en meta del run.
4. Actualizar `execute_engine` para que solo orqueste: pick_base_url → step_compiler → step_llm fallback → validate → execute_test → evidence_pipeline → save_run → response.

**Definición de terminado:**
- execute_engine < 400 líneas; step compiler y step LLM en módulos separados.
- Logs de una ejecución contienen el mismo request_id/correlation_id de punta a punta.
- Comportamiento funcional idéntico (tests de regresión pasan).

---

# 4. Diseño técnico propuesto

## A. Contrato único de steps

### Shape canónico (dict)

```python
{
    "action": str,           # requerido, ver lista abajo
    "selector": str | None,  # para acciones que lo requieren
    "target": dict | None,   # {"primary": str, "fallbacks": [...], "intent": str}
    "url": str | None,       # goto
    "value": str | None,     # fill, assert_url_contains
    "text": str | None,      # assert_text_contains
    "key": str | None,       # press
    "ms": int | None,        # wait_ms
    "expected": str | None,  # "pass" | "fail"
    "timeout_ms": int | None
}
```

### Acciones permitidas (runner real)

Subset ejecutable por `runners/generic_steps.execute_test`:

- `goto`, `fill`, `click`, `press`, `wait_ms`
- `assert_visible`, `assert_not_visible`, `assert_text_contains`, `assert_url_contains`

### Campos requeridos por acción

| action | requeridos |
|--------|------------|
| goto | url |
| fill | selector o target.primary, value |
| click | selector o target.primary |
| press | selector o target.primary, key |
| wait_ms | ms (suggested) |
| assert_visible, assert_not_visible | selector o target.primary |
| assert_text_contains | text, selector opcional (default body) |
| assert_url_contains | value |

### Validaciones mínimas

- `action` en vocabulario permitido.
- Para acciones con selector: `selector` o `target.primary` presente.
- `goto`: `url` presente.
- `press`: `key` presente.
- Step es dict; no se aceptan steps sin `action`.

### Step inválido

- `action` vacío o no permitido.
- action con selector requerido y sin selector ni target.primary.
- goto sin url.
- press sin key.
- Step no es dict.

---

## B. Planner → Compiler → Runner

**Recomendación: Planner genera DSL alto; Compiler traduce a runner steps.**

**Por qué:**
1. El planner (`nl_test_planner`) ya produce intents de negocio (login, search, add_to_cart) que no son 1:1 con acciones Playwright.
2. Un compilador permite múltiples targets (Playwright, API, desktop) sin reescribir el planner.
3. Validación y expansión en un solo lugar; el runner se mantiene estable.

**Flujo:**
```
text → nl_test_planner.plan_from_text() → plan.steps (DSL alto)
    → core/step_compiler.compile_to_runner_steps(plan.steps, base_url)
    → runner steps (vocabulario del runner)
    → core.step_validator.validate_steps()
    → runner.execute_test()
```

---

## C. Action Registry

### Estructura propuesta

**Archivo:** `core/action_registry.py`

```python
# Registro: action_name -> {executor, required_fields, optional_fields}
ACTION_REGISTRY: Dict[str, ActionSpec] = {
    "goto": ActionSpec(executor="runner", required=["url"]),
    "fill": ActionSpec(executor="runner", required=["selector|target", "value"]),
    "click": ActionSpec(executor="runner", required=["selector|target"]),
    ...
}
```

### Responsabilidad

- Definir qué acciones existen y qué campos requieren.
- `step_validator` usa el registry para validar.
- `generic_steps` ejecuta solo acciones registradas con `executor="runner"`.

### Mapeo a ejecutores

- `executor="runner"` → `runners/generic_steps` (switch por action dentro del runner).
- Futuro: `executor="api"` → `services/api_runner`.

### Validación de compatibilidad

- Antes de ejecutar: `all(step["action"] in ACTION_REGISTRY for step in steps)`.
- Si una acción no está registrada o no tiene executor para el target actual, fallar con error explícito.

---

## D. Evidence Pipeline

### Separación de responsabilidades

| Fase | Módulo | Responsabilidad |
|------|--------|-----------------|
| Captura | `runners/generic_steps` + `runners/evidence_capture.py` | Screenshot, trace, DOM snapshot. Salida: dict con b64, bundle. |
| Transformación | `services/evidence_pipeline.py` | Normalizar formato (PNG/jpeg), empaquetar en `EvidenceBundle`. |
| Upload | `services/cloudinary_service.py` | Subir screenshot y PDF; retornar URLs. |
| Metadata | `services/evidence_pipeline.py` | Armar metadata (evidence_id, urls, timestamps). |
| Report generation | `services/report_service.py` | Generar PDF desde runner result + screenshot. |

### Flujo

```
runner result (screenshot_b64, ...) 
  → evidence_pipeline.process(result, evidence_id, prompt, base_url, ...)
    → normaliza formato
    → report_service.generate_pdf_report(...)
    → cloudinary.upload_screenshot_b64(...)
    → cloudinary.upload_pdf_bytes(...)
  → {evidence_url, report_url, pdf_error}
```

---

## E. Run persistence

### Modelo unificado propuesto

**Fuente única para lectura:** `services/run_history_service` (ya delegado a SQLite).

**Escritura unificada:**

| Origen | Escritura | Tabla/Store |
|--------|-----------|-------------|
| Chat execute sync | `run_store.save_run` | In-memory TTL + bridge a SQLite |
| Async execute (workers) | `run_store.save_run` | In-memory + bridge; queued/running también a SQLite |
| Suite / catalog | `test_run_repo.create_run` | SQLite test_runs |
| Orchestrator job | `orch_job_repo` | SQLite orchestrator_jobs |

**Cambio clave:** run_store siempre escribe en SQLite para estados queued/running (nueva fila o actualización), de modo que `get_run_evidence` pueda resolver desde DB si el run no está en memoria.

**Esquema sugerido para async (reutilizar test_runs):**
- `test_case_id = '_async'` para runs de chat/execute async.
- `status` incluye queued, running, pass, fail, error.
- `evidence_url`, `report_url` en columnas existentes.
- `meta_json` guarda payload ligero (sin screenshot_b64 en listados).

**Estados a soportar:** queued, running, passed, failed, error, canceled.

---

# 5. Refactors con secuencia recomendada

## execute_engine.py

**Paso 1 — Step compiler (sin mover LLM aún):**
- Crear `core/step_compiler.py` con `compile_steps_from_prompt(prompt, base_url) -> List[dict] | None`.
- Mover `_parse_steps_from_prompt`, `_ensure_has_assert`, y las llamadas a `build_login_steps`, `_extract_semantic_intent`, `_extract_semantic_action`, `build_semantic_target`.
- `execute_engine` importa `compile_steps_from_prompt` y lo llama. Si retorna None, sigue con LLM en engine.
- Evitar romper: mismos tests; mismo output para mismos inputs.

**Paso 2 — Step LLM generator:**
- Crear `services/step_llm_generator.py` con `generate_steps_llm(prompt, base_url, messages) -> List[dict] | None`.
- Mover el bloque `if not steps: try: client.chat.completions.create(...)` de execute_engine.
- `execute_engine` llama `step_compiler`; si None, llama `step_llm_generator`.
- Evitar romper: mock del LLM en tests; mismo JSON de steps.

**Paso 3 — Evidence pipeline:**
- Crear `services/evidence_pipeline.py` con `process_evidence(runner_result, prompt, base_url, evidence_id, ...) -> {evidence_url, report_url, pdf_error}`.
- Mover lógica de Cloudinary + report_service + fallback PDF desde execute_engine.
- execute_engine solo llama `evidence_pipeline.process_evidence` y usa el resultado.
- Evitar romper: tests con y sin Cloudinary; mismo formato de respuesta.

---

## planner_routes.py

**Paso 1 — Usar step_compiler en lugar de _expand_abstract_steps:**
- Importar `from core.step_compiler import compile_to_runner_steps`.
- Reemplazar `_expand_abstract_steps(steps)` por `compile_to_runner_steps(steps, base_url)`.
- Añadir `validate_steps` antes de `execute_test`; si no valid, devolver 400 con errores.
- Evitar romper: tests con "login" y "add_to_cart"; add_to_cart puede devolver error controlado.

**Paso 2 — Eliminar _expand_abstract_steps:**
- Borrar función local una vez que compile_to_runner_steps cubra los casos.

---

## nl_test_planner.py

**Paso 1 — Sin cambiar output del planner:**
- Mantener `ALLOWED_ACTIONS` como está (DSL alto).
- Documentar que esas acciones deben ser compiladas por `step_compiler` antes de ejecutar.
- Añadir en docstring que `search`, `add_to_cart`, `screenshot` requieren compilador.

**Paso 2 — Validación en plan_from_text (opcional):**
- Si se añaden acciones nuevas, asegurar que el compilador las maneje o rechace explícitamente.

---

## run_store / run_bridge / SQLite

**Paso 1 — Persistir queued/running:**
- En `run_store.save_run`, cuando status in (queued, running), llamar a `test_run_repo.create_run` o a un `run_evidence_repo.upsert` con test_case_id='_async'.
- Asegurar que el bridge no duplique lógica; el bridge ya persiste completed/failed.
- Evitar romper: runs completados siguen yendo por bridge; listados no se duplican.

**Paso 2 — get_run unificado:**
- En `app.get_run_evidence`, orden de búsqueda: run_store → run_history_service → run_evidence_repo (async por evidence_id).
- Si run_evidence_repo devuelve queued/running, adaptar a la forma que espera el HTML renderer.

---

## Evidence handling

**Paso 1 — Crear evidence_pipeline:**
- `services/evidence_pipeline.py` con `process_evidence(...)` que centraliza upload y report.
- execute_engine deja de importar cloudinary_service y report_service directamente.
- Evitar romper: misma URL en respuesta; mismos fallbacks.

**Paso 2 — Unificar formato en captura:**
- `runners/screenshot.py` produce PNG.
- evidence_pipeline asume PNG en data URL.
- Evitar romper: report_service y Cloudinary siguen recibiendo formato esperado.

**Paso 3 — Catalog opcional con evidencia en cloud:**
- En `test_catalog_service._execute`, si HAS_CLOUDINARY y hay screenshot_b64, llamar evidence_pipeline para subir y actualizar TestRun con evidence_url/report_url.

---

# 6. Entregable final para ingeniería

| Tarea técnica | Resultado esperado | Módulo responsable | Prioridad | Complejidad | Comentarios |
|---------------|--------------------|--------------------|-----------|-------------|-------------|
| Fix expected-fail step inválido | No se insertan steps sin action; negativos funcionan | execute_engine | P0 | Baja | 1–2 hrs |
| Screenshot PNG consistente | Captura PNG; evidencia y PDF correctos | screenshot.py, report_service | P0 | Baja | 2–4 hrs |
| Redacción de credenciales | Prompt/steps sin creds en logs y PDF | execute_engine, report_service, core/redaction | P0 | Baja | 2–4 hrs |
| Integrar validate_steps | Errores 400 antes de ejecutar si steps inválidos | execute_engine, planner_routes | P0 | Baja | 1–2 hrs |
| Contrato único (VALID_ACTIONS alineado) | step_validator y runner usan mismo vocabulario | step_validator, schemas | P1 | Media | 2–4 hrs |
| step_compiler para planner | compile_to_runner_steps; login/search/add_to_cart | core/step_compiler | P1 | Media | 1 día |
| Usar compilador en planner_routes | execute_text no crashea con planner steps | planner_routes | P1 | Baja | 2–4 hrs |
| Evidence config por entorno | timeline desactivada por defecto en prod | generic_steps, settings | P1 | Baja | 1–2 hrs |
| evidence_id coherente en async | Job y artefactos usan mismo ID | workers/jobs, execute | P1 | Baja | 2–4 hrs |
| Persistir queued/running | Polling resuelve sin depender de proceso | run_store, db, app | P1 | Media | 1 día |
| evidence_pipeline | Servicio centralizado upload+report | evidence_pipeline (nuevo) | P1 | Media | 1–2 días |
| Migrar execute_engine a evidence_pipeline | Engine usa pipeline; sin lógica Cloudinary directa | execute_engine | P1 | Media | 4–8 hrs |
| Extraer step_compiler de execute_engine | compile_steps_from_prompt en core | step_compiler, execute_engine | P2 | Media | 1 día |
| Extraer step_llm_generator | Fallback LLM en servicio separado | step_llm_generator, execute_engine | P2 | Baja | 4–8 hrs |
| Correlation ID | request_id en logs y meta | app middleware, execute_engine, jobs | P2 | Baja | 4–8 hrs |
| Action registry básico | Registry como fuente de verdad de acciones | core/action_registry | P2 | Media | 1 día |
| Catalog con evidencia en cloud | TestRun con evidence_url cuando Cloudinary config | test_catalog_service | P2 | Baja | 4–8 hrs |

---

*Documento vivo. Actualizar tras cada sprint.*
