import React, { useMemo, useState } from "react";

function Table({ columns, rows }) {
  return (
    <div style={{ overflowX: "auto", borderRadius: 12, border: "1px solid rgba(255,255,255,0.12)" }}>
      <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 13 }}>
        <thead>
          <tr>
            {columns.map((c) => (
              <th key={c} style={{ textAlign: "left", padding: "10px 12px", borderBottom: "1px solid rgba(255,255,255,0.12)" }}>
                {c}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((r, idx) => (
            <tr key={idx}>
              {columns.map((c) => (
                <td key={c} style={{ padding: "10px 12px", verticalAlign: "top", borderBottom: "1px solid rgba(255,255,255,0.06)" }}>
                  {r[c] ?? ""}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export default function DocArtifactTabs({ doc }) {
  const [tab, setTab] = useState("executive");

  const ev = doc?.executive_view || {};
  const qv = doc?.qa_view || {};

  const riskRows = useMemo(() => {
    const risks = Array.isArray(ev.top_risks) ? ev.top_risks : [];
    return risks.map((r) => ({
      Prioridad: r.priority || "",
      Riesgo: r.risk || "",
      Impacto: r.impact || "",
    }));
  }, [ev]);

  const matrixRows = useMemo(() => {
    const rows = Array.isArray(ev.matrix_summary) ? ev.matrix_summary : [];
    return rows.map((r) => ({
      ID: r.id || "",
      Escenario: r.scenario || "",
      "Resultado esperado": r.expected || "",
      Prioridad: r.priority || "",
    }));
  }, [ev]);

  const caseRows = useMemo(() => {
    const cases = Array.isArray(qv.cases) ? qv.cases : [];
    return cases.map((c) => ({
      ID: c.id || "",
      Escenario: c.scenario || "",
      Prioridad: c.priority || "",
      Tipo: c.type || "",
      Preconditions: (c.preconditions || []).join(" • "),
      Pasos: (c.steps || []).map((s, i) => `${i + 1}. ${s}`).join("\n"),
      "Resultado esperado": c.expected || "",
    }));
  }, [qv]);

  return (
    <div style={{ display: "grid", gap: 12 }}>
      <div style={{ display: "flex", gap: 8 }}>
        <button
          onClick={() => setTab("executive")}
          style={{
            padding: "8px 12px",
            borderRadius: 10,
            border: "1px solid rgba(255,255,255,0.16)",
            background: tab === "executive" ? "rgba(255,255,255,0.10)" : "transparent",
            color: "inherit",
          }}
        >
          Executive
        </button>
        <button
          onClick={() => setTab("qa")}
          style={{
            padding: "8px 12px",
            borderRadius: 10,
            border: "1px solid rgba(255,255,255,0.16)",
            background: tab === "qa" ? "rgba(255,255,255,0.10)" : "transparent",
            color: "inherit",
          }}
        >
          QA
        </button>
      </div>

      <div style={{ display: "grid", gap: 10 }}>
        <div style={{ fontSize: 18, fontWeight: 700 }}>{ev.title || "Artefacto QA"}</div>
        {ev.objective ? <div style={{ opacity: 0.9 }}><b>Objetivo:</b> {ev.objective}</div> : null}

        {tab === "executive" ? (
          <>
            {riskRows.length ? (
              <>
                <div style={{ fontWeight: 700, marginTop: 6 }}>Riesgos principales</div>
                <Table columns={["Prioridad", "Riesgo", "Impacto"]} rows={riskRows} />
              </>
            ) : null}

            {matrixRows.length ? (
              <>
                <div style={{ fontWeight: 700, marginTop: 6 }}>Matriz resumida</div>
                <Table columns={["ID", "Escenario", "Resultado esperado", "Prioridad"]} rows={matrixRows} />
              </>
            ) : null}
          </>
        ) : (
          <>
            {(qv.assumptions || []).length ? (
              <div>
                <div style={{ fontWeight: 700 }}>Supuestos</div>
                <ul style={{ marginTop: 6 }}>
                  {qv.assumptions.map((a, i) => <li key={i}>{a}</li>)}
                </ul>
              </div>
            ) : null}

            {(qv.questions_to_clarify || []).length ? (
              <div>
                <div style={{ fontWeight: 700 }}>Preguntas</div>
                <ul style={{ marginTop: 6 }}>
                  {qv.questions_to_clarify.map((q, i) => <li key={i}>{q}</li>)}
                </ul>
              </div>
            ) : null}

            {caseRows.length ? (
              <>
                <div style={{ fontWeight: 700 }}>Casos detallados</div>
                <Table columns={["ID", "Escenario", "Prioridad", "Tipo", "Preconditions", "Pasos", "Resultado esperado"]} rows={caseRows} />
              </>
            ) : null}
          </>
        )}
      </div>
    </div>
  );
}