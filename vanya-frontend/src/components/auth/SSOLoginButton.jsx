import React from "react";

export default function SSOLoginButton({ label, onClick, disabled = false, busy = false }) {
  return (
    <button
      type="button"
      className="btn btn-secondary"
      style={{ width: "100%", justifyContent: "center" }}
      disabled={disabled || busy}
      onClick={onClick}
    >
      {busy ? "…" : label}
    </button>
  );
}
