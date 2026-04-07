import React from "react";

export default function Card({
  children,
  className = "",
  padding = "md",
  interactive = false,
  style,
  ...rest
}) {
  const pad =
    padding === "none"
      ? ""
      : padding === "sm"
        ? "zu-card--pad-sm"
        : padding === "lg"
          ? "zu-card--pad-lg"
          : "zu-card--pad-md";

  const cls = `zu-card ${interactive ? "zu-card--interactive" : ""} ${pad} ${className}`.trim();

  return (
    <div className={cls} {...rest} style={style}>
      {children}
    </div>
  );
}
