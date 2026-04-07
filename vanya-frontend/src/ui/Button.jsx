import React from "react";

const VARIANT_CLASS = {
  primary: "btn btn-primary",
  secondary: "btn btn-secondary",
  ghost: "btn btn-ghost",
  destructive: "btn btn-destructive",
};

const SIZE_CLASS = {
  md: "",
  sm: "btn-sm",
  lg: "btn-lg",
};

export default function Button({
  variant = "primary",
  size = "md",
  className = "",
  as = "button",
  href,
  type = "button",
  children,
  ...rest
}) {
  const base = [VARIANT_CLASS[variant] || VARIANT_CLASS.primary, SIZE_CLASS[size] || "", className]
    .filter(Boolean)
    .join(" ");

  if (as === "a" && href) {
    return (
      <a href={href} className={base} {...rest}>
        {children}
      </a>
    );
  }

  return (
    <button type={type} className={base} {...rest}>
      {children}
    </button>
  );
}
