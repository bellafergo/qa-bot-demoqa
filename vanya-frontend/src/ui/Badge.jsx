import React from "react";

const TONE_CLASS = {
  default: "badge badge-gray",
  success: "badge badge-green",
  warning: "badge badge-orange",
  danger: "badge badge-red",
  info: "badge badge-blue",
  accent: "badge badge-accent",
  neutral: "badge badge-gray",
};

export default function Badge({ tone = "neutral", className = "", children, ...rest }) {
  return (
    <span className={`${TONE_CLASS[tone] || TONE_CLASS.neutral} ${className}`.trim()} {...rest}>
      {children}
    </span>
  );
}
