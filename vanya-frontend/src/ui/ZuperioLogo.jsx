import React, { useState } from "react";

const MARK = "/logo/zuperio-mark.svg";
const HORIZONTAL = "/logo/zuperio-horizontal.svg";

export default function ZuperioLogo({ variant = "mark", className = "", alt = "Zuperio" }) {
  const [ok, setOk] = useState(true);

  if (!ok) {
    return (
      <div className={`zu-logo-fallback ${className}`.trim()} aria-label={alt}>
        Z
      </div>
    );
  }

  if (variant === "horizontal") {
    return (
      <img
        src={HORIZONTAL}
        alt={alt}
        className={`zu-logo-horizontal ${className}`.trim()}
        onError={() => setOk(false)}
      />
    );
  }

  return (
    <img
      src={MARK}
      alt={alt}
      width={48}
      height={48}
      className={`zu-logo-mark ${className}`.trim()}
      onError={() => setOk(false)}
    />
  );
}
