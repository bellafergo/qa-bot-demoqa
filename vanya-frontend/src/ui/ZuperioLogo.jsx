import React, { useEffect, useState } from "react";
import { useTheme } from "../context/ThemeContext.jsx";
import { getThemeLogoSrc } from "../lib/themeLogos.js";

export default function ZuperioLogo({ variant = "mark", className = "", alt = "Zuperio" }) {
  const { resolvedTheme } = useTheme();
  const paths = getThemeLogoSrc(resolvedTheme);
  const primary = variant === "horizontal" ? paths.horizontal : paths.mark;
  const fallback = variant === "horizontal" ? paths.fallbackHorizontal : paths.fallbackMark;

  const [src, setSrc] = useState(primary);

  useEffect(() => {
    setSrc(primary);
  }, [primary]);

  const [ok, setOk] = useState(true);

  if (!ok) {
    return (
      <div className={`zu-logo-fallback ${className}`.trim()} aria-label={alt}>
        Z
      </div>
    );
  }

  const onError = () => {
    if (src !== fallback) setSrc(fallback);
    else setOk(false);
  };

  if (variant === "horizontal") {
    return (
      <img
        src={src}
        alt={alt}
        className={`zu-logo-horizontal ${className}`.trim()}
        onError={onError}
      />
    );
  }

  return (
    <img
      src={src}
      alt={alt}
      width={48}
      height={48}
      className={`zu-logo-mark ${className}`.trim()}
      onError={onError}
    />
  );
}
