import React, { useEffect, useState } from "react";
import { useTheme } from "../context/ThemeContext.jsx";
import { getThemeLogoSrc } from "../lib/themeLogos.js";

export default function SidebarBrand({
  productName = "Vanya",
  productTagline = "QA Intelligence",
  variant = "rail",
}) {
  const { resolvedTheme } = useTheme();
  const paths = getThemeLogoSrc(resolvedTheme);

  const [wideSrc, setWideSrc] = useState(paths.horizontal);
  const [markSrc, setMarkSrc] = useState(paths.mark);

  useEffect(() => {
    setWideSrc(paths.horizontal);
    setMarkSrc(paths.mark);
  }, [paths.horizontal, paths.mark]);

  const [markOk, setMarkOk] = useState(true);
  const [wideOk, setWideOk] = useState(true);

  const onWideError = () => {
    if (wideSrc !== paths.fallbackHorizontal) setWideSrc(paths.fallbackHorizontal);
    else setWideOk(false);
  };

  const onMarkError = () => {
    if (markSrc !== paths.fallbackMark) setMarkSrc(paths.fallbackMark);
    else setMarkOk(false);
  };

  if (variant === "horizontal" && wideOk) {
    return (
      <div className="zu-sidebar-brand zu-sidebar-brand--horizontal">
        <img
          src={wideSrc}
          alt=""
          className="zu-sidebar-brand__logo-wide"
          onError={onWideError}
        />
        <span className="zu-sidebar-brand__sr">{productName}</span>
      </div>
    );
  }

  return (
    <div className="zu-sidebar-brand">
      {markOk ? (
        <img
          src={markSrc}
          alt=""
          width={28}
          height={28}
          className="zu-sidebar-brand__mark"
          onError={onMarkError}
        />
      ) : (
        <div className="nav-sidebar-brand-mark zu-sidebar-brand__fallback" aria-hidden>
          Z
        </div>
      )}
      <div className="zu-sidebar-brand__text">
        <div className="zu-sidebar-brand__name">{productName}</div>
        <div className="zu-sidebar-brand__tagline">{productTagline}</div>
      </div>
    </div>
  );
}
