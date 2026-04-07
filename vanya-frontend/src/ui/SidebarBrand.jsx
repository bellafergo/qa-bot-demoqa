import React, { useState } from "react";

const MARK = "/logo/zuperio-mark.svg";
const HORIZONTAL = "/logo/zuperio-horizontal.svg";

export default function SidebarBrand({
  productName = "Vanya",
  productTagline = "QA Intelligence",
  variant = "rail",
}) {
  const [markOk, setMarkOk] = useState(true);
  const [wideOk, setWideOk] = useState(true);

  if (variant === "horizontal" && wideOk) {
    return (
      <div className="zu-sidebar-brand zu-sidebar-brand--horizontal">
        <img
          src={HORIZONTAL}
          alt=""
          className="zu-sidebar-brand__logo-wide"
          onError={() => setWideOk(false)}
        />
        <span className="zu-sidebar-brand__sr">{productName}</span>
      </div>
    );
  }

  return (
    <div className="zu-sidebar-brand">
      {markOk ? (
        <img
          src={MARK}
          alt=""
          width={28}
          height={28}
          className="zu-sidebar-brand__mark"
          onError={() => setMarkOk(false)}
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
