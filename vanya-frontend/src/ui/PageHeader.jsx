import React from "react";

export default function PageHeader({
  title,
  subtitle,
  actions,
  className = "",
  eyebrow,
}) {
  return (
    <header className={`zu-page-header ${className}`.trim()}>
      <div className="zu-page-header__main">
        {eyebrow ? <div className="zu-page-header__eyebrow">{eyebrow}</div> : null}
        <h1 className="zu-page-header__title">{title}</h1>
        {subtitle ? <p className="zu-page-header__subtitle">{subtitle}</p> : null}
      </div>
      {actions ? <div className="zu-page-header__actions">{actions}</div> : null}
    </header>
  );
}
