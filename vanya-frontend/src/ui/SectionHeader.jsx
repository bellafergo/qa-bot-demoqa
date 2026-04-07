import React from "react";

export default function SectionHeader({ title, description, actions, className = "" }) {
  return (
    <div className={`zu-section-header ${className}`.trim()}>
      <div>
        <h2 className="zu-section-header__title">{title}</h2>
        {description ? <p className="zu-section-header__desc">{description}</p> : null}
      </div>
      {actions ? <div className="zu-section-header__actions">{actions}</div> : null}
    </div>
  );
}
