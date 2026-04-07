/**
 * Logo assets per resolved theme (dark | light).
 *
 * Expected files (add to public/logo/ when design is ready):
 *   Dark UI  → light-colored marks:  zuperio-mark-white.svg, zuperio-horizontal-white.svg
 *   Light UI → dark-colored marks:   zuperio-mark-black.svg, zuperio-horizontal-black.svg
 *
 * Legacy fallbacks (always present): zuperio-mark.svg, zuperio-horizontal.svg
 *
 * @param {"dark" | "light"} resolvedTheme
 */
export function getThemeLogoSrc(resolvedTheme) {
  const light = resolvedTheme === "light";
  return {
    mark: light ? "/logo/zuperio-mark-black.svg" : "/logo/zuperio-mark-white.svg",
    horizontal: light
      ? "/logo/zuperio-horizontal-black.svg"
      : "/logo/zuperio-horizontal-white.svg",
    fallbackMark: "/logo/zuperio-mark.svg",
    fallbackHorizontal: "/logo/zuperio-horizontal.svg",
  };
}
