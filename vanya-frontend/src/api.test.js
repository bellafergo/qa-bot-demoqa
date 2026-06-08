import { describe, expect, it } from "vitest";
import { normalizeErrorText, parseResponseDetail } from "./api.js";

describe("normalizeErrorText", () => {
  it("shortens Cloudflare HTML 400 pages", () => {
    const html = "<title>400 Bad Request</title><h1>400 Bad Request</h1>";
    expect(normalizeErrorText(html)).toBe("Gateway error (400). Please refresh in a moment.");
  });

  it("handles HTTP/2 trailer errors", () => {
    expect(normalizeErrorText("Trailers must have END_STREAM set.")).toMatch(/Connection interrupted/);
    expect(normalizeErrorText("ConnectionTerminated error_code:1")).toMatch(/Connection lost/);
  });

  it("handles Supabase JSON generation errors", () => {
    expect(normalizeErrorText("JSON could not be generated")).toMatch(/invalid response/i);
  });

  it("handles HTTP/2 KeyError stream ids from backend", () => {
    expect(normalizeErrorText("KeyError: 3")).toMatch(/connection interrupted/i);
  });
});

describe("parseResponseDetail", () => {
  it("extracts FastAPI detail without leaking HTML", () => {
    const body = JSON.stringify({ detail: "<html><title>400 Bad Request</title></html>" });
    expect(parseResponseDetail(body)).toBe("Gateway error (400). Please refresh in a moment.");
  });
});
