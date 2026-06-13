/** Normalize GET /projects/{id}/incidents/history response to a list of items. */
export function normalizeProjectIncidentHistory(data) {
  if (Array.isArray(data)) return data;
  if (Array.isArray(data?.items)) return data.items;
  return [];
}

export function hasProjectIncidentHistoryItems(items) {
  return Array.isArray(items) && items.length > 0;
}
