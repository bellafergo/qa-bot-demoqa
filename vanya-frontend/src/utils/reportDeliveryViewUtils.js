/** View helpers for Executive Report Delivery (ENT-02C). */

export const REPORT_DELIVERY_I18N_KEYS = {
  title: "report_delivery.title",
  empty: "report_delivery.empty",
  reportType: "report_delivery.report_type",
  channel: "report_delivery.channel",
  recipient: "report_delivery.recipient",
  preview: "report_delivery.preview",
  send: "report_delivery.send",
  previewTitle: "report_delivery.preview_title",
  subject: "report_delivery.subject",
  summary: "report_delivery.summary",
  channelNotReady: "report_delivery.channel_not_ready",
  sendSuccess: "report_delivery.send_success",
  sendFailed: "report_delivery.send_failed",
  readOnlyNote: "report_delivery.read_only_note",
  requiresApprovalNote: "report_delivery.requires_approval_note",
  executiveQuality: "report_delivery.type.executive_quality",
  releaseReadiness: "report_delivery.type.release_readiness",
  weeklyQualityBrief: "report_delivery.type.weekly_quality_brief",
  incidentReview: "report_delivery.type.incident_review",
  email: "report_delivery.channel.email",
  slack: "report_delivery.channel.slack",
  teams: "report_delivery.channel.teams",
};

export const REPORT_DELIVERY_TYPES = [
  "EXECUTIVE_QUALITY",
  "RELEASE_READINESS",
  "WEEKLY_QUALITY_BRIEF",
  "INCIDENT_REVIEW",
];

export const REPORT_DELIVERY_CHANNELS = ["email", "slack", "teams"];

const REPORT_TYPE_LABEL_KEY = {
  EXECUTIVE_QUALITY: REPORT_DELIVERY_I18N_KEYS.executiveQuality,
  RELEASE_READINESS: REPORT_DELIVERY_I18N_KEYS.releaseReadiness,
  WEEKLY_QUALITY_BRIEF: REPORT_DELIVERY_I18N_KEYS.weeklyQualityBrief,
  INCIDENT_REVIEW: REPORT_DELIVERY_I18N_KEYS.incidentReview,
};

const CHANNEL_LABEL_KEY = {
  email: REPORT_DELIVERY_I18N_KEYS.email,
  slack: REPORT_DELIVERY_I18N_KEYS.slack,
  teams: REPORT_DELIVERY_I18N_KEYS.teams,
};

export function reportTypeLabelKey(reportType) {
  return REPORT_TYPE_LABEL_KEY[String(reportType || "").toUpperCase()] || reportType;
}

export function channelLabelKey(channel) {
  return CHANNEL_LABEL_KEY[String(channel || "").toLowerCase()] || channel;
}

export function isChannelReady(readiness, channel) {
  const ch = String(channel || "").toLowerCase();
  const item = readiness?.connectors?.[ch] || readiness?.[ch];
  return Boolean(item?.ready);
}

export function anyDeliveryChannelReady(readiness) {
  return REPORT_DELIVERY_CHANNELS.some((ch) => isChannelReady(readiness, ch));
}

export function buildReportDeliveryViewModel({ readiness, t }) {
  const channels = REPORT_DELIVERY_CHANNELS.map((id) => ({
    id,
    label: t(channelLabelKey(id)),
    ready: isChannelReady(readiness, id),
  }));
  const reportTypes = REPORT_DELIVERY_TYPES.map((id) => ({
    id,
    label: t(reportTypeLabelKey(id)),
  }));
  const hasChannels = anyDeliveryChannelReady(readiness);

  return {
    show: true,
    empty: !hasChannels,
    emptyMessage: t(REPORT_DELIVERY_I18N_KEYS.empty),
    title: t(REPORT_DELIVERY_I18N_KEYS.title),
    reportTypeLabel: t(REPORT_DELIVERY_I18N_KEYS.reportType),
    channelLabel: t(REPORT_DELIVERY_I18N_KEYS.channel),
    recipientLabel: t(REPORT_DELIVERY_I18N_KEYS.recipient),
    previewLabel: t(REPORT_DELIVERY_I18N_KEYS.preview),
    sendLabel: t(REPORT_DELIVERY_I18N_KEYS.send),
    previewTitle: t(REPORT_DELIVERY_I18N_KEYS.previewTitle),
    subjectLabel: t(REPORT_DELIVERY_I18N_KEYS.subject),
    summaryLabel: t(REPORT_DELIVERY_I18N_KEYS.summary),
    channelNotReady: t(REPORT_DELIVERY_I18N_KEYS.channelNotReady),
    sendSuccess: t(REPORT_DELIVERY_I18N_KEYS.sendSuccess),
    sendFailed: t(REPORT_DELIVERY_I18N_KEYS.sendFailed),
    readOnlyNote: t(REPORT_DELIVERY_I18N_KEYS.readOnlyNote),
    requiresApprovalNote: t(REPORT_DELIVERY_I18N_KEYS.requiresApprovalNote),
    reportTypes,
    channels,
    hasChannels,
  };
}

export function canSendDelivery({ channel, readiness, recipient, sending }) {
  if (sending) return false;
  if (!isChannelReady(readiness, channel)) return false;
  if (String(channel).toLowerCase() === "email" && !String(recipient || "").trim()) return false;
  return true;
}
