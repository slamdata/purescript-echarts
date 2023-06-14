"use strict";

export const _dark = { name: "dark", value: require("echarts/theme/dark") };
export const _infographic = { name: "infographic", value: require("echarts/theme/infographic") };
export const _macarons = { name: "macarons", value: require("echarts/theme/macarons") };
export const _roma = { name: "roma", value: require("echarts/theme/roma") };
export const _shine = { name: "shine", value: require("echarts/theme/shine") };
export const _vintage = { name: "vintage", value: require("echarts/theme/vintage") };

export function builtInThemeName(theme) {
  return theme.name;
};
