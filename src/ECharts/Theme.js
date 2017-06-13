"use strict";

exports._dark = { name: "dark", value: require("echarts/theme/dark") };
exports._infographic = { name: "infographic", value: require("echarts/theme/infographic") };
exports._macarons = { name: "macarons", value: require("echarts/theme/macarons") };
exports._roma = { name: "roma", value: require("echarts/theme/roma") };
exports._shine = { name: "shine", value: require("echarts/theme/shine") };
exports._vintage = { name: "vintage", value: require("echarts/theme/vintage") };

exports.builtInThemeName = function (theme) {
  return theme.name;
};
