"use strict";
const echarts = require("echarts");

export function initImpl(theme) {
    return function(el) {
        return function() {
            return echarts.init(el, theme);
        };
    };
};

function isObject(o) {
    return null != o &&
      typeof o === 'object' &&
      Object.prototype.toString.call(o) === '[object Object]';
}

export function registerTheme(name) {
    return function(theme) {
        return function() {
            // if value is not `undefined` and is not of type `string` then it must be `Foreign` for which we only permit plain Objects.
            if (theme !== undefined && typeof theme !== 'string' && !isObject(theme)) {
                throw new TypeError('Theme must be an Object');
            }
            echarts.registerTheme(name, theme);
        };
    };
};

export function setOptionImpl(option) {
    return function(chart) {
        return function() {
            try {
                chart.setOption(option, false, false);
            } catch (e) {
                console.error(e);
            }
        };
    };
};

export function resetOptionImpl(option) {
    return function(chart) {
        return function() {
            try {
                chart.setOption(option, true, false);
            } catch (e) {
                console.error(e);
            }
        };
    };
};

export function resizeImpl(chart) {
    return function() {
        chart.resize();
        return {};
    };
};

export function clearImpl(chart) {
    return function() {
        chart.clear();
        return {};
    };
};

export function disposeImpl(chart) {
    return function() {
        chart.dispose();
        return {};
    };
};

export function getOptionImpl(chart) {
    return function() {
        return chart.getOption();
    };
};
