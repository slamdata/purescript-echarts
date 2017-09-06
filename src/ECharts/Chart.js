var echarts = require("echarts");

exports.initImpl = function(theme) {
    return function(el) {
        return function() {
            return echarts.init(el,theme);
        };
    };
};

function isObject(o) {
    return null != o &&
      typeof o === 'object' &&
      Object.prototype.toString.call(o) === '[object Object]';
}

exports.registerTheme = function(name) {
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

exports.setOptionImpl = function(option) {
    return function(chart) {
        return function() {
            chart.setOption(option, false, false);
            return {};
        };
    };
};

exports.resetOptionImpl = function(option) {
    return function(chart) {
        return function() {
            console.log("reset option")
            console.log(option)
            chart.setOption(option, true, false);
            return {};
        };
    };
};

exports.resizeImpl = function(chart) {
    return function() {
        chart.resize();
        return {};
    };
};

exports.clearImpl = function(chart) {
    return function() {
        chart.clear();
        return {};
    };
};

exports.disposeImpl = function(chart) {
    return function() {
        chart.dispose();
        return {};
    };
};

exports.getOptionImpl = function(chart) {
    return function() {
        return chart.getOption();
    };
};
