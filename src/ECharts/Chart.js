// module ECharts.Chart

exports.initImpl = function(node, theme) {
    return function() {
        var chart = echarts.init(node, theme);
        return chart;
    };
};

exports.setThemeImpl = function(args, chart) {
    return function() {
        chart.setTheme.apply(chart, args);
    };
};

exports.getZRender = function(chart) {
    return function() {
        return chart.getZRender();
    };
};

exports.resize = function(chart) {
    return function() {
        return chart.resize();
    };
};

exports.refresh = function(chart) {
    return function() {
        return chart.refresh();
    };
};

exports.clear = function(chart) {
    return function() {
        return chart.clear();
    };
};

exports.dispose = function(chart) {
    return function() {
        return chart.dispose();
    };
};
