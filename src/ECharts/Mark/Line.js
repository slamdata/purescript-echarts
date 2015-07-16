// module ECharts.Mark.Line

exports.addMarkLineImpl = function(ml, chart) {
    return function() {
        return chart.addMarkLine(ml);
    };
};

exports.delMarkLineImpl = function(idx, name, chart) {
    return function() {
        return chart.delMarkLine(idx, name);
    };
};
