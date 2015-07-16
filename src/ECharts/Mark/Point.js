// module ECharts.Mark.Point

exports.addMarkPointImpl = function(mp, chart) {
    return function() {
        return chart.addMarkPoint(mp);
    };
};

exports.delMarkPointImpl = function(idx, name, chart) {
    return function() {
        return chart.delMarkPoint(idx, name);
    };
};
