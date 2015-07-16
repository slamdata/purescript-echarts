// module ECharts.Series

exports.setSeriesImpl = function(series, notMerge, chart) {
    return function() {
        return chart.setSeries(series, notMerge);
    };
};
