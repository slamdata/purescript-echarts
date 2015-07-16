// module ECharts.Options

exports.setOptionImpl = function(option, notMerge, chart) {
    return function() {
        chart.setOption(option, notMerge);
        return chart;
    };
};
