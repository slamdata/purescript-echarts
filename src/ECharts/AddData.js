// module ECharts.AddData

exports.addDataImpl = function(data, chart) {
    return function() {
        return chart.addData.apply(chart, data);
    };
};
