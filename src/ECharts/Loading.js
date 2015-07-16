// module ECharts.Loading

exports.showLoadingImpl = function(json, chart) {
    return function() {
        chart.showLoading(json);
        return chart;
    };
};

exports.hideLoading = function(chart) {
    return function() {
        chart.hideLoading();
        return chart;
    };
};
