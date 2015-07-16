// module ECharts.Image

exports.getDataURLImpl = function(imgType, chart) {
    return function() {
        return chart.getDataURL(imgType);
    };
};

exports.getImageImpl = function(imgType, chart) {
    return function() {
        return chart.getImage(imgType);
    };
};
