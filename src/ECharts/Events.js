// module ECharts.Events

exports.listenImpl = function(event, effHandler, chart) {
    return function() {
        var handler = function(param) {
            effHandler(param)();
        };
    };
    chart.on(event, handler);
    return function() {
        chart.un(event, handler);
    };
};
