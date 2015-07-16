// module ECharts.Connect

exports.connectImpl = function(target, source) {
    return function() {
        source.connect(target);
        return function() {
            source.disconnect(target);
        };
    };
};
