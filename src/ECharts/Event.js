exports.on_ = function(chart) {
    return function(eName) {
        return function(callback) {
            return function() {
                return chart.on(eName, function(e) {
                    callback(e)();
                });
            };
        };
    };
};
