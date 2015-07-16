// module ECharts.Formatter

exports.func2json = function(fn) {
    return fn;
};

exports.effArrToFn = function(arr) {
    return function(x) {
        arr(x)();
    };
};
