// module Utils

exports.randomLst = function(count) {
    return function() {
        var Math = window['Math'];
        var result = [];
        for (var i = 0; i < count; i++) {
            result.push(Math.random());
        }
        return result;
    };
};
