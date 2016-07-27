exports.randomArrayImpl = function(NonEmpty) {
    return function(count) {
        return function() {
            if (count < 1) count = 1;
            var Math = window['Math'];
            var result = [];
            for (var i = 0; i < count; i++) {
                result.push(Math.random());
            }
            result.unshift();
            return NonEmpty(Math.random())(result);
        };
    };
};
