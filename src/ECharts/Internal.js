exports.unsafeSetField = function(obj) {
    return function(key) {
        return function(val) {
            obj[key] = val;
            return obj;
        };
    };
};

exports.emptyObject = function() {
    return {};
};

exports.undefinedValue = undefined;
