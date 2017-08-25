exports.on_ = function(chart) {
    return function(eName) {
        return function(callback) {
            return function() {
                return chart.on(eName, function(e) {
                    console.log(e);
                    callback(e)();
                });
            };
        };
    };
};

exports.dispatchAction_ = function(maybeKeys) {
    return function(maybe) {
        return function(rawAction) {
            return function(chart) {
                return function() {
                    var keys = Object.keys(rawAction),
                        i, key, value, keysArr,
                        action = {};
                    for (i = 0; i < keys.length; i++) {
                        key = keys[i];
                        if (typeof maybeKeys[rawAction.type] === "undefined") {
                            keysArr = [];
                        } else {
                            keysArr = maybeKeys[rawAction.type];
                        }
                        if (keysArr.indexOf(key) == -1) {
                            value = rawAction[key];
                        } else {
                            value = maybe(undefined)(function(x) {return x;})(rawAction[key]);
                        }
                        if (typeof value !== "undefined") {
                            action[key] = value;
                        }
                    }
                    chart.dispatchAction(action);
                };
            };
        };
    };
}
