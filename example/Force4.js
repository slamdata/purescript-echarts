// module Force4

exports.clientWidth = function(id) {
    return function() {
        return document.getElementById(id).clientWidth;
    };
};

exports.clientHeight = function(id) {
    return function() {
        return document.getElementById(id).clientHeight;
    };
};
