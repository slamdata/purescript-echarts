// module Events

exports.log = function(a) {
    return function() {
        console.log(a);
    };
};
