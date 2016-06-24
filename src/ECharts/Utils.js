// module ECharts.Utils

function unnull(obj) {
    if (obj == null
        || typeof(obj) != 'object'
        || obj instanceof window['Date']
        || obj instanceof window['Function']
        || obj instanceof window['RegExp']
        || obj instanceof window['CanvasGradient']) {
        return obj;
    }
    var temp = new obj.constructor();
    for (var i in obj) {
        if (obj.hasOwnProperty(i) && obj[i] !== null && obj[i] !== undefined) {
            temp[i] = unnull(obj[i]);
        }
    }
    return temp;
}

exports.unnull = unnull;
