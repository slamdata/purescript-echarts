function addData(area, axis, setKey) {
    if (axis.type !== "value") {
        area[setKey] = axis.data;
    } else {
        area[setKey] = [ ];
    }
    return;
}

// ECharts doesn't send x-axis data in brush events but we need it
// because sorting algo may be opaque
function addAxisData(option, e, axisKey, setKey, axisIndexKey) {
    var i, j, gridIndex;
    for (i = 0; i < e.areas.length; i++) {
        gridIndex = parseInt(e.areas[i].panelId.match(/\d+$/g)[0]);
        e.areas[i].gridIndex = gridIndex;
        if (typeof option === "undefined") {
            e.areas[i][setKey] = [ ];
            e.areas[i][axisIndexKey] = 0;
        } else if (typeof option[axisKey].length === "undefined" && gridIndex === 0) {
            addData(e.areas[i], option[axisKey], setKey);
            e.areas[i][axisIndexKey] = 0;
        } else if (option[axisKey].length === 1 && gridIndex === 0) {
            addData(e.areas[i], option[axisKey][0], setKey);
            e.areas[i][axisIndexKey] = 0;
        } else {
            for (j = 0; j < option[axisKey].length; j++) {
                if (option[axisKey][j].gridIndex === gridIndex) {
                    addData(e.areas[i], option[axisKey][j], setKey);
                    e.areas[i][axisIndexKey] = j;
                }
            }
        }
    }
    return e;
}

function addXAxisData(option, e) {
    return addAxisData(option, e, "xAxis", "xAxisData", "xAxisIndex");
}

function addYAxisData(option, e) {
    return addAxisData(option, e, "yAxis", "yAxisData", "yAxisIndex");
}

function addSeriesAndTitles(option, e) {
    var i, xIx, yIx, serie, ai, area;
    for (ai = 0; ai < e.areas.length; ai++) {
        area = e.areas[ai];
        area.serieNames = [];
        area.serieIndices = [];
        for (i = 0; i < option.series.length; i++) {
            serie = option.series[i];
            xIx = serie.xAxisIndex || 0;
            yIx = serie.xAxisIndex || 0;
            if (xIx == area.xAxisIndex && yIx == area.yAxisIndex) {
                area.serieNames.push(serie.name);
                area.serieIndices.push(i);
            }
        }
    }
    var titles, titleTexts = [];
    if (!option.title) {
        titles = [];
    } else if (typeof option.title.length === "undefined") {
        titles = [option.title];
    } else {
        titles = option.title;
    }
    for (i = 0; i < titles.length; i++) {
        if (titles[i].text) titleTexts.push(titles[i].text);
    }
    e.titleTexts = titleTexts;
    return e;
}

function addAll(option, e) {
    return addSeriesAndTitles(option, addYAxisData(option, addXAxisData(option, e)));
}

exports.on_ = function(chart) {
    return function(eName) {
        return function(callback) {
            return function() {
                return chart.on(eName, function(e) {
                    if (eName === "brush") {
                        addAll(this.getOption(), e);
                    }
                    callback(e)();
                    });
            };
        };
    };
};

exports.dispatchAction_ = function(action) {
    return function(chart) {
        return function() {
            try {
                return chart.dispatchAction(action);
            } finally {
                return;
            }
        };
    };
}
