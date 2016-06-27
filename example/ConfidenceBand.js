// module ConfidenceBand

exports.numeralFormatter = function(f) {
  return function (v){return numeral(v).format(f)}
};

exports.numeralFormatterManipulation = function(f, a, m) {
  return function (v){return numeral(v).add(a).multiply(m).format(f)}
};

exports.dateTimeFormatter = function(fIn, fOut) {
  return function (v){return moment(v, fIn).format(fOut)}
};

exports.toolTipFomatter = function(fn) {
	return	function (params,ticket,callback) {
		var res = params[0].name;
		for (var i = 0, l =  params.length; i < l; i++) {
			res += '<br/>' + params[i].seriesName + ' : ' + fn(params[i].value);		
    	}	
    	return res;
    }
};

exports.toolTipFomatterseriesNames = function(fn, seriesNames) {
	return	function (params,ticket,callback) {
		var res = params[0].name;
		for (var s = 0, sl =  seriesNames.length; s < sl; s++) {
			for (var i = 0, il =  params.length; i < il; i++) {
				if (seriesNames[s]===params[i].seriesName) {
					res += '<br/>' + params[i].seriesName + ' : ' + fn(params[i].value);
				}	
    		}
		} 	
    	return res;
    }
};