// module ConfidenceBand

exports.toolTipFomatter = function(fn) {
	return	function (params,ticket,callback) {
		var res = params[0].name;
		for (var i = 0, l =  params.length; i < l; i++) {
			res += '<br/>' + params[i].seriesName + ' : ' + fn(params[i].value);		
    	}	
    	return res;
    }
};