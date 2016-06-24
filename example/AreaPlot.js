// module AreaPlot

exports.linearGradientColor = function(x0, y0, x1, y1, n0, c0, n1, c1) {
  return (function() {
  	return zrender.tool.color.getLinearGradient(
  		x0, y0, x1, y1, [[n0, c0],[n1, c1]])})()
};

exports.numeralFormatter = function(f) {
  return function (v){return numeral(v).format(f)}
};
