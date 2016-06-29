// module AreaPlot

exports.linearGradientColor = function(a) {
  return zrender.tool.color.getLinearGradient(
  		a.x0, a.y0, a.x1, a.y1, [[a.s0, a.sc0],[a.s1, a.sc1]])
};

exports.numeralFormatter = function(f) {
  return function (v){return numeral(v).format(f)}
};





