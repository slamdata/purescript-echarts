// module AreaPlot


exports.linearGradient = function() {
  return zrender.tool.color.getLinearGradient(
  	0, 100, 0, 400, 
  	[[0, 'rgba(255,0,0,0.8)'],[0.8, 'rgba(255,255,255,0.1)']])
};
