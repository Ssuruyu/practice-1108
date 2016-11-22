function testD3()
{
dataPath = './R/';
dataFile = 'euro.csv';
dataUrl  = dataPath + dataFile;

console.log(dataUrl);
var dataset = [];
var getCSV = d3.csv(dataUrl, 
	function(data) 
	{
		dataset = data.map(function(data) { return data;} );
	});

var svgSet = d3.select(".featured").append("svg").attr("width", 200).attr("height", 200);

var div_data_bind = svgSet.data(dataset);

div_set = div_data_bind.enter().append("div");
div_data_bind.exit().remove();
div_set.text(function(d,i) {
 return i + " / " + d.c;
});
div_set.style("height", "20px");
div_set.style("background", "orange");
div_set.style("margin", "5px");
div_set.style("width", function(d,i) {
return (d.x * 10)+"px";
});

var lineData = [ 
{ "x": 20,   "y": 5},  { "x": 30,  "y": 90},
{ "x": 45,  "y": 10}, { "x": 60,  "y": 5},
{ "x": 10,  "y": 5},  { "x": 100, "y": 30}];

var svgContainer = d3.selectAll(".featured").append("svg").attr("width", 210).attr("height", 500);
svgContainer.append('circle').attr('cx', 60).attr('cy', 60).attr('r', 50);
var lineTest = d3.line().
	x(function(d){return d.x;}).
	y(function(d){return d.y;});

var lineGraph = svgContainer.append("path").attr("d", lineTest(lineData)).attr("stroke", "blue").attr("stroke-width", 2).attr("fill", "none");

circleRadii = [40, 20, 10];
function hiD3() {
	var svgContainer = d3.selectAll(".featured").append("svg").attr("width", 200).attr("height", 100);
	var circles = svgContainer.selectAll("circle")
						.data(circleRadii)
						.enter()
						.append("circle");
	var circleAttributes = circles
						.attr("cx", 50)
						.attr("cy", 50)
						.attr("r", function(d) { return d; })
						.style("fill", function(d){
							var returnColor;
							if (d === 40) { 
								returnColor = "green";
							} else if (d === 20) {
								returnColor = "purple";
							} else if (d === 10) {
								returnColor = "red";
							}
							return returnColor;
						});
}

hiD3();

//The data for our line
var lineData = [ 
		{ "x": 1,   "y": 5},  { "x": 20,  "y": 20},
		{ "x": 40,  "y": 10}, { "x": 60,  "y": 40},
		{ "x": 80,  "y": 5},  { "x": 100, "y": 60}
	];
function lineTest(hi)
{
	//This is the accessor function we talked about above
	var lineFunction = d3.line()
						.x(function(d) { return d.x; })
						.y(function(d) { return d.y; });

	//The SVG Container
	var svgContainer = d3.select(".featured").append("svg")
						.attr("width", 200)
						.attr("height", 200);

	//The line SVG Path we draw
	var lineGraph = svgContainer.append("path")
						.attr("d", lineFunction(hi))
						.attr("stroke", "blue")
						.attr("stroke-width", 2)
						.attr("fill", "none");
}
lineTest(lineData);
	
}