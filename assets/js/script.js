/**
 * ---------------------------------------
 * This demo was created using amCharts 4.
 * 
 * For more information visit:
 * https://www.amcharts.com/
 * 
 * Documentation is available at:
 * https://www.amcharts.com/docs/v4/
 * ---------------------------------------
 */

// Themes begin
am4core.useTheme(am4themes_animated);
// Themes end

// Create map instance
var chart = am4core.create("chartdiv", am4maps.MapChart);

// Set map definition
chart.geodata = am4geodata_worldHigh;

// Set projection
chart.projection = new am4maps.projections.Mercator();

// Center on the groups by default
chart.homeZoomLevel = 6;
chart.homeGeoPoint = { longitude: 10, latitude: 51 };

// Polygon series
var polygonSeries = chart.series.push(new am4maps.MapPolygonSeries());
polygonSeries.exclude = ["AQ"];
polygonSeries.useGeodata = true;
polygonSeries.nonScalingStroke = true;
polygonSeries.strokeOpacity = 0.5;

// Image series
var imageSeries = chart.series.push(new am4maps.MapImageSeries());
var imageTemplate = imageSeries.mapImages.template;
imageTemplate.propertyFields.longitude = "longitude";
imageTemplate.propertyFields.latitude = "latitude";
imageTemplate.nonScaling = true;

var image = imageTemplate.createChild(am4core.Image);
image.propertyFields.href = "imageURL";
image.width = 50;
image.height = 50;
image.horizontalCenter = "middle";
image.verticalCenter = "middle";

var label = imageTemplate.createChild(am4core.Label);
label.text = "{label}";
label.horizontalCenter = "middle";
label.verticalCenter = "top";
label.dy = 20;

imageSeries.data = [{
  "latitude": 60,
  "longitude": -15,
  "imageURL": "https://img.icons8.com/clouds/35/000000/user-male-circle.png",
  "width": 35,
  "height": 35,
},{
  "latitude": 55,
  "longitude": 5,
  "imageURL": "https://img.icons8.com/bubbles/35/000000/user-male.png",
  "width": 35,
  "height": 35,
}, {
  "latitude": 62,
  "longitude": -6,
  "imageURL": "https://img.icons8.com/doodle/35/000000/user-male-circle.png",
  "width": 35,
  "height": 35,
}, {
  "latitude": 60,
  "longitude": 33,
  "imageURL": "https://img.icons8.com/cute-clipart/50/000000/user-male-circle.png",
  "width": 35,
  "height": 35,
}, {
  "latitude": 53,
  "longitude": 15,
  "imageURL": "https://img.icons8.com/plasticine/50/000000/user-male-circle.png",
  "width": 50,
  "height": 50,
}];