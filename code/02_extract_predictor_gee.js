// 1. Combine site with observed data and sites to predict ----

// 1.1 Load sites with observed data and sites to predict ----

var site_obs = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords_obs");
var site_pred = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords_pred");

// 1.2 Data vizualisation ----

Map.addLayer(site_pred);
Map.addLayer(site_obs.style({color: 'red'}));

// 1.3 Merge FeatureCollections ----

var site_coords = site_pred.merge(site_obs);

// 1.4 Export the data as an SHP file ----

Export.table.toDrive({
  collection:site_coords,
  folder:"GEE",
  fileNamePrefix:"site-coords_all",
  fileFormat:"SHP",
  description:"site-coords_all"
});

// 2. Extract predictor "human population living within 5 km radius from the site" ----

// 2.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 2.2 Apply the function (here 5 km radius) ----

var site_buffer = site_coords.map(bufferPoints(5000, false));

// 2.3 Load data population data ----

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
  .select('population_count');

// 2.4 Sum of population within the buffer ----

var pop_by_site = data_pop.map(function(image){
  return image.reduceRegions({
    collection:site_buffer, 
    reducer:ee.Reducer.sum().setOutputs(["pred_population"]), 
    scale: 1000
  });
})
.flatten();

// 2.5 Export the data ----

Export.table.toDrive({
  collection:pop_by_site,
  folder:"GEE",
  fileNamePrefix:"pred_human-pop",
  fileFormat:"CSV",
  description:"pred_human-pop",
  selectors:["system:index", "type", "pred_population"]
});

// 3. Extract predictor "land extent around 10 km radius from the site" ----

// 3.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 3.2 Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 3.3 Load elevation data ----

var elevation = ee.Image('CGIAR/SRTM90_V4').selfMask();

// 3.4 Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 3.5 Use this layer to mask elevation data ----

var data_area = data_area.mask(elevation);

// 3.6 Extract mean elevation ----

var data_elevation = data_area.reduceRegions({
  reducer: ee.Reducer.sum().setOutputs(["pred_land"]),
  collection: site_buffer,
  scale: 90,
});

// 3.7 Export the data ----

Export.table.toDrive({
  collection:data_elevation,
  folder:"GEE",
  fileNamePrefix:"pred_land",
  fileFormat:"CSV",
  description:"pred_land",
  selectors:["site_id", "type", "pred_land"]
});
