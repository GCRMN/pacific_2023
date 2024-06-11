// 1. Import data ----

var data_reefs = ee.FeatureCollection("users/jeremywicquart/pacific_2023_reefs");

// 2. Create 100 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(100000); // 100 km  
};

var data_reefs_buffer = data_reefs.map(reef_buffer);
  
// 3. Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(data_reefs_buffer);

// 4. Export the data ----

Export.table.toDrive({
  collection:data_reefs_buffer,
  folder:"GEE",
  fileNamePrefix:"reef_buffer",
  fileFormat:"SHP",
  description:"reef_buffer"
});
