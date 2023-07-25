// IMPORTS = "data/03_reefs-area_wri/clean/pacific_reef.shp"

// 1. Create a function to make a buffer of 100 km
var reef_buffer = function(feature) {
  return feature.buffer(100000);   
};

// 2. Apply the buffer to each polygon
var reef_buffer = reef.map(reef_buffer);

// 3. Union all buffers
var reef_buffer = reef_buffer.union();

// 4. Visual check
Map.addLayer(reef_buffer);

// 5. Export to SHP
Export.table.toDrive({
  collection: reef_buffer,
  folder:"GEE",
  description: 'reef_buffer',
  fileFormat: 'SHP'
});
