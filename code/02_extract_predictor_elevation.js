// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

// 2. Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 3. Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 4. Load elevation data ----

var elevation = ee.Image('CGIAR/SRTM90_V4').select('elevation');

// 5. Extract mean elevation ----

var data_elevation = elevation.reduceRegions({
  reducer: ee.Reducer.mean().setOutputs(["pred_elevation"]),
  collection: site_buffer,
  scale: 90,
});

// 6. Export the data ----

Export.table.toDrive({
  collection:data_elevation,
  folder:"GEE",
  fileNamePrefix:"pred_elevation",
  fileFormat:"CSV",
  description:"pred_elevation",
  selectors:["site_id", "type", "pred_elevation"]
});
