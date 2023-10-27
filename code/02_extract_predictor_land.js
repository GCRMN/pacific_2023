// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site_coords");

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

var elevation = ee.Image('CGIAR/SRTM90_V4').selfMask();

// 5. Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 6. Use this layer to mask elevation data ----

var data_area = data_area.mask(elevation);

// 7. Extract mean elevation ----

var data_elevation = data_area.reduceRegions({
  reducer: ee.Reducer.sum(),
  collection: site_buffer,
  scale: 90,
});

// 8. Export the data ----

Export.table.toDrive({
  collection:data_elevation,
  folder:"GEE",
  fileNamePrefix:"pred_land",
  fileFormat:"CSV",
  description:"pred_land",
  selectors:["site_id", "sum"]
});
