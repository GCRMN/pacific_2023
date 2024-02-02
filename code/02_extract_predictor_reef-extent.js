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

// 4. Load and Allen Coral Atlas (ACA) data ----

var aca_benthic = ee.Image("ACA/reef_habitat/v2_0").select('benthic').selfMask();

// 5. Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 6. Use this layer to mask ACA data ----

var aca_area = data_area.mask(aca_benthic);

// 7. Calculate reef area within each buffer ----

var reef_extent = aca_area.reduceRegions({
  collection: site_buffer,
  reducer: ee.Reducer.sum(), 
  scale:5
});

// 8. Export the data ----

Export.table.toDrive({
  collection:reef_extent,
  folder:"GEE",
  fileNamePrefix:"pred_reef-extent",
  fileFormat:"CSV",
  description:"pred_reef-extent",
  selectors:["site_id", "type", "sum"]
});
