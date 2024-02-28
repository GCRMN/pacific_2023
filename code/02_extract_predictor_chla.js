// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

// 2. Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 3. Apply the function (here 5 km radius) ----

var site_buffer = site_coords.map(bufferPoints(5000, false));

// 4. Load chlorophyll a data ----

var data_chla = ee.ImageCollection("NASA/OCEANDATA/MODIS-Aqua/L3SMI")
                  .filter(ee.Filter.date('2002-01-01', '2022-12-31'))
                  .select('chlor_a');

// 5. Mean chorophyll a ----

// 5.1 Reduce mean between all the images ----

var pred_chla_mean = data_chla.reduce(ee.Reducer.mean());

// 5.2 Extract values for sites ----

var pred_chla_mean = pred_chla_mean.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_chla_mean"]),
  collection: site_buffer,
  scale: 5000,
});

// 5.3 Export the data ----

Export.table.toDrive({
  collection:pred_chla_mean,
  folder:"GEE",
  fileNamePrefix:"pred_chla_mean",
  fileFormat:"CSV",
  description:"pred_chla_mean",
  selectors:["site_id", "type", "pred_chla_mean"]
});

// 6. SD chorophyll a ----

// 6.1 Reduce SD between all the images ----

var pred_chla_sd = data_chla.reduce(ee.Reducer.stdDev());

// 6.2 Extract values for sites ----

var pred_chla_sd = pred_chla_sd.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_chla_sd"]),
  collection: site_buffer,
  scale: 5000,
});

// 6.3 Export the data ----

Export.table.toDrive({
  collection:pred_chla_sd,
  folder:"GEE",
  fileNamePrefix:"pred_chla_sd",
  fileFormat:"CSV",
  description:"pred_chla_sd",
  selectors:["site_id", "type", "pred_chla_sd"]
});
