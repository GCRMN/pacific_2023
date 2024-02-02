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

// 4. Import MODIS data ----

var modis = ee.ImageCollection("NASA/OCEANDATA/MODIS-Aqua/L3SMI")
  .filterDate('2010-01-01', '2020-01-01')
  .select(['chlor_a']);

// 5. Mean of chl. a between the dates ----

var modis_mean = modis.reduce(ee.Reducer.mean());

// 6. Extract chl. a on each site ----

var data_chla = modis_mean.reduceRegions({
  reducer: ee.Reducer.mean(),
  collection: site_buffer,
  scale: 5000,
});

// 7. Export the data ----

Export.table.toDrive({
  collection:data_chla,
  folder:"GEE",
  fileNamePrefix:"pred_chla",
  fileFormat:"CSV",
  description:"pred_chla",
  selectors:["site_id", "type", "mean"]
});
