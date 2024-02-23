// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

// 2. Transform NOAA SST data ----

// 2.1 Import data ----

var data_windspeed = ee.ImageCollection("NOAA/CDR/SST_PATHFINDER/V53")
                     .select('wind_speed');

// 2.2 List of years to aggregate ----

var years = ee.List.sequence(1981, 2022);

// 2.3 Map a function to select data within the year and apply mean reducer ----

var data_windspeed = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_windspeed
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.max())
        .set('year', y);
    })
  );

// 3. Mean of SST per year for each site ----

var pred_windspeed = data_windspeed.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_windspeed"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 4. Export the data ----

Export.table.toDrive({
  collection:pred_windspeed,
  folder:"GEE",
  fileNamePrefix:"pred_windspeed",
  fileFormat:"CSV",
  description:"pred_windspeed",
  selectors:["year", "site_id", "type", "pred_windspeed"]
});
