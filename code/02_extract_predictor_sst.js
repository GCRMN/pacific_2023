// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

// 2. Transform NOAA SST data ----

// 2.1 Import data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1981-01-01', '2023-12-31'))
                  .select('sst');

// 2.2 List of years to aggregate ----

var years = ee.List.sequence(1981, 2023);

// 2.3 Map a function to select data within the year and apply mean reducer ----

var data_sst = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_sst
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.mean())
        .set('year', y);
    })
  );

// 3. Mean of SST per year for each site ----

var pred_sst_mean = data_sst.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.mean().setOutputs(["pred_sst_mean"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 4. Export the data ----

Export.table.toDrive({
  collection:pred_sst_mean,
  folder:"GEE",
  fileNamePrefix:"pred_sst_mean",
  fileFormat:"CSV",
  description:"pred_sst_mean",
  selectors:["year", "site_id", "type", "pred_sst_mean"]
});
