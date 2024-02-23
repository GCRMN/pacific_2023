// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1981-01-01', '2023-12-31'))
                  .select('sst');

// 2. Mean SST per year ----

// 2.1 List of years to aggregate ----

var years = ee.List.sequence(1981, 2023);

// 2.2 Map a function to select data within the year and apply mean reducer ----

var data_sst_year = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_sst
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.mean())
        .set('year', y);
    })
  );

// 2.3 Mean of SST per year for each site ----

var pred_sst_mean = data_sst_year.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_sst_mean"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 2.4 Export the data ----

Export.table.toDrive({
  collection:pred_sst_mean,
  folder:"GEE",
  fileNamePrefix:"pred_sst_mean",
  fileFormat:"CSV",
  description:"pred_sst_mean",
  selectors:["year", "site_id", "type", "pred_sst_mean"]
});

// 3. Max SST per year ----

// 3.1 List of years to aggregate ----

var years = ee.List.sequence(1981, 2023);

// 3.2 Map a function to select data within the year and apply max reducer ----

var data_sst_year = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_sst
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.max())
        .set('year', y);
    })
  );

// 3.3 Max of SST per year for each site ----

var pred_sst_max = data_sst_year.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_sst_max"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 3.4 Export the data ----

Export.table.toDrive({
  collection:pred_sst_max,
  folder:"GEE",
  fileNamePrefix:"pred_sst_max",
  fileFormat:"CSV",
  description:"pred_sst_max",
  selectors:["year", "site_id", "type", "pred_sst_max"]
});

// 4. Min SST per year ----

// 4.1 List of years to aggregate ----

var years = ee.List.sequence(1981, 2023);

// 4.2 Map a function to select data within the year and apply min reducer ----

var data_sst_year = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_sst
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.min())
        .set('year', y);
    })
  );

// 4.3 Min of SST per year for each site ----

var pred_sst_min = data_sst_year.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_sst_min"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 4.4 Export the data ----

Export.table.toDrive({
  collection:pred_sst_min,
  folder:"GEE",
  fileNamePrefix:"pred_sst_min",
  fileFormat:"CSV",
  description:"pred_sst_min",
  selectors:["year", "site_id", "type", "pred_sst_min"]
});

// 5. SST standard deviation (SD) ----

// 5.1 SD between the dates ----

var data_sst_sd = data_sst.reduce(ee.Reducer.stdDev());

// 5.2 Extract SST SD for each site ----

var result_sst_sd = data_sst_sd.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_sst_sd"]),
  collection: site_coords
});

// 5.3 Export the data ----

Export.table.toDrive({
  collection:result_sst_sd,
  folder:"GEE",
  fileNamePrefix:"pred_sst_sd",
  fileFormat:"CSV",
  description:"pred_sst_sd",
  selectors:["site_id", "type", "pred_sst_sd"]
});

// 6. SST kurtosis ----

// 6.1 Kurtosis between the dates ----

var data_sst_kurtosis = data_sst.reduce(ee.Reducer.kurtosis());

// 6.2 Extract SST kurtosis for each site ----

var result_sst_kurtosis = data_sst_kurtosis.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_sst_kurtosis"]),
  collection: site_coords
});

// 6.3 Export the data ----

Export.table.toDrive({
  collection:result_sst_kurtosis,
  folder:"GEE",
  fileNamePrefix:"pred_sst_kurtosis",
  fileFormat:"CSV",
  description:"pred_sst_kurtosis",
  selectors:["site_id", "type", "pred_sst_kurtosis"]
});

// 7. SST skewness ----

// 7.1 Skewness between the dates ----

var data_sst_skew = data_sst.reduce(ee.Reducer.skew());

// 7.2 Extract SST skewness for each site ----

var result_sst_skew = data_sst_skew.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_sst_skewness"]),
  collection: site_coords
});

// 7.3 Export the data ----

Export.table.toDrive({
  collection:result_sst_skew,
  folder:"GEE",
  fileNamePrefix:"pred_sst_skewness",
  fileFormat:"CSV",
  description:"pred_sst_skewness",
  selectors:["site_id", "type", "pred_sst_skewness"]
});
