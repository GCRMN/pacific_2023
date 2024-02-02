// 1. Import data ----

// 1.1 Import NOAA SST data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1985-01-01', '2023-12-31'))
                  .select('sst');

// 1.2 Import site coordinates ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

// 2. Mean SST ----

// 2.1 Mean SST between the dates ----

var data_sst_mean = data_sst.reduce(ee.Reducer.mean());

// 2.2 Extract mean SST for each site ----

var result_sst_mean = data_sst_mean.reduceRegions(site_coords, ee.Reducer.first());

// 2.3 Export the data ----

Export.table.toDrive({
  collection:result_sst_mean,
  folder:"GEE",
  fileNamePrefix:"pred_sst_mean",
  fileFormat:"CSV",
  description:"pred_sst_mean",
  selectors:["site_id", "type", "first"]
});

// 3. SST standard deviation (SD) ----

// 3.1 SD between the dates ----

var data_sst_sd = data_sst.reduce(ee.Reducer.stdDev());

// 3.2 Extract SST SD for each site ----

var result_sst_sd = data_sst_sd.reduceRegions(site_coords, ee.Reducer.first());

// 3.3 Export the data ----

Export.table.toDrive({
  collection:result_sst_sd,
  folder:"GEE",
  fileNamePrefix:"pred_sst_sd",
  fileFormat:"CSV",
  description:"pred_sst_sd",
  selectors:["site_id", "type", "first"]
});

// 4. SST kurtosis ----

// 4.1 Kurtosis between the dates ----

var data_sst_kurtosis = data_sst.reduce(ee.Reducer.kurtosis());

// 4.2 Extract SST kurtosis for each site ----

var result_sst_kurtosis = data_sst_kurtosis.reduceRegions(site_coords, ee.Reducer.first());

// 4.3 Export the data ----

Export.table.toDrive({
  collection:result_sst_kurtosis,
  folder:"GEE",
  fileNamePrefix:"pred_sst_kurtosis",
  fileFormat:"CSV",
  description:"pred_sst_kurtosis",
  selectors:["site_id", "type", "first"]
});

// 5. SST skewness ----

// 5.1 Skewness between the dates ----

var data_sst_skew = data_sst.reduce(ee.Reducer.skew());

// 5.2 Extract SST skewness for each site ----

var result_sst_skew = data_sst_skew.reduceRegions(site_coords, ee.Reducer.first());

// 5.3 Export the data ----

Export.table.toDrive({
  collection:result_sst_skew,
  folder:"GEE",
  fileNamePrefix:"pred_sst_skew",
  fileFormat:"CSV",
  description:"pred_sst_skew",
  selectors:["site_id", "type", "first"]
});
