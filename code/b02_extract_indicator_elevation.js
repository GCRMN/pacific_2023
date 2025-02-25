// 1. Import data ----

var data_eez = ee.FeatureCollection("users/jeremywicquart/pacific_2023_eez");

// 2. Import elevation data ----

var elevation = ee.Image('CGIAR/SRTM90_V4').select('elevation');

// 3. Extract maximum elevation ----

var data_results = elevation.reduceRegions({
  reducer: ee.Reducer.mean(),
  collection: data_eez,
  scale: 90,
});

// 4. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_elevation",
  fileFormat:"CSV",
  description:"ind_elevation",
  selectors:["TERRITORY1", "mean"],
});
