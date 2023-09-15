// 1. Import elevation data ----

var elevation = ee.Image('CGIAR/SRTM90_V4').select('elevation');

// 2. Extract maximum elevation ----

var data_elevation = elevation.reduceRegions({
  reducer: ee.Reducer.mean(),
  collection: data_eez,
  scale: 90,
});

// 3. Export the data ----

Export.table.toDrive({
  collection:data_elevation,
  folder:"GEE",
  fileNamePrefix:"02_elevation",
  fileFormat:"CSV",
  description:"02_elevation",
  selectors:["TERRITORY1", "mean"]
});
