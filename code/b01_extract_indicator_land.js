// 1. Import data ----

var data_eez = ee.FeatureCollection("users/jeremywicquart/pacific_2023_eez");

// 2. Import ACA data ----

var data_land = ee.Image('CGIAR/SRTM90_V4').selfMask();

// 3. Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 4. Use this layer to mask elevation data ----

var data_area = data_area.mask(data_land);

// 5. Calculate coral reef area for each EEZ ----

var data_results = data_area.reduceRegions({
  collection: data_eez,
  reducer: ee.Reducer.sum(), 
  scale:90,
});

// 6. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_land",
  fileFormat:"CSV",
  description:"ind_land",
  selectors:["TERRITORY1", "sum"]
});
