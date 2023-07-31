// 1. Import GPW 2020 count ----

var data_pop_2000 = ee.Image("CIESIN/GPWv411/GPW_Population_Count/gpw_v4_population_count_rev11_2000_30_sec");
var data_pop_2020 = ee.Image("CIESIN/GPWv411/GPW_Population_Count/gpw_v4_population_count_rev11_2020_30_sec");

// 2. Dataviz ----

var raster_vis = {
  "max": 1000.0,
  "palette": [
    "ffffe7",
    "86a192",
    "509791",
    "307296",
    "2c4484",
    "000066"
  ],
  "min": 0.0
};

Map.setCenter(170, 0, 3);
Map.addLayer(data_pop_2000, raster_vis, 'population_count');
Map.addLayer(data_eez);

// 3. Sum of population within the buffer ----

var pop_by_eez_2000 = data_pop_2000.reduceRegions({
  reducer: ee.Reducer.sum(),
  collection: data_eez,
  scale: 930,
});

var pop_by_eez_2020 = data_pop_2020.reduceRegions({
  reducer: ee.Reducer.sum(),
  collection: data_eez,
  scale: 930,
});

// 4. Export the data ----

Export.table.toDrive({
  collection:pop_by_eez_2020,
  folder:"GEE",
  fileNamePrefix:"01_human-pop-2000",
  fileFormat:"CSV",
  description:"extraction_pop",
  selectors:["TERRITORY1", "sum"]
});

Export.table.toDrive({
  collection:pop_by_eez_2020,
  folder:"GEE",
  fileNamePrefix:"01_human-pop-2020",
  fileFormat:"CSV",
  description:"extraction_pop",
  selectors:["TERRITORY1", "sum"]
});
