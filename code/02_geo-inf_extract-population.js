// 1. Import GPW 2020 count ----

var data_pop = ee.Image("CIESIN/GPWv411/GPW_Population_Count/gpw_v4_population_count_rev11_2020_30_sec");

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

Map.setCenter(79.1, 19.81, 3);
Map.addLayer(data_pop, raster_vis, 'population_count');
Map.addLayer(eez);

// 3. Sum of population within the buffer ----

var pop_by_site = data_pop.reduceRegions(eez, ee.Reducer.sum());

// 4. Check output ----

print(pop_by_site);

// 5. Export the data ----

Export.table.toDrive({
  collection:pop_by_site,
  folder:"GEE",
  fileNamePrefix:"01_human-pop",
  fileFormat:"CSV",
  description:"extraction_pop",
  selectors:["TERRITORY1", "sum"]
});
