// 1. Import ACA data ----

var data_land = ee.Image('CGIAR/SRTM90_V4').selfMask();

// 2. Dataviz to check ----

Map.addLayer(data_eez);
Map.addLayer(data_land);

// 3. Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 4. Use this layer to mask ACA data ----

var data_area = data_area.mask(data_land);

// 5. Calculate coral reef area for each EEZ ----

var reef_area = data_area.reduceRegions({
  collection: data_eez,
  reducer: ee.Reducer.sum(), 
  scale:5,
});

// 6. Export the data ----

Export.table.toDrive({
  collection:land_area,
  folder:"GEE",
  fileNamePrefix:"02_land-area",
  fileFormat:"CSV",
  description:"02_land-area",
  selectors:["TERRITORY1", "sum"]
});
