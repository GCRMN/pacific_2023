// eez = data_eez.shp

// 1. Import ACA data ----

var aca_data = ee.Image('ACA/reef_habitat/v2_0').select('reef_mask').selfMask();

// 2. Dataviz to check ----

Map.addLayer(eez);
Map.addLayer(aca_data);

// 3. Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 4. Use this layer to mask ACA data ----

var aca_area = data_area.mask(aca_data);

// 5. Calculate coral reef area for each EEZ ----

var reef_area = aca_area.reduceRegions({
  collection: eez,
  reducer: ee.Reducer.sum(), 
  scale:5,
});

// 6. Export the data ----

Export.table.toDrive({
  collection:reef_area,
  folder:"GEE",
  fileNamePrefix:"02_reef-area",
  fileFormat:"CSV",
  description:"02_reef-area",
  selectors:["TERRITORY1", "sum"]
});
