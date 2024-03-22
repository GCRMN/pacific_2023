// 1. Use EEZ as the area of interest ----

var data_eez = ee.FeatureCollection("users/jeremywicquart/pacific_2023_eez");

// 2. Load and transform ACA data ----

// 2.1 Load and Allen Coral Atlas (ACA) data ----

var aca_benthic = ee.Image("ACA/reef_habitat/v2_0").select('benthic').selfMask();

// 2.2 Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 2.3 Use this layer to mask ACA data ----

var aca_area = data_area.mask(aca_benthic);

// 3. Create the grid over coral reefs ----

var reef_grid = aca_area.sample({
  region: data_eez.geometry(),
  geometries: true, // To have points
  scale: 100, // In meters
  seed: 10,
  factor: 0.0025 // To reduce the number of sampled sites
});

// 4. Export sites coordinates ----

Export.table.toDrive({
  collection:reef_grid,
  folder:"GEE",
  fileNamePrefix:"site-coords_pred",
  fileFormat:"SHP",
  description:"site-coords_pred",
});

// 5. Visualise the results ----

Map.addLayer(data_eez);
Map.addLayer(reef_grid.style({color: 'red'}));
//Map.addLayer(aca_area);
