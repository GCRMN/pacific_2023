// 1. Create a polygon defining the area of interest ----

var polygon_area = ee.Feature(ee.Geometry.Polygon(
  [133, 6, 135, 6, 135, 8, 133, 8]
));

// 2. Load and transform ACA data ----

// 2.1 Load and Allen Coral Atlas (ACA) data ----

var aca_benthic = ee.Image("ACA/reef_habitat/v2_0").select('benthic').selfMask();

// 2.2 Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 2.3 Use this layer to mask ACA data ----

var aca_area = data_area.mask(aca_benthic);

// 3. Create the grid over coral reefs ----

var reef_grid = aca_area.sample({
  region: polygon_area.geometry(),
  geometries: true, // if you want points
  scale: 1000
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

Map.addLayer(polygon_area);
Map.addLayer(reef_grid.style({color: 'red'}));
//Map.addLayer(aca_area);
