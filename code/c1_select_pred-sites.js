// 1. Import data ----

var data_reefs = ee.FeatureCollection("users/jeremywicquart/pacific_2023_reefs");

// 2. Create the random points over coral reefs ----

var site_coords = ee.FeatureCollection.randomPoints(
    {region: data_reefs, points: 10000, seed: 0, maxError: 1})
    .map(function (feature) {
      return feature.set("type", "pred"); // add type of points (pred = prediction)
    });
  
// 3. Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(site_coords.style({color: 'red'}));

// 4. Export the data ----

// 4.1 As an SHP file (for R) ----

Export.table.toDrive({
  collection:site_coords,
  folder:"GEE",
  fileNamePrefix:"site-coords_pred",
  fileFormat:"SHP",
  description:"site-coords_pred"
});

// 4.2 As an asset (for GEE) ----

Export.table.toAsset({
  collection:site_coords,
  description:"pacific_2023_site-coords_pred"
});
