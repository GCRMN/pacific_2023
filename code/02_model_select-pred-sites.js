// 1. Import data ----

var data_reefs = ee.FeatureCollection("users/jeremywicquart/pacific_2023_reefs");

// 2. Create the random points over coral reefs ----

var site_coords = ee.FeatureCollection.randomPoints(
    {region: data_reefs, points: 5000, seed: 0, maxError: 1});
  
// 3. Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(site_coords.style({color: 'red'}));

// 4. Export the data ----

Export.table.toDrive({
  collection:site_coords,
  folder:"GEE",
  fileNamePrefix:"site-coords_pred",
  fileFormat:"SHP",
  description:"site-coords_pred"
});
