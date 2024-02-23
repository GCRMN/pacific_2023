// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

var data_chla = ee.ImageCollection("NASA/OCEANDATA/MODIS-Aqua/L3SMI")
                .select('chlor_a');

// 2. List of years to aggregate ----

var years = ee.List.sequence(2002, 2022);

// 3. Map a function to select data within the year and apply mean reducer ----

var data_chla_year = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_chla
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.mean())
        .set('year', y);
    })
  );

// 4. Mean of chl a per year for each site ----

var pred_chla_mean = data_chla_year.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_chla_mean"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 5. Export the data ----

Export.table.toDrive({
  collection:pred_chla_mean,
  folder:"GEE",
  fileNamePrefix:"pred_chla_mean",
  fileFormat:"CSV",
  description:"pred_chla_mean",
  selectors:["year", "site_id", "type", "pred_chla_mean"]
});
