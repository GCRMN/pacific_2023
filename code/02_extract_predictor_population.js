// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site_coords");

// 2. Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 3. Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 4. Load data population data ----

var data_pop = ee.Image("CIESIN/GPWv411/GPW_Population_Count/gpw_v4_population_count_rev11_2020_30_sec");

// 5. Sum of population within the buffer ----

var pop_by_site = data_pop.reduceRegions(site_buffer, ee.Reducer.sum());

// 6. Export the data ----

Export.table.toDrive({
  collection:pop_by_site,
  folder:"GEE",
  fileNamePrefix:"pred_human-pop",
  fileFormat:"CSV",
  description:"extraction_pop",
  selectors:["site_id", "sum"]
});
