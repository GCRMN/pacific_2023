// 1. Import data ----

var site_coords = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords-all");

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

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
  .select('population_count');

// 5. Sum of population within the buffer ----

var pop_by_site = data_pop.map(function(image){
  return image.reduceRegions({
    collection:site_buffer, 
    reducer:ee.Reducer.sum(), 
    scale: 1000
  });
})
.flatten();

// 6. Export the data ----

Export.table.toDrive({
  collection:pop_by_site,
  folder:"GEE",
  fileNamePrefix:"pred_human-pop",
  fileFormat:"CSV",
  description:"pred_human-pop",
  selectors:["system:index", "site_id", "type", "sum"]
});
