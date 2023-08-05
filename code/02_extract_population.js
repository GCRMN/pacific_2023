// 1. Import GPW count ----

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
                  .select('population_count');

// 2. Empty Collection to fill ----

var ft = ee.FeatureCollection(ee.List([]));

// 3. Create function to extract SST ----

var fill = function(img, ini) {
  // type cast
  var inift = ee.FeatureCollection(ini);

  // gets the values for the points in the current img
  var ft2 = img.reduceRegions({
    reducer: ee.Reducer.sum(),
    collection: data_eez,
    scale: 930,
    });
  
  // gets the date of the img
  var date = img.date().format();

  // writes the date in each feature
  var ft3 = ft2.map(function(f){return f.set("date", date)});

  // merges the FeatureCollections
  return inift.merge(ft3);
};

// 4. Apply the function ----

var newft = ee.FeatureCollection(data_pop.iterate(fill, ft));

// 5. Export the data ----

Export.table.toDrive({
  collection:newft,
  folder:"GEE",
  fileNamePrefix:"01_human-pop",
  fileFormat:"CSV",
  selectors:["TERRITORY1", "date", "sum"],
  description:"extraction_population"
});
