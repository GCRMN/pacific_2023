// 1. Import NOAA SST data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1998-01-01', '1998-12-31'))
                  .select('anom');

// 2. Mean of SST anomaly over the year ----

var mean_data_sst = data_sst.mean();

// 3. Create the rectangle to extract ----

var rectangleBounds = ee.Geometry.Rectangle(
  [-70, -30, 140, 30]
);

// 4. Export the TIF file ---- 

Export.image.toDrive({
  image: mean_data_sst,
  folder:"GEE",
  fileNamePrefix:"sst_anomaly",
  description: 'sst_anomaly',
  region: rectangleBounds,
  maxPixels:1e10,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});
