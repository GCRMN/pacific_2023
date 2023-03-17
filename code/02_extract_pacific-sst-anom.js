// 1. Import NOAA SST anom data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1995-01-01', '1995-12-31'))
                  .select('anom');

// 2. Average SST anom over the period ----

var mean_data_sst = data_sst.mean();

// 3. Create area to export ----

var rectangleBoundsA = ee.Geometry.Rectangle(
  [-180, -40, -100, 40]
);

var rectangleBoundsB = ee.Geometry.Rectangle(
  [140, -40, 180, 40]
);

var rectangleBounds = ee.FeatureCollection([rectangleBoundsA, rectangleBoundsB]);

// 4. Export image ----

Export.image.toDrive({
  image: mean_data_sst,
  folder:"GEE",
  fileNamePrefix:"1995",
  description: '1995',
  region: rectangleBounds,
  maxPixels:1e10,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});
