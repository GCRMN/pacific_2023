# 1. Required packages and functions ----

library(tidyverse) # Core tidyverse packages
library(sf) # To plot maps
sf_use_s2(TRUE) # Switch from S2 to GEOS

# 2. Load and modify data tropical storms data (TS) ----

# To skip the second row
all_content <- readLines("data/05_cyclones/ibtracs.ALL.list.v04r00.csv")
all_content <- all_content[-2]
# data_cyclones <- read.csv("./../data/ibtracs.ALL.list.v04r00.csv") # Without skipping the 2nd row

data_ts_points <- read.csv(textConnection(all_content), header = TRUE, stringsAsFactors = FALSE) %>% 
  # Select useful variables
  select(SID, NAME, ISO_TIME, LAT, LON, STORM_SPEED,
         ends_with("WIND"), # Select all columns ending by WIND (wind speed of each RSMC)
         -WMO_WIND) %>% 
  # Coalesce to put all wind speed in a unique column
  mutate(WIND_SPEED = coalesce(MLC_WIND, TOKYO_WIND, CMA_WIND,
                               HKO_WIND, NEWDELHI_WIND, REUNION_WIND, BOM_WIND, 
                               NADI_WIND, WELLINGTON_WIND, DS824_WIND, TD9636_WIND, 
                               TD9635_WIND, NEUMANN_WIND, USA_WIND)) %>% 
  select(-ends_with("WIND")) %>% 
  mutate(WIND_SPEED = WIND_SPEED*1.852, # Convert from knots to km/h
         WIND_SPEED = ifelse(WIND_SPEED < 0, NA, WIND_SPEED), # NA values if below 0
         NAME = ifelse(NAME == "NOT_NAMED", NA, NAME),
         STORM_SPEED = STORM_SPEED*1.852) %>% # Convert from knots to km/h
  rename(ts_id = SID, name = NAME, time = ISO_TIME,
         lat = LAT, long = LON, storm_speed = STORM_SPEED, wind_speed = WIND_SPEED) %>% 
  mutate(long = ifelse(long > 180, long - 360, long)) %>% # Transform long greater than 180
  filter(time > as.Date("1980-01-01") & time <= as.Date("2023-12-31")) %>% # Remove TS before 1980 due to high position uncertainty
  group_by(ts_id) %>% 
  mutate(max_windspeed = max(wind_speed, na.rm = TRUE)) %>% 
  filter(max_windspeed != -Inf) %>% 
  ungroup() %>% 
  mutate(saffir = case_when(max_windspeed < 119 ~ 0,
                            max_windspeed >= 119 & max_windspeed <= 153 ~ 1,
                            max_windspeed > 153 & max_windspeed <= 177 ~ 2,
                            max_windspeed > 177 & max_windspeed <= 210 ~ 3,
                            max_windspeed > 210 & max_windspeed <= 251 ~ 4,
                            max_windspeed > 251 ~ 5)) %>% 
  st_as_sf(., coords = c("long", "lat"), crs = "EPSG:4326") %>% 
  st_make_valid() %>% 
  mutate(time = as_date(time))

# 3. Save TS points data ----

save(data_ts_points, file = "data/05_cyclones/01_cyclones_points.RData")

# 4. Transform points to lines ----

# 4.1 First transformation --

data_ts_lines <- data_ts_points %>% 
  group_by(ts_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  # Remove linestring with only one point (else error with the st_intersection() function)
  mutate(n = str_count(geometry, ",")) %>% 
  filter(n > 1) %>% 
  select(-n) %>% 
  st_make_valid() %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

# 4.2 Apply the correction ----

data_ts_lines_corrected <- data_ts_points %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  mutate(long = if_else(long < 0, long + 360, long)) %>% 
  st_as_sf(., coords = c("long", "lat"), crs = "EPSG:4326") %>% 
  group_by(ts_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  # Remove linestring with only one point (else error with the st_intersection() function)
  mutate(n = str_count(geometry, ",")) %>% 
  filter(n > 1) %>% 
  select(-n) %>% 
  st_make_valid() %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()
  
# 4.3 Visual check --

ggplot() +
  geom_sf(data = data_ts_lines)

# 5. Save TS lines data ----

save(data_ts_lines, file = "data/05_cyclones/01_cyclones_lines.RData")
