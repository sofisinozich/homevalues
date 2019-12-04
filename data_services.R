source("funs.r")
library(sf)
library(lwgeom)
library(magrittr)
library(tidyverse)
library(httr)
library(jsonlite)
# Some of this data is more or less updated - using what I can find
# Transportation ----------------------------------------------------------
# bus
wmatabus <- st_read("https://opendata.arcgis.com/datasets/e85b5321a5a84ff9af56fd614dab81b3_53.geojson")
wmatabus %<>% select(stopname=BSTP_MSG_TEXT)
rideonbus <- st_read("https://data.montgomerycountymd.gov/api/geospatial/v5jk-z7tf?method=export&format=GeoJSON")
busstops <- rbind(wmatabus,rideonbus) %>% st_transform(crs=4269)
# Ready for counts ❇️

wmatametro <-st_read("https://opendata.arcgis.com/datasets/556208361a1d42c68727401386edf707_111.geojson") %>% 
  st_transform(crs=4269)
# Ready for distance calculations ❇️

# MARC
marc <- st_read("https://opendata.arcgis.com/datasets/e476dcb6dc154683ab63f23472bed5d6_6.geojson") %>% 
  st_transform(crs=4269)
# Ready for distance calculations ❇️

#Bike lanes
bikeways <- st_read("https://data.montgomerycountymd.gov/api/geospatial/icc2-ppee?method=export&format=GeoJSON") %>% 
  st_transform(crs=4269)
# Intersects with the tract information to get the total mileage within each tract
bikeways %<>% st_intersection(mocotracts,.) %>% 
  mutate(distance = st_length(geometry)) %>% 
  group_by(TRACTCE) %>% summarize(totaldistance = sum(distance)) %>% 
  st_drop_geometry # Dropping geometry for simplicity

# Double counting bus stops is OK - indicates additional service!

# Hospital - Police - Fire ------------------------------------------------
hospitals <- st_read("https://data.montgomerycountymd.gov/api/geospatial/qtub-p7bm?method=export&format=GeoJSON") %>% 
  st_transform(crs=4269)
# Ready for distance calculations ❇️

munipolice <- st_read("https://opendata.arcgis.com/datasets/ee30e72e9a2d47828d51d47430a0ed01_2.geojson")
statepolice <- st_read("https://opendata.arcgis.com/datasets/24cc2d1f3acf454f86cb54d048eb3dd7_0.geojson")
countypolice <- st_read("https://opendata.arcgis.com/datasets/c77fb3c801474c678f7a8337c6db45fb_1.geojson")
policestations <- rbind(munipolice,
                        statepolice %>% select(-Barrack,-Area_Served), 
                        countypolice) %>% 
  st_transform(crs=4269)
# Ready for distance calculations ❇️

munifire <- st_read("https://opendata.arcgis.com/datasets/b82fddd2991f42a59a09e6fa787f3c91_2.geojson")
countyfire <- st_read("https://opendata.arcgis.com/datasets/edd45b2aaf174671ab69febe0e29bfbf_1.geojson")
firestations <- rbind(munifire,
                      countyfire) %>% 
  st_transform(crs=4269)
# Ready for distance calculations ❇️

# MVA Locations -----------------------------------------------------------
mva <- st_read("https://opendata.arcgis.com/datasets/8525d72d50eb4ff7b2a13b34452390cf_0.geojson") %>% 
  st_transform(crs=4269)
# Ready for distance calculations ❇️
veip <- st_read("https://opendata.arcgis.com/datasets/438fcaf06c3d4e5495f9b17064a8c7e9_1.geojson") %>% 
  st_transform(crs=4269)
# Ready for distance calculations ❇️

# Internet ✅---------------------------------------------------------------
# Since it's at the block level, ask the API to do the summarizing...
broadband <- fromJSON(
  GET("https://geodata.md.gov/imap/rest/services/UtilityTelecom/MD_BroadbandProviderPerCensusBlock/MapServer/0/query",
      query = list(where='COUNTYFP10=031',f='geojson','groupByFieldsForStatistics'='TRACTCE10',
                   outStatistics='[{"statisticType": "avg","onStatisticField": "FREQUENCY", "outStatisticFieldName": "avg_num_providers"}]')) %>% 
  content(as = "text"), flatten= TRUE)
broadband %<>% use_series("features") 
names(broadband) <- c("TRACTCE","avg_num_providers")# For some reason these don't pipe together nicely but they work fine separately

upspeed <- fromJSON(
  GET("https://geodata.md.gov/imap/rest/services/UtilityTelecom/MD_BroadbandSpeedTest/MapServer/3/query",
      query = list(where='COUNTYFP10=031',f='geojson','groupByFieldsForStatistics'='TRACTCE10',
                   outStatistics='[{"statisticType": "avg","onStatisticField": "Ave_Upload", "outStatisticFieldName": "avg_up"}]')) %>% 
  content(as = "text"), flatten= TRUE)
upspeed %<>% use_series("features") 
names(upspeed) <- c("TRACTCE","avg_up")# For some reason these don't pipe together nicely but they work fine separately

downspeed <- fromJSON(
  GET("https://geodata.md.gov/imap/rest/services/UtilityTelecom/MD_BroadbandSpeedTest/MapServer/0/query",
      query = list(where='COUNTYFP10=031',f='geojson','groupByFieldsForStatistics'='TRACTCE10',
                   outStatistics='[{"statisticType": "avg","onStatisticField": "Ave_Download", "outStatisticFieldName": "avg_down"}]')) %>% 
  content(as = "text"), flatten= TRUE)
downspeed %<>% use_series("features") 
names(downspeed) <- c("TRACTCE","avg_down")# For some reason these don't pipe together nicely but they work fine separately

# Internet dataset by tract
internet <- broadband %>% left_join(upspeed) %>% left_join(downspeed)
# DONE ✅

# Libraries etc. ----------------------------------------------------------
# Dataset with publicly reservable spaces - seems to hit most libraries, parks, schools
# This does not include rec centers but seems to include everything else
# There are cleaner ways to do this but I did this first and it works so... :| Will revisit if time allows

library(RSocrata)
countyfacilities<- read.socrata(
  "https://data.montgomerycountymd.gov/resource/mhkm-fwjj.csv",
  app_token = Sys.getenv("socrata727token")
) %>% as_tibble
# Cleanup
countyfacilities %<>% separate(geolocation,sep="\n\\(",into =c("add","latlong")) %>% 
  separate(latlong,into=c("lat","lon"),sep=",") %>% 
  mutate(lat = as.numeric(lat),lon = as.numeric(gsub("\\)","",lon)))

countyfacilities %>% filter(is.na(add) | is.na(lat) | is.na(lon))
# A couple don't have latlong...

# Use Google Maps again to get the latlong for these since they all have parsable addresses
library(ggmap)
register_google(Sys.getenv("googlekey"))
countyfacilities %<>% filter(is.na(add) | is.na(lat) | is.na(lon)) %>% select(add) %>% 
  mutate_geocode(add) %>% coalesce_join(y=countyfacilities,x=.,by="add")

# Should filter out MCPS to avoid double counting schools
countyfacilities %<>% filter(facility_owner != "Montgomery County Public Schools")
# MCPS is a lot of these facilities, heh...

# Making it into an sf
countyfacilities %<>% st_as_sf(x =., coords = c("lon", "lat"),crs=4326) %>% st_transform(crs=4269)
# Ready for distance calculations ❇️

# Voting ------------------------------------------------------------------
earlycenters <- st_read("https://data.montgomerycountymd.gov/resource/ucwu-v65d.geojson") %>% 
  st_transform(crs=4269)
earlycenters %<>% select(location_1_address,zipcode,name,data)
# Dropping a lot of random extra information that came along
# Ready for distance calculations ❇️

# Polling places
pollingplaces <- fromJSON(GET("https://data.montgomerycountymd.gov/resource/gtij-bx6r.json") 
                          %>% content(as="text"),flatten=TRUE)
pollingplaces %<>% select(-shape.latitude,-shape.needs_recoding,-shape.longitude,-x,-y,-objectid) %>% 
  mutate_at(vars(ends_with("_lat")),as.numeric) # Dropping information about the districts
pollingplaces %<>% st_as_sf(crs = 4326,coords = c("x_lat","y_lat")) %>% st_transform(crs=4269)
# Ready for distance calculations ❇️


# Merging with tracts data ------------------------------------------------
# Easy one - merge the internet data
mocodata %<>% left_join(internet,by="TRACTCE")
# Also easy, merge in the length of bikeways
mocodata %<>% left_join(bikeways,by="TRACTCE")

# For others - we want distance to nearest if not in, and if in (dist = 0 for at least one),how many
# MARC stations
mocodata$marc_count <- tractcount(mocodata,marc)
mocodata$marc_dist <- tractdist(mocodata,marc)
# Metro stations
mocodata$metro_count <- tractcount(mocodata,wmatametro)
mocodata$metro_dist <- tractdist(mocodata,wmatametro)  
# Bus stops
mocodata$bus_count <- tractcount(mocodata,busstops)
# mocodata$bus_dist <- tractdist(mocodata,busstops)
# Think we may keep this as counts only given the number of bus stops...
# Hospitals
mocodata$hospital_count <- tractcount(mocodata,hospitals)
mocodata$hospital_dist <- tractdist(mocodata,hospitals)
# Police
mocodata$police_count <- tractcount(mocodata,policestations)
mocodata$police_dist <- tractdist(mocodata,policestations)
# Fire stations
mocodata$fire_count <- tractcount(mocodata,firestations)
mocodata$fire_dist <- tractdist(mocodata,firestations)
# MVA locations
mocodata$mva_count <- tractcount(mocodata,mva)
mocodata$mva_dist <- tractdist(mocodata,mva)
# VEIP locations
mocodata$veip_count <- tractcount(mocodata,veip)
mocodata$veip_dist <- tractdist(mocodata,veip)
# Community facilities
mocodata$facil_count <- tractcount(mocodata,countyfacilities)
mocodata$facil_dist <- tractdist(mocodata,countyfacilities)
# Early voting centers
mocodata$early_count <- tractcount(mocodata,earlycenters)
mocodata$early_dist <- tractdist(mocodata,earlycenters)
# Polling places
mocodata$poll_count <- tractcount(mocodata,pollingplaces)
mocodata$poll_dist <- tractdist(mocodata,pollingplaces)

# Cleanup crew 🗑
remove(wmatabus,rideonbus,busstops,wmatametro,marc,
       hospitals,munipolice,countypolice,statepolice,policestations,munifire,countyfire,firestations,
       mva,veip,
       broadband,upspeed,downspeed,internet,
       countyfacilities,
       earlycenters,pollingplaces,
       bikeways)
