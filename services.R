source("funs.r")
library(sf)
library(lwgeom)
library(magrittr)
library(tidyverse)
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
bikelanes <- st_read("https://data.montgomerycountymd.gov/api/geospatial/icc2-ppee?method=export&format=GeoJSON") 
# ‼️ Don't know what to do with this yet... ‼️

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

# Merge these police
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
broadband <- st_read("https://opendata.arcgis.com/datasets/8c736807b7af4ee593b4566a7e4b4153_0.geojson")
st_geometry(broadband) <- NULL # Don't need block-level geometry when we're going to merge into tract-level
broadband %<>% filter(COUNTYFP10=="031") %>% group_by(TRACTCE10) %>% 
  summarize(avg.num.providers=mean(FREQUENCY,na.rm = TRUE))

upspeed <- st_read("https://opendata.arcgis.com/datasets/b47266bb36004f5c870158bb352c33f5_3.geojson")
st_geometry(upspeed) <- NULL # Don't need block-level geometry when we're going to merge into tract-level
upspeed %<>% filter(COUNTYFP10=="031") %>% group_by(TRACTCE10) %>% 
  summarize(avg.up=mean(Ave_Upload,na.rm = TRUE))

downspeed <- st_read("https://opendata.arcgis.com/datasets/b67dd03b97df4cab8f9e76656f95b574_0.geojson")
st_geometry(downspeed) <- NULL # Don't need block-level geometry when we're going to merge into tract-level
downspeed %<>% filter(COUNTYFP10=="031") %>% group_by(TRACTCE10) %>% 
  summarize(avg.down=mean(Ave_Download,na.rm = TRUE))

# Internet dataset by tract
internet <- left_join(broadband,upspeed) %>% left_join(downspeed) %>% rename(TRACTCE = TRACTCE10)
# DONE ✅

# Libraries etc. ----------------------------------------------------------
# Dataset with publicly reservable spaces - seems to hit most libraries, parks, schools
# This does not include rec centers but seems to include everything else
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
earlycenters <- read.socrata("https://data.montgomerycountymd.gov/resource/ucwu-v65d.csv",
                             app_token = Sys.getenv("socrata727token")) %>% 
                as_tibble
earlycenters %<>% separate(location_1,sep="\n\\(",into =c("add","latlong")) %>% 
  separate(latlong,into=c("lat","lon"),sep=",") %>% 
  mutate(lat = as.numeric(lat),lon = as.numeric(gsub("\\)","",lon)))
earlycenters %<>% st_as_sf(x =., coords = c("lon", "lat"),crs=4326) %>% st_transform(crs=4269)
# Ready for distance calculations ❇️

# Polling places not working as of 11/30? What :| ‼️ ‼️ ‼️
# pollingplaces <- read.socrata("https://data.montgomerycountymd.gov/resource/gtij-bx6r.csv",
#                               app_token = Sys.getenv("socrata727token")) %>% 
#                 as_tibble



# Merging with tracts data ------------------------------------------------
mocotracts_services <- mocotracts

# Easy one - merge the internet data
mocotracts_services %<>% left_join(internet,by="TRACTCE")

# For others - we want distance to nearest if not in, and if in (dist = 0 for at least one),how many
# MARC stations
mocotracts_services$marc_count <- tractcount(mocotracts_services,marc)
mocotracts_services$marc_dist <- tractdist(mocotracts_services,marc)

mocotracts_services$metro_count <- tractcount(mocotracts_services,wmatametro)
mocotracts_services$metro_dist <- tractdist(mocotracts_services,wmatametro)  

mocotracts_services$bus_count <- tractcount(mocotracts_services,busstops)
# mocotracts_services$bus_dist <- tractdist(mocotracts_services,busstops)
# Think we may keep this as counts only given the number of bus stops...

mocotracts_services$hospital_count <- tractcount(mocotracts_services,hospitals)
mocotracts_services$hospital_dist <- tractdist(mocotracts_services,hospitals)

mocotracts_services$police_count <- tractcount(mocotracts_services,policestations)
mocotracts_services$police_dist <- tractdist(mocotracts_services,policestations)

mocotracts_services$fire_count <- tractcount(mocotracts_services,firestations)
mocotracts_services$fire_dist <- tractdist(mocotracts_services,firestations)

mocotracts_services$mva_count <- tractcount(mocotracts_services,mva)
mocotracts_services$mva_dist <- tractdist(mocotracts_services,mva)

mocotracts_services$veip_count <- tractcount(mocotracts_services,veip)
mocotracts_services$veip_dist <- tractdist(mocotracts_services,veip)

mocotracts_services$facil_count <- tractcount(mocotracts_services,countyfacilities)
mocotracts_services$facil_dist <- tractdist(mocotracts_services,countyfacilities)

mocotracts_services$early_count <- tractcount(mocotracts_services,earlycenters)
mocotracts_services$early_dist <- tractdist(mocotracts_services,earlycenters)
