# Property tax mangling/munging/etc.
source("funs.r")
library(readr)
library(tidyverse)
library(magrittr)


# Pulling in the data -----------------------------------------------------
library(RSocrata)
# proptax2018<- read.socrata(
#   "https://data.montgomerycountymd.gov/resource/26vm-snmd.csv",
#   app_token = Sys.getenv("socrata727token"),
# )
# names(proptax2018) <- gsub(" ","_",names(proptax2018))
# proptax2018 %<>% as_tibble
# saveRDS(proptax2018,"data/moco_real_property_tax_2018.rds")
proptax2018 <- readRDS("data/moco_real_property_tax_2018.rds")

# Copy to a working file
propertytax <- proptax2018

library(stringr)

# Adding latlong where available and remove those that are not principal residences
propertytax %<>% filter(residence == "PRINCIPAL RESIDENCE") %>% 
  separate(location_1,sep="\n\\(",into=c("add","latlong"), remove=FALSE) %>% 
  separate(latlong,sep=",",into=c("addlat","addlong")) %>% 
  mutate(addlong = str_match(addlong,"\\s(\\S+)\\)")[,2]) %>% 
  mutate_at(vars(addlat,addlong),as.numeric)
# Note that not all of them will have latlong... so let's fix that

# Adding latlong ----------------------------------------------------------
library(ggmap)
# Add latlong from Google Maps API - don't run this again! Just pull from the RDS
# missinggeo <- propertytax %>%
#   filter(is.na(addlong),is.na(addlat)) %>%
#   select(PARCEL_CODE,add) %>%
#   mutate(add=gsub("\\n"," ",add))
# missinggeo %<>% mutate_geocode(add)
# Note that the crs is most likely 4326.
#### This might be introducing some error, honestly... some of these seem a lil' sus. But we'll see.

# saveRDS(missinggeo,"geocoded_missings.rds")
missinggeo <- readRDS("geocoded_missings.rds")
missinggeo %<>% rename(addlong = lon, addlat = lat, parcel_code=PARCEL_CODE)

# Check which ones are still missing
stillmissing <- propertytax %>% left_join(missinggeo, by = "parcel_code") %>% 
  mutate(addlat = coalesce(addlat.x,addlat.y), addlong = coalesce(addlong.x,addlong.y), add = coalesce(add.x,add.y)) %>% 
  select(-addlat.x,-addlat.y,-addlong.x,-addlong.y, -add.x, -add.y) %>% 
  filter(is.na(addlat)) %>% 
  select(parcel_code,location_1:addlong)

# There are only 8 left, so just going to do these manually... faster than yelling at the Google API
# write_csv(stillmissing, "data/manual_fill_missing.csv")
# Don't do that again so you don't have to do this again!!

stillmissing<-read_csv("data/manual_fill_missing.csv")
# Note but ignore that there are two properties at the same address with different assessments

# Add everything back to propertytax
propertytax %>% coalesce_join(missinggeo,by="parcel_code") %>% coalesce_join(stillmissing,by="parcel_code") %>% 
  filter(is.na(addlat))
# 🙌 finally, everyone has a latlong location.
propertytax %<>% coalesce_join(missinggeo,by="parcel_code") %>% coalesce_join(stillmissing,by="parcel_code")

# Double-check that everyone is in Montgomery County
# Note that the lat-longs derived from the original dataset seem to point to the 
# street immediately adjacent rather than the building location, so things will be fuzzed a little
# But I think we can reasonably assume that the crs is 4326

# Two stages: check to see if latlong falls within polygons and then check whether zip is a valid MoCo zip
# Going to be conservative given the fuzziness around the latlongs and projection etc.

library(sf)
library(tigris)
mocoshape_sf <- county_subdivisions(cb=TRUE,state="MD",county="Montgomery",year=2017,class="sf")
st_crs(mocoshape_sf)
# Want to convert this to API call but currently (11/16) hitting 500 error... why?
# mocozips <- read.socrata(
#   "https://data.montgomerycountymd.gov/resource/mmib-2cgz.csv",
#   app_token = Sys.getenv("socrata727token"),
# )
# So using downloaded version instead
validzips <- read_csv("data/Zip_Codes__Map_Service_.csv") %>% select(ZIPCODE) %>% unique %>% pull

propertytax %<>% st_as_sf(x =., coords = c("addlong", "addlat"),crs=4326) %>% st_transform(crs=4269) %>% 
  mutate(inmoco = lengths(st_within(.,mocoshape_sf))) %>% 
  filter(inmoco == 1 | (inmoco == 0 & zip_code %in% validzips)) # Keep anything where latlong is in MoCo OR ZIP is valid MoCo ZIP
# Note that this will take a while.⤴️

# Save into an RDS for convenience
saveRDS(propertytax,"data/moco_real_property_tax_2018_cleaned.rds")