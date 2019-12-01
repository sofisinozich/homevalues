# Property tax mangling/munging/etc.
source("funs.r")
library(readr)
library(tidyverse)
library(magrittr)


# Pulling in the data -----------------------------------------------------
# library(RSocrata)
# proptax2018<- read.socrata(
#   "https://data.montgomerycountymd.gov/resource/26vm-snmd.csv",
#   app_token = Sys.getenv("socrata727token")
# )
# names(proptax2018) <- gsub(" ","_",names(proptax2018))
# proptax2018 %<>% as_tibble
# This takes a while, so...
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
# stillmissing <- propertytax %>% left_join(missinggeo, by = "parcel_code") %>% 
#   mutate(addlat = coalesce(addlat.x,addlat.y), addlong = coalesce(addlong.x,addlong.y), add = coalesce(add.x,add.y)) %>% 
#   select(-addlat.x,-addlat.y,-addlong.x,-addlong.y, -add.x, -add.y) %>% 
#   filter(is.na(addlat)) %>% 
#   select(parcel_code,location_1:addlong)

# There are only 8 left, so just going to do these manually... faster than yelling at the Google API
# write_csv(stillmissing, "data/manual_fill_missing.csv")
# Don't do that again so you don't have to do this again!!

stillmissing<-read_csv("data/manual_fill_missing.csv")
# Note but ignore (for now) that there are two properties at the same address with different assessments

# Add everything back to propertytax
propertytax %<>% coalesce_join(missinggeo,by="parcel_code") %>% coalesce_join(stillmissing,by="parcel_code")
propertytax %>% filter(is.na(addlat))
# 🙌 finally, everyone has a latlong location.


# Checking county ---------------------------------------------------------
# Double-check that everyone is in Montgomery County
# Note that the lat-longs derived from the original dataset seem to point to the 
# street immediately adjacent rather than the building location, so things will be fuzzed a little

library(sf)
library(tigris)
mocotracts <- tracts(state="MD",county="Montgomery",year=2018,class="sf") # Will use tract-level analysis later!
st_crs(mocotracts)
# All research leads to Google Maps latlong being 4326, so rolling with that
propertytax %<>% st_as_sf(x =., coords = c("addlong", "addlat"),crs=4326) %>% st_transform(crs=4269) %>% 
  st_join(.,mocotracts,join=st_within)
# Note that this will take a while.⤴️

propertytax %>% filter(is.na(NAME))
# Removing ~200 seems reasonable - enough to get rid of the obvious ones but also not worth probing into much further.
propertytax %<>% filter(!is.na(NAME))

# Save into an RDS for convenience

sveRDS(propertytax,"data/moco_real_property_tax_2018_cleaned.rds")