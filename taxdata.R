# Property tax mangling/munging/etc.
source("homevalues/funs.r")
library(readr)
library(tidyverse)
library(magrittr)

proptax2018 <- read_csv("data/Real_Property_Tax_2018.csv")
names(proptax2018) <- gsub(" ","_",names(proptax2018))

# Copy to a working file
propertytax <- proptax2018

library(stringr)

# Adding latlong where available
propertytax %<>% filter(RESIDENCE == "PRINCIPAL RESIDENCE") %>% 
  separate(Location_1,sep="\n\\(",into=c("add","latlong"), remove=FALSE) %>% 
  separate(latlong,sep=",",into=c("addlat","addlong")) %>% 
  mutate(addlong = str_match(addlong,"\\s(\\S+)\\)")[,2]) %>% 
  mutate_at(vars(addlat,addlong),as.numeric)
# Note that not all of them will have latlong... don't really know why... :|

# library(RDSTK) Good try but not accurate enough
# vstreet2coordinates <- Vectorize(street2coordinates)
# This is annoying to vectorize.


# Adding latlong ----------------------------------------------------------
# Add latlong from Google Maps API - don't run this again! Just pull from the RDS
# missinggeo <- propertytax %>%
#   filter(is.na(addlong),is.na(addlat)) %>%
#   select(PARCEL_CODE,add) %>%
#   mutate(add=gsub("\\n"," ",add))
# missinggeo %<>% mutate_geocode(add)

#### This might be introducing some error, honestly... some of these seem a lil' sus. But we'll see.

# saveRDS(missinggeo,"geocoded_missings.rds")
missinggeo <- readRDS("geocoded_missings.rds")
missinggeo %<>% rename(addlong = lon, addlat = lat)

# Check which ones are still missing
stillmissing <- propertytax %>% left_join(missinggeo, by = "PARCEL_CODE") %>% 
  mutate(addlat = coalesce(addlat.x,addlat.y), addlong = coalesce(addlong.x,addlong.y), add = coalesce(add.x,add.y)) %>% 
  select(-addlat.x,-addlat.y,-addlong.x,-addlong.y, -add.x, -add.y) %>% 
  filter(is.na(addlat)) %>% 
  select(PARCEL_CODE,Location_1:addlong)

# There are only 8 left, so just going to do these manually... faster than yelling at the Google API
# write_csv(stillmissing, "homevalues/manual_fill_missing.csv")
# Don't do that again so you don't have to do this again!!

stillmissing <- read_csv("manual_fill_missing.csv")
# Note but ignore that there are two properties at the same address with different assessments

# Add everything back to propertytax
propertytax %>% coalesce_join(stillmissing,by="PARCEL_CODE") %>% filter(is.na(addlat))
# YOU STOPPED HERE