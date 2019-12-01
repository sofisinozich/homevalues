# Average home values by tract
propertytax <- readRDS("data/moco_real_property_tax_2018_cleaned.rds")

source("funs.r")
library(tidyverse)
library(magrittr)
library(sf)

assess_by_tract <- propertytax %>% group_by(TRACTCE) %>% 
  summarize(avgassessed = mean(assessment),medassessed=median(assessment))
st_geometry(assess_by_tract) <- NULL

ggplot() + geom_sf(data=assess_by_tract,aes(fill=medassessed))


# Get median home values from the ACS at the tract level
library(tidycensus)
census_api_key(Sys.getenv("censuskey"))

acsmedvalue <- get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B25077")

# Also need population by tract...
tractpop <- get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B01003")

# Merge this with the services dataset!
mocotracts_services %<>% left_join(acsmedvalue,by="GEOID") %>% left_join(tractpop,by="GEOID")

mocotracts_services %>% mutate_at(vars(ends_with("_count")), funs(./estimate.y) )

  
mocotracts_services$basicscore <- mocotracts_services %>% select(ends_with("_dist")) %>% st_set_geometry(NULL) %>% rowMeans

                              
mocotracts_services %<>% left_join(assess_by_tract)
