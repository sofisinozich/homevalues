# Average home values by tract
source("funs.r")
library(tidyverse)

# Get median home values from the ACS at the tract level
library(tidycensus)
census_api_key(Sys.getenv("censuskey"))

acsmedvalue <- get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B25077") %>% 
  select(-variable,-moe,-NAME) %>% 
  rename(acshomevalue = estimate)

# Also need population by tract...
tractpop <- get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B01003") %>% 
  select(-variable,-moe,-NAME) %>% 
  rename(population = estimate)

# These data are all 5-year estimates (through 2017) - not ideal but most appropriate given the sample sizes

acsdata <- inner_join(acsmedvalue,tractpop)

remove(acsmedvalue,tractpop)