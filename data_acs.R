# Average home values by tract
source("funs.r")
library(tidyverse)

# Get median home values from the ACS at the tract level
library(tidycensus)
census_api_key(Sys.getenv("censuskey"))

acsmedvalue <- get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B25077", cache_table = TRUE) %>% 
  select(-variable,-moe,-NAME) %>% 
  rename(acshomevalue = estimate)

# Also need population by tract...
tractpop <- get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B01003", cache_table = TRUE) %>% 
  select(-variable,-moe,-NAME) %>% 
  rename(population = estimate)


# These data are all 5-year estimates (through 2017) - not ideal but most appropriate given the sample sizes

acsdata <- inner_join(acsmedvalue,tractpop)

remove(acsmedvalue,tractpop)


# get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B01001H")  %>% 
#   select(-variable,-moe,-NAME) %>% 
#   rename(whitepop = estimate)


# Get other tract-level data
# Demos, household size, urbanity, what else?

medage <- get_acs(geography = "tract", state = "MD", county="Montgomery",table = "B01002", cache_table = TRUE) %>% 
  filter(variable == "B01002_001") %>% 
  select(-variable,-moe,-NAME) %>% 
  rename(medianage = estimate)

raceeth <- get_acs(geography = "tract", state = "MD", county="Montgomery", table = "B03002", cache_table = TRUE) %>% 
  filter(variable %in% c("B03002_001", "B03002_002", "B03002_003", "B03002_004", "B03002_012")) %>% 
  mutate(variable = recode(variable,"B03002_001" = "total", "B03002_002" = "nonhisptotal", "B03002_003" = "nonhispwhite", "B03002_004" = "nonhispblack", "B03002_012" = "hisp")) %>% 
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = estimate) %>% 
  mutate(nonhispother = nonhisptotal - nonhispwhite - nonhispblack) %>%
  mutate_at(vars(nonhispwhite:nonhispother), ~(./total)) %>% 
  select(-total, -nonhisptotal)

citizenship <- get_acs(geography = "tract", state = "MD", county="Montgomery",table="B05001", cache_table = TRUE) %>% 
  pivot_wider(id_cols=GEOID,names_from = variable, values_from = estimate) %>% 
  transmute(GEOID=GEOID, notbirthcit = (B05001_005 + B05001_006)/B05001_001, naturalized = B05001_005/B05001_001, noncit = B05001_006/B05001_001)

moving <- get_acs(geography = "tract", state = "MD", county="Montgomery",table = "B07003", cache_table = TRUE) %>%  
  pivot_wider(id_cols = GEOID, names_from=variable, values_from=estimate) %>%
  transmute(GEOID=GEOID, samehouse = B07003_004/B07003_001, samecounty = B07003_007/B07003_001, samestate = B07003_010/B07003_001, samecountry = B07003_013/B07003_001, othercountry = B07003_016/B07003_001)


# Income

# HHsize

# HHtype
hhtype <- get_acs(geography = "tract", state = "MD", county="Montgomery",table="B11001", cache_table = TRUE) %>% 
  pivot_wider(id_cols = GEOID, names_from=variable, values_from=estimate) %>%
  transmute(GEOID=GEOID,nuclearfam = B11001_003/B11001_001)
