#### Creating the value score
# Run this to get everything!
source("funs.r")
library(tidyverse)
library(magrittr)
library(sf)
library(tigris)

################ DATA SETUP #################

# Montgomery County tract-level shapefiles --------------------------------
mocotracts <- tracts(state="MD",county="Montgomery",year=2018,class="sf")
st_crs(mocotracts)
mocodata <- mocotracts
# Keep blank tracts available if needed

# Services data -----------------------------------------------------------
source("data_services.r")
# This processes the data directly so as long as you have initialized mocodata

# ACS data ----------------------------------------------------------------
source("data_acs.r")
mocodata %<>% left_join(acsdata,by="GEOID")
# There are two tracts that do not have home value information
# ‼️ IMPUTATION?

# Property tax data ------------------------------------------------------------
# source("data_taxes.r") ⬅️ please don't run this again - a lot of initial work with geocoding locations
proptaxdata <- readRDS("data/moco_real_property_tax_2018_cleaned.rds")
proptaxbytractdata<-proptaxdata %>% st_drop_geometry() %>% group_by(GEOID) %>% 
  summarize_at(vars(assessment:bill_total),lst(mean,median))
mocodata %<>% left_join(proptaxbytractdata,by="GEOID")

# Crime data --------------------------------------------------------------
source("data_crimes.r")
mocodata %<>% left_join(crimedata,by="GEOID") %>% 
  mutate_at(vars(othercrime,personcrime,propertycrime,societycrime),~(./population)) # Make all crime data per capita

# High school data -------------------------------------------------------------
source("data_mcps.r")
mocodata %<>% st_intersection(hsdata) %>% 
  group_by_at(vars(-shape_area:-apib,-geometry)) %>% # Group on everything that's not the school variables
  summarize_at(vars(enrollment:apib),mean) %>% # Take the mean of data for all HS that are covered
  ungroup

################### VALUE SCORE CREATION ##################
# Make subscores for different aspects

# School subscore ---------------------------------------------------------
# Subscore for schools - SAT scores + AP/IB percentage + UMD req + grad rate
mocodata %<>% mutate(score_school = (satt/160 + apib + umdreq + gradrate)/4,
                    score_school_quantile = cut(score_school,quantile(score_school),labels=FALSE,include.lowest = TRUE),
                    score_school_esol = score_school/(1-esol/100),
                    score_school_farms = score_school/(1-farms/100),
                    score_school_esol_quantile= cut(score_school_esol,quantile(score_school_esol),labels=FALSE,include.lowest = TRUE),
                    score_school_farms_quantile= cut(score_school_farms,quantile(score_school_farms),labels=FALSE,include.lowest = TRUE)
                    ) 
# ‼️ Need to consider weight to give ESOL/FARMS - particularly FARMS
# High FARMS rates are really destroying high income areas...
# And consider how this scale actually works LOL
# If we want to separate out, want to rescale the SAT scores component - 
# unrealistic to expect 700+ averages from even the best schools


# Crime subscore ----------------------------------------------------------
# A straight average of the types of crime. 
# Will re-calculate this dynamically in the visualization depending on preferred weights.
# ‼️ Think this will just have to be based on n-tiles unless I can think of something better
mocodata %<>% mutate(score_crime = 
                      rowMeans(select(.,othercrime,personcrime,propertycrime,societycrime) 
                                           %>% st_drop_geometry,na.rm=TRUE) %>% 
                       cut(.,quantile(.,probs=seq(0,1,0.1)),labels=FALSE,include.lowest = TRUE) * 10)


# Home value score --------------------------------------------------------
# Does this really need a score? Might as well.
mocodata %>% mutate(score_homevalue = cut(acshomevalue,quantile(acshomevalue,probs=seq(0,1,0.1)),labels=FALSE,include.lowest = TRUE) * 10)
# ‼️ See note above re imputation on the two that don't have home value - won't work until that's resolved :|


# Property taxes ----------------------------------------------------------
# Hadn't thought about it this way but could just put it in like as a n-tile...

# Testing
mocodata %>% transmute(score = rowMeans(select(.,score_school,score_crime)%>% st_drop_geometry)) %>% ggplot + geom_sf(aes(fill=score)
                                                                                                                      + )


