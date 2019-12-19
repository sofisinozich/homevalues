#### Creating the value score
# Run this to get everything!
source("funs.r")
library(tidyverse)
library(magrittr)
library(sf)
library(tigris)
library(scales)

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
# Imputation farther down...

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
mocodata %<>% mutate(score_school = ((satt/160 + apib + umdreq + gradrate)/4) %>% rescale(to=c(0,100)),
                    score_school_esol = (score_school/(1-esol/100)) %>% rescale(to=c(0,100)),
                    score_school_farms = (score_school/(1-farms/100)) %>% rescale(to=c(0,100))
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
mocodata %<>% mutate(score_crime = 110 - 
                       rowMeans(select(.,othercrime,personcrime,propertycrime,societycrime)
                                           %>% st_drop_geometry,na.rm=TRUE) %>%
                       cut(.,quantile(.,probs=seq(0,1,0.1)),labels=FALSE,include.lowest = TRUE) * 10)
# Subtraction term because higher crime is worse

# There's one outlier that's messing this up so leave ^ that for now
# mocodata %>% mutate(score_crime = 
#                       rowMeans(select(.,othercrime,personcrime,propertycrime,societycrime) %>% 
#                       st_drop_geometry,na.rm=TRUE) %>% scale %>% as.vector %>% rescale(to=c(0,100))) %>% select(ends_with("crime"))

# Home value score --------------------------------------------------------
# Does this really need a score? Might as well.
mocodata %<>%
  mutate(score_homevalue = cut(acshomevalue,quantile(acshomevalue,probs=seq(0,1,0.1),na.rm=TRUE),labels=FALSE,include.lowest = TRUE) * 10)
# Alternatively, a straight scaling
# Need to impute two values ‼️ ‼️ ‼️ ‼️ ‼️ 
homevalue_imputation <- mocodata %>% lm(data =., acshomevalue ~ population + assessment_median) # Very basic
predict(homevalue_imputation,mocodata %>% filter(is.na(acshomevalue)))
mocodata %<>% mutate(score_homevalue = rescale(acshomevalue,to=c(0,100)))


# Services score ----------------------------------------------------------
# Higher counts, internet, bikeways, etc. = good
# Lower distance = good
# But counts need to be standardized by population and/or area

# Transport subscore - public transit and bikeways
# Scale station counts by population, bikeways/distances by land area (not water area)
# Slightly less weight given to bikeways given their practicality
mocodata %<>% mutate(score_services_transport = 
                         (1.25*scale((marc_count+metro_count+bus_count)/population) + # Number of transit stops (scaled by pop)
                         1.25*scale(ifelse(marc_dist > metro_dist,-metro_dist,-marc_dist)) + # Distance to nearest station (MARC or Metro)
                         0.5*scale(totaldistance/ALAND)) %>% rescale(to=c(0,100)))
# Hospital/fire/police subscore
mocodata %<>% mutate(score_services_emergency = 
                         (scale(hospital_count/population) + scale(police_count/population) + scale(fire_count/population) +
                         scale(-hospital_dist) + scale(-police_dist) + scale(-fire_dist)) %>% rescale(to=c(0,100)))
# Other facilities subscore
# community faclities, MVA/VEIP, polling places
mocodata %<>% mutate(score_services_facilities = 
                         (scale(mva_count/population) + scale(veip_count/population) + scale(facil_count/population) + scale(early_count/population) + scale(poll_count/population) +
                         scale(-mva_dist) + scale(-veip_dist) + scale(-facil_dist) + scale(-early_dist) + scale(-poll_dist)) %>% rescale(to=c(0,100)))
# Internet subscore
# Weighted such that better down speeds are most important - think this reflects use
mocodata %<>% mutate(score_services_internet = 
                         .5*scale(avg_num_providers) + 
                         1*scale(avg_up) + 
                         1.5*scale(avg_down) %>% rescale(to=c(0,100)))

mocodata %<>% mutate(score_services = (score_services_transport + score_services_emergency + score_services_facilities + score_services_transport)/4)



# Final score -------------------------------------------------------------
mocodata %<>% mutate(valuescore = (score_services + score_school + score_crime + score_homevalue)/4)

mocodata %>% ggplot(aes(x=valuescore,y=bill_total_median)) + geom_point() + 
  geom_smooth(method ="lm", formula = y ~ x, color="red")

mocodata %<>% mutate(valuerank = resid(lm(bill_total_median ~ valuescore,data=mocodata,na.action = na.exclude)) %>% rank(na.last="keep"))
# Ranked list
mocodata %>% ggplot + geom_sf(aes(fill=valuerank)) +
  geom_sf_label(data = mocodata %>% top_n(wt=valuerank,-5), aes(label = valuerank), color="green") +
  geom_sf_label(data = mocodata %>% top_n(wt=valuerank,5), aes(label = valuerank), color ="red")

write_rds(mocodata,"data/mocodata_with_scores.rds")
  

