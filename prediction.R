# Predictions
library(tidyverse)
library(sf)
library(magrittr)
mocodata <- read_rds("mocodata_with_scores.rds")
mocodata %<>% mutate(valueresid = resid(lm(bill_total_median ~ valuescore,data=mocodata,na.action = na.exclude)))

source("data_acs.R")

mocodata %<>% left_join(medage,by="GEOID") %>% 
  left_join(raceeth,by="GEOID") %>% 
  left_join(citizenship,by="GEOID") %>% 
  left_join(moving,by="GEOID") %>% 
  left_join(hhtype,by="GEOID")

# Adding urbanicity data
btsdata <- read_csv("https://www.bts.gov/sites/bts.dot.gov/files/nhts2017/latch_2017-b.csv") %>% 
  select(geocode,urban_group) %>% mutate(geocode = geocode %>% as.character) %>% 
  mutate(ruralness = urban_group) %>% select(-urban_group)

mocodata %<>% left_join(btsdata,by = c("GEOID" = "geocode"))

set.seed(393)
mocotrain <- mocodata %>% sample_frac(size = .75)

library(randomForest)
mocotrain %<>% select(ends_with("_credit_mean"),valuescore,valueresid,medianage:ruralness) %>% st_drop_geometry %>% 
  filter(!is.na(valuescore))

m1 <- randomForest(valueresid ~ . -valuescore, data = mocotrain, importance = TRUE)
varImpPlot(m1)

m2 <- randomForest(valuescore ~ . -valueresid,data=mocotrain, importance = TRUE)
varImpPlot(m2)

library(party)
m1c <- cforest(valueresid~ . -valuescore, mocotrain,
                      controls=cforest_unbiased(ntree=1000, mtry=5))
varimp(m1c)

m2c <- cforest(valuescore~ . -valueresid, mocotrain,
               controls=cforest_unbiased(ntree=1000, mtry=5))
varimp(m2c)

