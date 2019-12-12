# Predictions
library(tidyverse)
library(sf)
library(magrittr)
mocodata <- read_rds("/Users/brightsideofthedark/Documents/727project/homevalues/mocodata_with_scores.rds")
mocodata %<>% mutate(valueresid = resid(lm(bill_total_median ~ valuescore,data=mocodata,na.action = na.exclude)))

source("data_acs.R")

mocodata %<>% left_join(medage,by="GEOID") %>% 
  left_join(raceeth,by="GEOID") %>% 
  left_join(citizenship,by="GEOID") %>% 
  left_join(moving,by="GEOID") %>% 
  left_join(hhtype,by="GEOID")

set.seed(393)
mocotrain <- mocodata %>% sample_frac(size = .75)

library(randomForest)
mocotrain %<>% select(ends_with("_credit_mean"),valuescore,valueresid,medianage:nuclearfam) %>% st_drop_geometry %>% 
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

