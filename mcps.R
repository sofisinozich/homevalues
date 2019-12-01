# School data
library(stringr)
library(tidyverse)
library(pdftools)



# High school info - from MCPS schools at a glance PDF ------------------------
saag_full <-pdf_text("https://www.montgomeryschoolsmd.org/departments/regulatoryaccountability/glance/currentyear/SAAG2018.pdf")
saag_hs <- saag_full[370:421]
# Remove Thomas Edison since non-trad HS
saag_hs <- saag_hs[-c(13,14)]


# Vectorizing it
hsp1<-str_match_all(string=saag_hs,pattern=regex("^ +([A-Za-z- \\.]+High School)[\\s\\S]+Enrollment = ([0-9,]+)[\\s\\S]+ESOL +([0-9.≤]+)[\\s\\S]+FARMS +([0-9.≤]+)[\\s\\S]+Graduation Rate² ³ = ([0-9.≥]+)%[\\s\\S]+Requirements² ³ = ([0-9.≥]+)%",multiline=TRUE))
hsp2<-str_match_all(string=saag_hs,pattern=regex("Total\\nSchool\\s+([0-9]{3})\\s+([0-9]{3})\\s+([0-9]{3,4})[\\s\\S]+Paraeducators\\n[\\s0-9]+([0-9\\.]{4})",multiline=TRUE))
mcpshs <- hsp1[seq(1,length(saag_hs),2)] %>% sapply(.,function(x) x[2:7]) %>% t %>% 
  cbind(hsp2[seq(2,length(saag_hs),2)] %>% sapply(.,function(x) x[2:5]) %>% t)
colnames(mcpshs)<-c("name","enrollment","esol","farms","gradrate","umdreq","satv","satm","satt","apib")

# Cleanup
mcpshs %<>% as_tibble %>% 
  mutate(enrollment = gsub(",","",enrollment),farms = gsub("≤"," ",farms),esol = gsub("≤"," ",esol),gradrate = gsub("≥"," ",gradrate)) %>% 
  mutate_at(vars(enrollment:apib),as.numeric) %>% 
  mutate(short_name = gsub(" High School","",name))
# A quick fix for Blake
mcpshs$short_name[mcpshs$short_name=="James Hubert Blake"]<-"James Blake"

# High school boundaries - neglecting clusters for now, but can come back
library(sf)
hsbounds <- st_read("https://data.montgomeryschoolsmd.org/api/geospatial/hkez-wyb9?method=export&format=GeoJSON")
clusterbounds <- st_read("https://data.montgomeryschoolsmd.org/api/geospatial/3hy6-nzu3?method=export&format=GeoJSON")

library(fuzzyjoin)
hsdata <- left_join(hsbounds,mcpshs,by="short_name",method="dl")

# Need to append assigned HS to each property or tract at least...

# Leave other school info pending for now...