# This was all work completed prior to the midterm presentation
# Includes all code used to create visualizations for the presentation
# Did not use version control at this point due to how ~exploratory~ it was
# Includes a lot of intermediate junk because of no git, haha
# Now cleaned up to remove extraneous stuff I didn't use

library(readr)
library(tidyverse)
library(magrittr)

# GitHub won't take files over 100mb so gotta pull directly from the source
library(RSocrata)
proptax2018<- read.socrata(
  "https://data.montgomerycountymd.gov/resource/26vm-snmd.csv",
  app_token = Sys.getenv("socrata727token"),
)
names(proptax2018) <- gsub(" ","_",names(proptax2018))

# Copy to a working file (in the very likely case that you mess it up at least once)
propertytax <- proptax2018

library(stringr)

# Adding latlong where available
propertytax %<>% filter(RESIDENCE == "PRINCIPAL RESIDENCE") %>% 
  separate(Location_1,sep="\n\\(",into=c("add","latlong"), remove=FALSE) %>% 
  separate(latlong,sep=",",into=c("addlat","addlong")) %>% 
  mutate(addlong = str_match(addlong,"\\s(\\S+)\\)")[,2]) %>% 
  mutate_at(vars(addlat,addlong),as.numeric)

# Add geocodes from Google Maps
# missinggeo %<>% mutate_geocode(add)
# This might be introducing some error, honestly... some of these seem a lil' sus. But we'll see.

# saveRDS(missinggeo,"geocoded_missings.rds")
missinggeo <- readRDS("geocoded_missings.rds")
missinggeo %<>% rename(addlong = lon, addlat = lat)

# Save this to an external files so I don't have to do it again..

# Load Zillow home value data

zhvisummary <- read_csv("data/Zip_Zhvi_Summary_AllHomes.csv") %>% 
  mutate_at(vars(RegionName),as.numeric)

taxvalues <- inner_join(propertytax,zhvisummary,by=c("ZIP_CODE"="RegionName"))

taxvalues_by_zip <- taxvalues %>% group_by(ZIP_CODE) %>% 
  summarize(med_home_assess = median(ASSESSMENT),
            med_home_val = median(Zhvi),
            diff = median(Zhvi) - median(ASSESSMENT),
            diffp = (median(Zhvi) - median(ASSESSMENT))/median(ASSESSMENT)) %>% 
  mutate(ZIP_CODEchar = as.character(ZIP_CODE))

# Only want the starts from valid zipcodes
validzips <- read_csv("data/Zip_Codes__Map_Service_.csv") %>% 
  select(ZIPCODE) %>% 
  unique %>% 
  mutate_all(as.character) %>% 
  pull

library(tigris)
# zipshapes <- zctas(cb=TRUE,starts_with = validzips,class="sf")
# saveRDS(zipshapes,"zipshapes.rds")
zipshapes <- readRDS("zipshapes.rds")
mocoshape <- county_subdivisions(cb=TRUE,state="MD",county="Montgomery",year=2017,class="sf")
ggplot() + geom_sf(data=mocoshape) + geom_sf_text(aes(label = NAME), colour = "black",data=mocoshape)
library(tmap)
qtm(shp=mocoshape,fill="NAME")

library(sf)
combined <- left_join(zipshapes,taxvalues_by_zip,by=c("ZCTA5CE10" = "ZIP_CODEchar"))

library(ggplot2)
ggplot() +
  geom_sf(data=combined, aes(fill=diff))


# Mapping top/bottom 10 property tax values with leaflet just to start
library(leaflet)
IconSet <- awesomeIconList(
  rich   = makeAwesomeIcon(icon= 'dollar-sign', markerColor = 'red', iconColor = 'white', library = "fa")
)
IconSet2 <- awesomeIconList(
  poor   = makeAwesomeIcon(icon= 'dollar-sign', markerColor = 'blue', iconColor = 'white', library = "fa")
)
topbottom10<-leaflet(zipshapes) %>%
  addTiles() %>% 
  setView(-77.03492,39.1410,zoom=9) %>% 
  addPolygons(popup = ~ZCTA5CE10) %>% 
  addAwesomeMarkers(lat=~addlat,lng=~addlong,label=~as.character(BILL_TOTAL),icon = IconSet,data=propertytax %>% distinct(addlat,addlong,.keep_all=TRUE) %>% top_n(10,wt=BILL_TOTAL)) %>% 
  addAwesomeMarkers(lat=~addlat,lng=~addlong,label=~as.character(BILL_TOTAL),icon = IconSet2,data=propertytax %>% distinct(addlat,addlong,.keep_all=TRUE) %>% filter(!grepl("RIDGE RD",PROPERTY_ADDRESS)) %>% top_n(-10,wt=BILL_TOTAL))

library(htmlwidgets)
saveWidget(topbottom10,"topbottom10.html")
