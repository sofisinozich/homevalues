# Munging the crimes data

crimedata <- st_read(paste0("https://data.montgomerycountymd.gov/resource/icn6-v9z3.geojson?",
                         "$where=date_extract_y(start_date)=2018",
                         "&$limit=184568"))


crimedata %<>% st_transform(crs=4269) %>% 
  st_join(mocodata %>% select(GEOID),left=TRUE) %>% # Intersect with the geoids
  filter(crimename1 != "Not a Crime") %>% # We only want actual crimes
  mutate(crimename1 = case_when(crimename1 == "Crime Against Person" ~ "personcrime",
                                crimename1 == "Crime Against Property" ~ "propertycrime",
                                crimename1 == "Crime Against Society" ~ "societycrime",
                                crimename1 == "Other" ~ "othercrime")) %>%
  group_by(GEOID,crimename1) %>% # Group on tract and crime type
  count() %>% 
  st_drop_geometry() %>% # Dropping geometry for simplicity 
  pivot_wider(id_cols = GEOID,names_from=crimename1,values_from=n)
