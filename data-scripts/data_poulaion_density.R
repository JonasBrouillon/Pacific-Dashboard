

pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_POP_DENSITY,1.0/A..?endPeriod=2022&dimensionAtObservation=AllDimensions"
population_density <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))


total_pop <- population_density%>%
  filter(indicator=="POPULATION")%>%
  select(time_period, geo_pict,obs_value)%>%
  rename("total_pop"=3)


population_density <- population_density%>%
  filter(indicator=="POPDENSITY")%>%
  mutate(unit_measure="habs per km²",text=paste(Name,"in",time_period,":", obs_value,unit_measure))



population_density$indicator <- "Population density"





write_rds(population_density,here("data","population_density_pacific.rds"))

write_rds(total_pop,here("data","total_pop_pacific.rds"))
