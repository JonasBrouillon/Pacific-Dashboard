pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_POP_URBAN,1.0/A...?endPeriod=2022&dimensionAtObservation=AllDimensions"
urbanisation <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%
  filter(unit_measure== "PERCENT"&!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))




population_density <- read_rds(here("data","population_density_pacific.rds"))
population_coast <- read_rds(here("data","population_coast_pacific.rds"))



urbanisation <- urbanisation%>%filter(urbanization=="U")%>%
  mutate(indicator="Urban population",
         unit_measure="% of the population",text=paste(Name,"in",time_period,":", obs_value,unit_measure)
  )



geography_pacific <- population_coast%>%
  bind_rows(urbanisation)%>%
  bind_rows(population_density)

write_rds(geography_pacific,here("data","geography_pacific.rds"))
