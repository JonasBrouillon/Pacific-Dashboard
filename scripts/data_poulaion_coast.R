pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_POP_COAST,2.0/A..COASTALPOPRF.?endPeriod=2021&dimensionAtObservation=AllDimensions"
population_coast <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))

population_coast$indicator <- "Coastal population"

population_coast <- population_coast%>%
  mutate(range=paste(str_to_lower(range),"from coast"),
         unit_measure="% of the population",text=paste(Name,"in",time_period,":", obs_value,unit_measure)
         )

population_coast$range<- factor(population_coast$range,
                                levels = c("1km from coast", "5km from coast", "10km from coast")
)

write_rds(population_coast,here("data","population_coast_pacific.rds"))
