

pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_NMDI_POP,1.0/A..NMDI0002+NMDI0001._T._T._T..?endPeriod=2020&dimensionAtObservation=AllDimensions"
total_pop <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))


total_pop$indicator<- fct_recode(total_pop$indicator,
                                 "Population growth" = "NMDI0002",
                                 "Population size" = "NMDI0001"
)

total_pop$unit_measure<- fct_recode(total_pop$unit_measure,
                                    "%" = "PERCENT",
                                    "habs" = "N"
)

total_pop <- total_pop %>%
  mutate(text=paste(Name,"in",time_period,":", format(obs_value,big.mark=" "),unit_measure))


write_rds(total_pop,here("data","total_pop_pacific.rds"))
