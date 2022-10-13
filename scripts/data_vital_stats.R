pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_VITAL,1.0/A...?startPeriod=2006&endPeriod=2020&dimensionAtObservation=AllDimensions"
vital_stats <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))

vital_stats$indicator <- vital_stats$indicator %>%
  fct_recode(
    "Crude birth rate" = "CBR",
    "Crude death rate" = "CDR",
    "Infant mortality rate" = "IMR",
    "Life expectancy at birth" = "LEB",
    "Total fertility rate" = "TFR",
    "Teenage fertility rate" = "TNFR"
  )

vital_stats$sex<- vital_stats$sex %>%
  fct_recode(
    "Total" = "_T",
    "Female" = "F",
    "Male" = "M"
  )


vital_stats$unit_measure<- vital_stats$unit_measure %>%
  fct_recode(
    "children per woman" = "N",
    "per 1000 live births" = "PER_1000_LIVE_BIRTHS",
    "per 1000 people" = "PER_1000_POP",
    "%" = "PERCENT",
    "years" = "YEAR"
  )


vital_stats <- vital_stats%>%
  mutate(text=paste(Name,"in",time_period,":", obs_value,unit_measure))%>%
  left_join(total_pop)


write_rds(vital_stats,here("data","vital_stats_pacific.rds"))
