pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_POP_AGE,1.0/A..MEDIANAGE+POPCHILD+POPYOUTH+POPELDER60+POPELDER65+DEPRATIO1559+DEPRATIO1564._T?endPeriod=2022&dimensionAtObservation=AllDimensions"
structure_age <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))

structure_age <- structure_age%>%
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))

structure_age$indicator<- structure_age$indicator %>%
  fct_recode(
    "Dependency ratio (15-59)" = "DEPRATIO1559",
    "Dependency ratio (15-64)" = "DEPRATIO1564",
    "Median age" = "MEDIANAGE",
    "Proportion of children (<14)" = "POPCHILD",
    "Proportion of elderly (60+)" = "POPELDER60",
    "Proportion of elderly (65+)" = "POPELDER65",
    "Population of youth" = "POPYOUTH"
  )



structure_age$unit_measure<- structure_age$unit_measure %>%
  fct_recode(
    "%" = "PERCENT",
    "years" = "YEAR"
  )


structure_age<- structure_age%>%
  mutate(text=paste(Name,"in",time_period,":", obs_value,unit_measure))


write_rds(structure_age,here("data","structure_age_pacific.rds"))
