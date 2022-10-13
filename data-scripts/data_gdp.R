pacman::p_load(rsdmx,tidyverse,ISOcodes,here)


url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_NATIONAL_ACCOUNTS,1.0/A.USD..GDPC+GDPCPC?dimensionAtObservation=AllDimensions"

gdp <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))


gdp$indicator<- fct_recode(gdp$indicator,
                           "GDP" = "GDPC",
                           "GDP per capita" = "GDPCPC"
)

gdp$unit_measure<- fct_recode(gdp$unit_measure,
                              "$"="USD",
                              "$ per capita" = "USD_POP"
)

gdp<- gdp%>%
  mutate(text=paste(Name,"in",time_period,":", format(obs_value,big.mark =" "),unit_measure))




write_rds(gdp, here("data","gdp_pacific.rds"))
