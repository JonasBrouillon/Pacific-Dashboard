


pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_IMTS,4.0/A..AMT.TB+X+M._T._T._T.USD?dimensionAtObservation=AllDimensions"
import_export <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))


import_export$indicator<- import_export$trade_flow %>%
  fct_recode(
    "Trade Statistics : Imports" = "M",
    "Trade Statistics : Trade balance" = "TB",
    "Trade Statistics : Total exports" = "X"
  )

import_export$unit_measure<- import_export$unit_measure %>%
  fct_recode("$"="USD")


url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_SOCECO,1.0/A..IMP2GDP+EXP2GDP+TB2GDP?endPeriod=2019&dimensionAtObservation=AllDimensions"
import_export2 <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))



import_export2$indicator <- import_export2$indicator%>%
  fct_recode(
    "Trade Statistics : Imports" = "IMP2GDP",
    "Trade Statistics : Trade balance" = "TB2GDP",
    "Trade Statistics : Total exports" = "EXP2GDP"
  )

import_export2$unit_measure <- fct_recode(import_export2$unit_measure,
                                          "% of GDP" = "PERCENT"
)


import_export_pacific <- import_export%>%
  select(time_period,geo_pict,Name,obs_value,indicator,unit_measure,Alpha_3)%>%
  rbind(import_export2%>%select(time_period,geo_pict,Name,indicator,obs_value,unit_measure,Alpha_3))%>%
  mutate(text=paste(Name,"in",time_period,":", obs_value,unit_measure))%>%
  filter(Name!="Pitcairn")



url <- 'https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_NMDI_OTH,1.0/A..BX_TRF_PWKR._T._T._T..?endPeriod=2022&dimensionAtObservation=AllDimensions'
remittances <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))%>%filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))%>%
  mutate(indicator='Volume of remittances',unit_measure= "% of GDP",
         text=paste(Name,"in",time_period,":", obs_value,unit_measure))

import_export_pacific <- import_export_pacific%>%bind_rows(remittances)  
 

write_rds(import_export_pacific,here("data","import_export_pacific.rds"))

