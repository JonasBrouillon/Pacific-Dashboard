devtools::install_github("opensdmx/rsdmx")
pacman::p_load(rsdmx,tidyverse,ISOcodes,here)

url <- "https://stats-nsi-stable.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A..MIDYEARPOPEST.F+M.Y70T999+Y65T69+Y60T64+Y55T59+Y50T54+Y45T49+Y40T44+Y35T39+Y30T34+Y25T29+Y20T24+Y15T19+Y10T14+Y05T09+Y00T04?endPeriod=2027&dimensionAtObservation=AllDimensions"
structure_sexe_age <- readSDMX(url)%>%as_tibble()%>%janitor::clean_names()%>%
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2"))

structure_sexe_age <- structure_sexe_age%>%
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC"))%>%
  mutate(age=str_remove_all(age,"Y"),age=str_replace_all(age,"T","-"),
         age=recode(age,"70-999"="70 +"),
        sex=recode(sex,"M"="Male","F"="Female")
                    
         )


write_rds(structure_sexe_age,here("data","structure_sexe_age_pacific.rds"))
RColorBrewer::display.brewer.all()
  