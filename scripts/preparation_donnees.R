
about_picts <- about_picts%>%
  mutate(text=paste(name,
                    paste("Membre of SPC since :",pacific_community_membership) ,  
                    paste("Government :",str_replace_all(government,"\\.","</p>" )),
                    paste("Langage(s) :",languages),
                    paste("Currency :",currency),sep ="</p>"
  ))%>%
  inner_join(zee,by=c("Alpha_3"="ISO_Ter1"))%>%st_as_sf()
about_picts_table <- about_picts%>% as_tibble()%>%
  select(name,government:pacific_community_membership)%>%
  rename("Country"=1)

colnames(about_picts_table) <- str_to_sentence(str_replace_all(colnames(about_picts_table),"_"," "))
structure_age_bis <- structure_age_pacific%>%
  filter(indicator%in% c("Median age","Dependency ratio (15-59)"))%>%mutate(indicator=snakecase::to_snake_case(as.character(indicator)))%>%
  group_by(Name)%>%
  filter(time_period==max(time_period))%>%
  ungroup()%>%
  select(Name,indicator,obs_value)%>%
  pivot_wider(names_from = indicator,values_from = obs_value)


vitals_stats_bis <- vital_stats_pacific%>%
  filter(indicator %in% c("Life expectancy at birth","Total fertility rate","Infant mortality rate") & sex %in% c("Total","Female"))%>%
  mutate(indicator=snakecase::to_snake_case(as.character(indicator)))%>%  group_by(Name,indicator)%>%
  filter(time_period==max(time_period))%>%
  ungroup()%>%select(Name,indicator,obs_value)%>%
  pivot_wider(names_from = indicator,values_from = obs_value)


gdp_bis <- gdp_pacific%>%
  filter(unit_measure %in% c("$ per capita","% of GDP") & !indicator %in% c("Trade Statistics : Total exports","Trade Statistics : Imports"))%>%
  mutate(indicator=snakecase::to_snake_case(as.character(indicator)),
         indicator=str_remove_all(indicator,"trade_statistics_")
  )%>%  group_by(Name,indicator)%>%
  filter(time_period==max(time_period,na.rm = T))%>%
  ungroup()%>%select(Name,indicator,obs_value)%>%
  pivot_wider(names_from = indicator,values_from = obs_value)

geography_bis <- geography_pacific%>%filter(indicator %in% c("Population density","Urban population")|range=="1km from coast")%>%
  mutate(indicator=snakecase::to_snake_case(as.character(indicator)),                                                                                                                                    indicator=str_remove_all(indicator,"trade_statistics_")
  )%>%  group_by(Name,indicator)%>%
  filter(time_period==max(time_period,na.rm = T))%>%
  ungroup()%>%select(Name, indicator,obs_value)%>%
  pivot_wider(names_from = indicator,values_from = obs_value)





indicators_matrice <- structure_age_bis%>%
  left_join(vitals_stats_bis)%>%
  left_join(gdp_bis)%>%
  left_join(geography_bis)

dend <- hclust(dist(percentize(indicators_matrice%>%column_to_rownames("Name")), method = "euclidean"), method = "complete")
groups <- cutree(dend, k = 3)

indicators_matrice <- indicators_matrice%>%
  mutate(cluster=paste("Group", groups))
indicators_matrice_b <- indicators_matrice
colnames(indicators_matrice_b) <- str_to_sentence(str_replace_all(colnames(indicators_matrice_b),"\\_"," "))    


c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)



structure_age_bis2 <- structure_age_pacific%>%
  filter(indicator%in% c("Median age","Dependency ratio (15-59)"))%>%
  group_by(Name)%>%
  filter(time_period==max(time_period))%>%
  ungroup()%>%
  mutate(text=paste(indicator,"in",time_period,":", obs_value,unit_measure))%>%
  select(Name,text)


vitals_stats_bis2 <- vital_stats_pacific%>%
  filter(indicator %in% c("Life expectancy at birth","Total fertility rate","Infant mortality rate") & sex %in% c("Total","Female"))%>%
  group_by(Name,indicator)%>%
  filter(time_period==max(time_period))%>%
  mutate(text=paste(indicator,"in",time_period,":", obs_value,unit_measure))%>%
  ungroup()%>%select(Name,text)



gdp_bis2 <- gdp_pacific%>%
  filter(unit_measure %in% c("$ per capita","% of GDP") & !indicator %in% c("Trade Statistics : Total exports","Trade Statistics : Imports"))%>%
  mutate(indicator=fct_recode(indicator,"Trade balance"= "Trade Statistics : Trade balance")
  )%>%  group_by(Name,indicator)%>%
  filter(time_period==max(time_period,na.rm = T))%>%
  ungroup()%>%
  mutate(text=paste(indicator,"in",time_period,":", obs_value,unit_measure))%>%select(Name,text)


geography_bis2 <- geography_pacific%>%filter(indicator %in% c("Population density","Urban population")|range=="1km from coast")%>%
  group_by(Name,indicator)%>%
  filter(time_period==max(time_period,na.rm = T))%>%
  ungroup()%>%
  mutate(text=paste(indicator,"in",time_period,":", obs_value,unit_measure))%>%
  select(Name,text)






text_indicators_matrice <- structure_age_bis2%>%
  rbind(vitals_stats_bis2)%>%
  rbind(gdp_bis2)%>%
  rbind(geography_bis2)%>%arrange(Name,text)%>%
  group_by(Name)%>%
  summarise(text=paste(text,collapse = "</p>"))
