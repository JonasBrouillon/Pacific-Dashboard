
text_vital_indicators <- tibble(Indicator= c("Crude birth rate","Infant mortality rate","Life expectancy at birth","Crude death rate","Total fertility rate","Teenage fertility rate"),
                                Description=c("Crude birth rate indicates the number of live births occurring during the year, per 1,000 population estimated at midyear. Subtracting the crude death rate from the crude birth rate provides the rate of natural increase, which is equal to the rate of population change in the absence of migration.",
                                              "Infant mortality rate is the number of infants dying before reaching one year of age, per 1,000 live births in a given year.",
                                              "Life expectancy at birth indicates the number of years a newborn infant would live if prevailing patterns of mortality at the time of its birth were to stay the same throughout its life.",
                                              "Crude death rate indicates the number of deaths occurring during the year, per 1,000 population estimated at midyear. Subtracting the crude death rate from the crude birth rate provides the rate of natural increase, which is equal to the rate of population change in the absence of migration.",
                                              "Total fertility rate (TFR): The number of children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance with age-specific fertility rates currently observed. The reference period is three years preceding the survey.",
                                              "Teenage fertility rate is the number of births per 1,000 women ages 15-19."),
                                "Description source"=c("https://databank.worldbank.org/metadataglossary/gender-statistics/series/SP.DYN.CBRT.IN",
                                                       "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SP.DYN.IMRT.IN",
                                                       "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SP.DYN.LE00.FE.IN",
                                                       "https://databank.worldbank.org/metadataglossary/all/series?search=Crude%20death%20rate",
                                                       "https://databank.worldbank.org/metadataglossary/environment-social-and-governance-(esg)-data/series/SP.DYN.TFRT.IN",
                                                       "https://databank.worldbank.org/metadataglossary/gender-statistics/series/SP.ADO.TFRT"
                                ),
                                "Data source"=rep("https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_VITAL&df[ag]=SPC&df[vs]=1.0&pd=%2C2020&dq=A...&ly[cl]=TIME_PERIOD&ly[rw]=INDICATOR%2CSEX",6)
)%>%
  mutate(`Data source`=paste0("<a href='", `Data source`, "' target='_blank'>", "Data source", "</a>"),
         `Description source`=paste0("<a href='", `Description source`, "' target='_blank'>", "Description source", "</a>")
         
         )

text_age_indicators <- tibble(Indicator=c("Dependency ratio (15-59)","Dependency ratio (15-64)","Population of youth","Proportion of elderly (65+)","Proportion of elderly (60+)","Proportion of children (<14)","Median age"),
                              Description=c("Age dependency ratio is the ratio of dependents--people younger than 15 or older than 60--to the working-age population--those ages 15-60. Data are shown as the proportion of dependents per 100 working-age population.",
                                            "Age dependency ratio is the ratio of dependents--people younger than 15 or older than 64--to the working-age population--those ages 15-64. Data are shown as the proportion of dependents per 100 working-age population.",
                                            "Proportion of the population (%) aged betwen 15 and 24 years old",
                                            "Proportion of the population (%) aged of 65 years old or more",
                                            "Proportion of the population (%) aged of 60 years old or more",
                                            "Proportion of the population (%) younger than 14 years old",
                                            "The age that divides a population into two numerically equal groups; that is, half the people are younger than this age and half are older. It is a single index that summarizes the age distribution of a population."
                                            
                              ),
                              
                              "Description source"=c("https://databank.worldbank.org/metadataglossary/gender-statistics/series/SP.POP.DPND",
                                                     "https://databank.worldbank.org/metadataglossary/gender-statistics/series/SP.POP.DPND","","","",
                                                     "", "https://www.cia.gov/the-world-factbook/field/median-age/"),
                              
                              "Data source"=c("https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_AGE&df[ag]=SPC&df[vs]=1.0&pd=2022%2C2022&dq=A..%2BDEPRATIO1564._T&ly[rw]=GEO_PICT&ly[cl]=INDICATOR", 
                                              "https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_AGE&df[ag]=SPC&df[vs]=1.0&pd=2022%2C2022&dq=A..%2BDEPRATIO1559._T&ly[rw]=GEO_PICT&ly[cl]=INDICATOR",
                                              rep("https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_AGE&df[ag]=SPC&df[vs]=1.0&pd=2022%2C2022&dq=A..MEDIANAGE%2BPOPCHILD%2BPOPYOUTH%2BPOPELDER60%2BPOPELDER65%2BDEPRATIO1559%2BDEPRATIO1564._T&ly[rw]=GEO_PICT&ly[cl]=INDICATOR",5))
                              
)%>%
  mutate(`Data source`=paste0("<a href='", `Data source`, "' target='_blank'>", "Data source", "</a>"),
         `Description source`=paste0("<a href='", `Description source`, "' target='_blank'>", "Description source", "</a>")
         
  )

text_gdp_indicators <- tibble(Indicator=c("GDP",
                                          "GDP per capita",
                                          "Volume of remittances",
                                          "Trade Statistics : Trade balance",
                                          "Trade Statistics : Imports",
                                          "Trade Statistics : Total exports"),
                              Descripton=c("GDP at purchaser's prices is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources.",
                                           "GDP per capita is gross domestic product divided by midyear population. GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources. Data are in constant 2015 U.S. dollars.",
                                           "Personal remittances comprise personal transfers and compensation of employees. Personal transfers consist of all current transfers in cash or in kind made or received by resident households to or from nonresident households. Personal transfers thus include all current transfers between resident and nonresident individuals.",
                                           "Trade balance is the sum of exports and imports of goods and services measured as a share of gross domestic product.",
                                           "Imports of goods and services represent the value of all goods and other market services received from the rest of the world. They include the value of merchandise, freight, insurance, transport, travel, royalties, license fees, and other services, such as communication, construction, financial, information, business, personal, and government services.",
                                           "Exports of goods and services represent the value of all goods and other market services provided to the rest of the world. They include the value of merchandise, freight, insurance, transport, travel, royalties, license fees, and other services, such as communication, construction, financial, information, business, personal, and government services."
                              ),
                              "Description source"=c("https://databank.worldbank.org/metadataglossary/africa-development-indicators/series/NY.GDP.MKTP.KD",
                                                     "https://databank.worldbank.org/metadataglossary/sustainable-development-goals-%28sdgs%29/series/NY.GDP.PCAP.KD",
                                                     "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/BX.TRF.PWKR.DT.GD.ZS",
                                                     "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/NE.TRD.GNFS.ZS",
                                                     "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/NE.IMP.GNFS.ZS",
                                                     "https://databank.worldbank.org/metadataglossary/jobs/series/NE.EXP.GNFS.ZS"),
                              "Data source"=c("https://stats.pacificdata.org/vis?lc=en&df[ds]=SPC2&df[id]=DF_POCKET&df[ag]=SPC&df[vs]=3.0&dq=..GDPCUSD%2BGDPCPCUSD&pd=%2C2022&ly[cl]=INDICATOR",
                                              "https://stats.pacificdata.org/vis?lc=en&df[ds]=SPC2&df[id]=DF_POCKET&df[ag]=SPC&df[vs]=3.0&dq=..GDPCUSD%2BGDPCPCUSD&pd=%2C2022&ly[cl]=INDICATOR",
                                              "https://stats.pacificdata.org/vis?dq=A..BX_TRF_PWKR.....&pd=%2C&frequency=A&lc=en&pg=0&df[ds]=SPC2&df[id]=DF_NMDI_OTH&df[ag]=SPC&df[vs]=1.0&ly[rw]=GEO_PICT&ly[cl]=TIME_PERIOD",
                                              "https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CEconomy%23ECO%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_IMTS&df[ag]=SPC&df[vs]=4.0&pd=2015%2C&dq=A..GDP.TB%2BX%2BM._T._T._T.USD&ly[rw]=TRADE_FLOW&ly[cl]=TIME_PERIOD",
                                              "https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CEconomy%23ECO%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_IMTS&df[ag]=SPC&df[vs]=4.0&pd=2015%2C&dq=A..GDP.TB%2BX%2BM._T._T._T.USD&ly[rw]=TRADE_FLOW&ly[cl]=TIME_PERIOD",
                                              "https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CEconomy%23ECO%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_IMTS&df[ag]=SPC&df[vs]=4.0&pd=2015%2C&dq=A..GDP.TB%2BX%2BM._T._T._T.USD&ly[rw]=TRADE_FLOW&ly[cl]=TIME_PERIOD")
)%>%
  mutate(`Data source`=paste0("<a href='", `Data source`, "' target='_blank'>", "Data source", "</a>"),
         `Description source`=paste0("<a href='", `Description source`, "' target='_blank'>", "Description source", "</a>")
         
  )


text_geography_indicators <- tibble(Indicator=c("Coastal population","Population density","Urban population"),
                                    Desctiption=c("Proportion of population living in 1, 5 and 10km buffer zones for Pacific Island Countries and Territories, determined using most recent Population and Housing Census. Number of people living in 1,5 and 10km buffer zones determined by apportioning population projections.",
                                                  "Population density is midyear population divided by land area in square kilometers. Population is based on the de facto definition of population, which counts all residents regardless of legal status or citizenship--except for refugees not permanently settled in the country of asylum, who are generally considered part of the population of their country of origin.",
                                                  "Urban population refers to people living in urban areas as defined by national statistical offices. The data are collected and smoothed by United Nations Population Division."),
                                    
                                    "Description source"=c("https://sdd.spc.int/mapping-coastal","https://databank.worldbank.org/metadataglossary/environment-social-and-governance-(esg)-data/series/EN.POP.DNST","https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SP.URB.TOTL.IN.ZS"),
                                    
                                    "Data source"=c("https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_COAST&df[ag]=SPC&df[vs]=2.0&pd=%2C2021&dq=A...&ly[rw]=GEO_PICT&ly[cl]=RANGE%2CINDICATOR",
                                                    "https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_DENSITY&df[ag]=SPC&df[vs]=1.0&pd=%2C2022&dq=A..&ly[rw]=GEO_PICT&ly[cl]=INDICATOR",
                                                    "https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_URBAN&df[ag]=SPC&df[vs]=1.0&pd=2022%2C2022&dq=A...&ly[cl]=INDICATOR%2CURBANIZATION&ly[rw]=GEO_PICT")
)%>%
  mutate(`Data source`=paste0("<a href='", `Data source`, "' target='_blank'>", "Data source", "</a>"),
         `Description source`=paste0("<a href='", `Description source`, "' target='_blank'>", "Description source", "</a>")
         
  )


zee <- read_sf(here("data","Pacific Island Countries and Territories Exclusive Economic Zones.geojson"))


# Define UI for application that draws a histogram
structure_age_pacific <- readRDS(here("data","structure_age_pacific.rds"))
structure_sexe_age_pacific <- readRDS(here("data","structure_sexe_age_pacific.rds"))
vital_stats_pacific <- readRDS(here("data","vital_stats_pacific.rds"))
gdp_pacific <- readRDS(here("data","gdp_pacific.rds"))
import_export_pacific <- readRDS(here("data","import_export_pacific.rds"))
gdp_pacific <- bind_rows(gdp_pacific, import_export_pacific)
total_pop_pacific <- readRDS(here("data","total_pop_pacific.rds"))


vital_stats_pacific <- vital_stats_pacific%>%bind_rows(total_pop_pacific)
geography_pacific <- readRDS(here("data","geography_pacific.rds"))
iso <- gdp_pacific %>%
dplyr::select(Name,geo_pict,Alpha_3)%>%unique()%>%
  dplyr::arrange(desc(Name)) 

about_picts <- readxl::read_xlsx(here("data","about_picts.xlsx"))%>%janitor::clean_names()%>%arrange(desc(name))%>%
  cbind(iso)
