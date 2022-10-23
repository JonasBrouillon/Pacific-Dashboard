# Pacific-Dashboard
Repository of the Shiny app <a href="https://jbrouillon.shinyapps.io/Pacific-Dashboard/ " target='_blank'> Pacific Dashboard</a> created for the Pacific Dataviz Challenge 2022. 

You can run the application locally by download the repository and run the app.R script (verify you have installed needed packages).

The "data" folder contain the data used by app.R .

The "data-scripts" folder contain the scripts used initally to download and cleaning data.

The "scripts" folder contain the scripts used by app.R .

The "www" folder contain pictures and CSS customized theme.

# Data sources

All the data used are from the <a href='https://pacificdata.org/ ' target='_blank'>Pacific data hub</a>.

Data were downloaded by API request via the <a href='https://stats.pacificdata.org/ ' target='_blank'>Pacific data hub explorer</a>.



| Indicator(s)          | Data source                                                                                                                                                                                                                                                                                                                                     |
|-----------------------|-------------------------------------------------------------------------------------|
|About PICTS             | <a href='https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CMulti-domain%23XDO%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_KEYFACTS&df[ag]=SPC&df[vs]=1.0&pd=2021%2C2021&dq=A..&ly[cl]=INDICATOR&ly[rw]=GEO_PICT' target='_blank'>Pacific data hub : About PICTS</a> |
| Age statistics        | <a href='https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_AGE&df[ag]=SPC&df[vs]=1.0&pd=2022%2C2022&dq=A..MEDIANAGE%2BPOPCHILD%2BPOPYOUTH%2BPOPELDER60%2BPOPELDER65%2BDEPRATIO1559%2BDEPRATIO1564._T&ly[rw]=GEO_PICT&ly[cl]=INDICATOR' target='_blank'>Pacific data hub : Population structure by age</a> |
| Coastal population    | <a href='https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_COAST&df[ag]=SPC&df[vs]=2.0&pd=%2C2021&dq=A...&ly[rw]=GEO_PICT&ly[cl]=RANGE%2CINDICATOR' target='_blank'>Pacific data hub : Coastal population</a>                                                                                    |
| EEZ boundaries | <a href='https://map.pacificdata.org/#share=s-8gcA4u3ykVQMJ9ptzKTG6QfMu2K' target='_blank'>Pacific Data Hub Map</a> |
| GDP & GDP per capita  | <a href='https://stats.pacificdata.org/vis?lc=en&df[ds]=SPC2&df[id]=DF_POCKET&df[ag]=SPC&df[vs]=3.0&dq=..GDPCUSD%2BGDPCPCUSD&pd=%2C2022&ly[cl]=INDICATOR' target='_blank'>Pacific data hub : Pocket Summary</a>                                                                                                       |
| Population density    | <a href='https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_DENSITY&df[ag]=SPC&df[vs]=1.0&pd=%2C2022&dq=A..&ly[rw]=GEO_PICT&ly[cl]=INDICATOR' target='_blank'>Pacific data hub : Population density</a>                                                                                           |
|Population pyramids | <a href='https://stats.pacificdata.org/vis?fs%5B0%5D=Topic%252C0%7CPopulation%2523POP%2523&pg=0&fc=Topic&df%5Bds%5D=ds%253ASPC2&df%5Bid%5D=DF_POP_PROJ&df%5Bag%5D=SPC&df%5Bvs%5D=3.0&pd=2017%252C2027&dq=A..MIDYEARPOPEST._T._T' target='_blank'>Pacific data hub : Population projections</a> |
| Trade statistics      | <a href='https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CEconomy%23ECO%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_IMTS&df[ag]=SPC&df[vs]=4.0&pd=2015%2C&dq=A..GDP.TB%2BX%2BM._T._T._T.USD&ly[rw]=TRADE_FLOW&ly[cl]=TIME_PERIOD' target='_blank'>Pacific data hub : IMTS</a>                                                                      |
| Urban population      | <a href='https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_POP_URBAN&df[ag]=SPC&df[vs]=1.0&pd=2022%2C2022&dq=A...&ly[cl]=INDICATOR%2CURBANIZATION&ly[rw]=GEO_PICT' target='_blank'>Pacific data hub : Urban population</a>                                                                         |
| Vitals statistics     | <a href='https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&df[ds]=ds%3ASPC2&df[id]=DF_VITAL&df[ag]=SPC&df[vs]=1.0&pd=%2C2020&dq=A...&ly[cl]=TIME_PERIOD&ly[rw]=INDICATOR%2CSEX' target='_blank'>Pacific data hub : Vitals statistics </a>                                                                                       |
| Volume of remittances | <a href='https://stats.pacificdata.org/vis?dq=A..BX_TRF_PWKR.....&pd=%2C&frequency=A&lc=en&pg=0&df[ds]=SPC2&df[id]=DF_NMDI_OTH&df[ag]=SPC&df[vs]=1.0&ly[rw]=GEO_PICT&ly[cl]=TIME_PERIOD' target='_blank'>Pacific data hub : NMDI for others topics</a> |



