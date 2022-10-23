#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(readxl)
library(here)
library(conflicted)
library(leaflet)
library(plotly)
library(ggthemes)
library(htmltools)
library(showtext)
library(readxl)
library(heatmaply)
library(RColorBrewer)
library(gt)
library(gtsummary)
library(snakecase)
library(janitor)
library(tidyverse)
library(sf)
library(lubridate)
library(extrafont)
library(gitlink)
library(extrafont)
library("emojifont")


font_add_google(name = "Montserrat")
showtext_auto()
conflict_prefer("filter","dplyr")
conflict_prefer_all("shinydashboardPlus")



source(here("scripts", "import_donnees.R"),encoding = "utf8")
source(here("scripts", "preparation_donnees.R"),encoding = "utf8")

  ui<- dashboardPage( skin = "midnight",
  dashboardHeader(title = p("Pacific countries indicators dashboard",style="font-family: 'Montserrat';color: #ffc433;font-size:20px;"), titleWidth = 450),
  dashboardSidebar(width = 350,sidebarMenu(
    menuItem("Presentation",tabName = "presentation",icon = icon("question")),
    menuItem("Structure by sex and age",tabName = "pyramid",icon = icon("person-cane")),
    menuItem("Vital stats",tabName = "vitals",icon = icon("heart")),
    menuItem("Economy",tabName = "gdp",icon = icon("dollar")),
    menuItem("Geography",tabName = "geography",icon = icon("map")),
    menuItem("Correlations and variable crossing",tabName = "correlation",icon = icon("magnifying-glass"))
    
    
    )
  
    ),
    dashboardBody(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
      tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      font-size:18px;
      }
    "))
    ), 
      tabItems(
        tabItem(tabName = "presentation",fluidPage(mainPanel(ribbon_css("https://github.com/JonasBrouillon/Pacific-Dashboard.git", text = "Repository link", fade = T,position = "left"),
          tags$figure(
            align = "left",
            tags$img(
              src = "pacific_dataviz.png",
              width = 450
            ),
            tags$img(
              src = "gouv_nc_logo.png",
              width = 75
            ),
            tags$img(
              src = "spc_logo.png",
              width = 150
            ),

            tags$img(
              src = "sprep_logo.png",
              width = 150
            )
            
            ),

          
            h1("Presentation of the Pacific countries indicators dashboard"),
            
            p("Welcome in the Pacific countries indicators dashboard.",style="font-size:20px;"),
            p("This dashboard was created as part of the Pacific Dataviz Challenge 2022, the goal is to have a simple and friendly way to visualize the indicators provided by the Pacific Community (SPC)."),
            p("The dashboard is composed of 5 tab decicated to differents thematics :"),
            tags$div(tags$ul(
              tags$li(tags$span(icon("person-cane"),"Structur by sex and age : provide pyramid population comparisons and indicators about sex & age structur.")),
              tags$li(tags$span(icon("heart"),"Vitals stats : provide indicators about mortality and fecondity.")),
              tags$li(icon("dollar"),tags$span("Economy : provide indicators about Gross Domestic Product (GDP) and trade.")),
              tags$li(icon("map"),tags$span("Geography : provide indicators about population density, urbanization and coastal population.")),
              tags$li(icon("magnifying-glass"),tags$span("Correlations and variable crossing : provide crossed analysis of the differents variables using Hierarchical clustering."))
              )),
            p("The four first tab allow user to choose the indicator and the countries to display."),
            p("User can plot the evolution of the indicator or display the last available values for it."),
            p("Each tab also includes a map to visualize the indicator in space, the geographical units of the maps being the exclusive economic zone (EEZ) of each country."),width = 12),
           
            
           fluidRow(column(h2("About the SPC members",tags$img(src = "spc_logo.png",width = 100)),  
                 tabsetPanel( tabPanel(title = "Table", tableOutput("table_about")),
                              tabPanel(title = "Map", leafletOutput("map_about",height = "50em"))),width = 12))
            
            )
          
          ),
      
      tabItem(
        tabName = "pyramid",  
   fluidPage( tags$style("#indicator_pyr1 {font-size:22px;}"),tags$style("#indicator_pyr2 {font-size:22px;}"),tags$style("#indicator_age {font-size:22px;}"),h1("Structure by sex and age : population projections and indicators"),   fluidRow(
          box(closable = FALSE,collapsible = F,  title = p("Population pyramid 1",textOutput("indicator_pyr1"), style='font-size:22px;'), 
            actionButton(inputId = "pyr_button1","Choose countries and years", style='height:60px; font-size:18px;'),
            sidebar =  boxSidebar(id = "pyr_update1",p(style="font-family: 'Montserrat';","Countries and years"),  selectInput("pyr_country1", "First country", choices = levels(as.factor(structure_sexe_age_pacific$Name))),
                         selectInput("pyr_year1", "First year", choices = levels(as.factor(structure_sexe_age_pacific$time_period)))
                         ),  
            htmlOutput("pyr_text1"), 
          tabsetPanel( 
           tabPanel("Plot",  plotlyOutput( "pyramide_age1",height = "40em")),
           tabPanel("Table"  ,tableOutput("pyr_table1"))
           ),height = "40em"),
          box(closable = FALSE,collapsible = F,  title = p("Population pyramid 2",textOutput("indicator_pyr2"), style='font-size:22px;'), 
            actionButton(inputId = "pyr_button2","Choose countries and years", style='height:60px; font-size:18px;'),
            sidebar =  boxSidebar(id = "pyr_update2",p(style="font-family: 'Montserrat';","Countries and years"),  selectInput("pyr_country2", "Second country", choices = levels(as.factor(structure_sexe_age_pacific$Name)),selected = "Cook Islands"),
                                  selectInput("pyr_year2", "Second year", choices = levels(as.factor(structure_sexe_age_pacific$time_period)))
            ),
            htmlOutput("pyr_text2"), 
            tabsetPanel( 
              tabPanel("Plot",  plotlyOutput( "pyramide_age2",height = "40em")),
              tabPanel("Table"  ,tableOutput("pyr_table2"))
            ),height = "40em" )),
        
         fluidRow(box(closable = FALSE,collapsible = TRUE,  title = p(style="font-family: 'Montserrat';color: #ffc433",textOutput("indicator_age")), 
                      actionButton(inputId = "age_button1","Choose indicator and countries", style='height:60px; font-size:18px;'),
                      sidebar =  boxSidebar(id = "age_update1",
                                            selectInput("age_indicator", "Indicator", choices = levels(as.factor(structure_age_pacific$indicator))),
                                            pickerInput("age_country","Countries to show",choices = levels(as.factor(structure_age_pacific$Name)),selected = levels(as.factor(structure_age_pacific$Name)),options = list(`actions-box` = TRUE),multiple = T),
                                            ),
                      
          tabsetPanel(  
            tabPanel( "Evolution of the indicator", plotlyOutput( "age_plot",height = "48em")),
            tabPanel("Last available values" , plotlyOutput( "age_plot2",height = "48em") )),height = "48em"),
      
         
         box(title = p(style="font-family: 'Montserrat';font-size:22px;","Map of indicator"),  leafletOutput("age_map",height = "80em"),height = "80em")
         ),
         fluidRow(column( tableOutput("text_age"),
             
             infoBoxOutput("info_box_age_max"),
             infoBoxOutput("info_box_age_min"),
             p("Sources :"),
             uiOutput("text_age2"),
             uiOutput("text_age3"),width = 12)
         ),
        
        
        )),
        tabItem(
          tabName = "vitals",fluidPage(tags$style("#indicator_vital {font-size:22px;}"),h1("Vitals stats : mortality and fecondity indicators"),  
           fluidRow(box(title = p(style="font-family: 'Montserrat';",textOutput("indicator_vital")), 
                       actionButton("vital_button1","Choose indicator and countries", style='height:60px; font-size:18px;'),

                        sidebar = boxSidebar(id="vital_update1",
                                             selectInput("vital_indicator", "Indicator", choices = levels(as.factor(vital_stats_pacific$indicator)),selected = "Life expectancy at birth"),
                                             conditionalPanel(condition = "input.vital_indicator == 'Life expectancy at birth' " , 
                                                              selectInput("vital_sex", "Sex", choices = c("Male","Female"),selected='Male')),
                                             pickerInput("vital_country","Countries to show", choices = levels(as.factor(vital_stats_pacific$Name)),selected = levels(as.factor(vital_stats_pacific$Name)),options = list(`actions-box` = TRUE),multiple = T ) ),
                       tabsetPanel( 
                         tabPanel("Evolution of the indicator", plotlyOutput( "vital_plot",height = "48em")),
                         tabPanel("Last available values" , plotlyOutput( "vital_plot2",height = "48em") )),height = "48em"),
          box(title = p(style="font-family: 'Montserrat';font-size:22px;","Map of indicator"),  leafletOutput("vital_map",height = "80em"),height = "80em"))
          
          ,
          
          fluidRow(column(tableOutput("text_vital"),
              infoBoxOutput("info_box_vital_max"),
              infoBoxOutput("info_box_vital_min"),
              p("Sources :"),
              uiOutput("text_vital2"),
              uiOutput("text_vital3"),width = 12
              ))
          
          
          
          )),
   tabItem(
     tabName = "gdp",fluidPage(tags$style("#indicator_gdp {font-size:22px;}"),h1("Economy : GDP and trade statistics"),  
     fluidRow(box(title = p(style="font-family: 'Montserrat';",textOutput("indicator_gdp")), 
                  actionButton("gdp_button1","Choose indicator and countries", style='height:60px; font-size:18px;'),
                  
                  sidebar = boxSidebar(id="gdp_update1",
                                       selectInput("gdp_indicator", "Indicator", choices = levels(as.factor(gdp_pacific$indicator)),selected ="GDP per capita" , multiple = FALSE),
                                       conditionalPanel(condition = "input.gdp_indicator == 'Trade Statistics : Total exports' ||
                                                                     input.gdp_indicator == 'Trade Statistics : Imports' ||
                                                                     input.gdp_indicator == 'Trade Statistics : Trade balance'" , 
                                                        selectInput("gdp_unit", "Unit measure", choices = c("$","% of GDP"),selected='$'),multiple=FALSE),
                                       pickerInput("gdp_country","Countries to show", choices = levels(as.factor(gdp_pacific$Name)),selected = levels(as.factor(gdp_pacific$Name)),options = list(`actions-box` = TRUE),multiple = T) ),
                  tabsetPanel( 
                    tabPanel("Evolution of the indicator", plotlyOutput( "gdp_plot",height = "48em")),
                    tabPanel("Last available values" , plotlyOutput( "gdp_plot2",height = "48em") )),height = "48em"),
              box(title = p(style="font-family: 'Montserrat';font-size:22px;","Map of indicator"),  leafletOutput("gdp_map",height = "80em"),height = "80em")),
     
     fluidRow(column(tableOutput("text_gdp"),
         infoBoxOutput("info_box_gdp_max"),
         infoBoxOutput("info_box_gdp_min"),
         p("Sources :"),
         uiOutput("text_gdp2"),
         uiOutput("text_gdp3"),width = 12)
     ))),
   
   tabItem(
     tabName = "geography",fluidPage(tags$style("#indicator_geography {font-size:22px;}"),h1("Geography : use of land indicators"),  
     fluidRow(box(title = p(style="font-family: 'Montserrat';",textOutput("indicator_geography")), 
                  actionButton("geography_button1","Choose indicator and countries", style='height:60px; font-size:18px;'),
                  
                  sidebar = boxSidebar(id="geography_update1",
                                       selectInput("geography_indicator", "Indicator", choices = levels(as.factor(geography_pacific$indicator)),multiple = FALSE,selected = "Population density"),
                                       conditionalPanel(condition = "input.geography_indicator == 'Coastal population'" , 
                                                        selectInput("geography_range", "Range", choices = levels(as.factor(geography_pacific$range))),multiple=FALSE),
                                       pickerInput("geography_country","Countries to show", choices = levels(as.factor(geography_pacific$Name)),selected = levels(as.factor(geography_pacific$Name)),options = list(`actions-box` = TRUE),multiple = T ) ),
                  tabsetPanel( 
                    tabPanel("Evolution of the indicator", plotlyOutput( "geography_plot",height = "48em")),
                    tabPanel("Last available values" , plotlyOutput( "geography_plot2",height = "48em") )),height = "48em"),
              box(title = p(style="font-family: 'Montserrat';font-size:22px;","Map of indicator"),  leafletOutput("geography_map",height = "80em"),height = "80em")),
     
     fluidRow(column(tableOutput("text_geography"),
         infoBoxOutput("info_box_geography_max"),
         infoBoxOutput("info_box_geography_min"),
         p("Sources :"),
         uiOutput("text_geography2"),
         uiOutput("text_geography3"),width = 12)
     ))), 
   tabItem(tabName = "correlation",
           fluidPage( h1("Correlations and variables crossing"),
           fluidRow(box(title = p("Hierarchical clustering analysis", style='font-size:22px;'),
             p("This page is an attempt at a global analysis by crossing the different variables presented in this dashboard."),
                 p("12 variables were selected and a statistical analysis named 'Hierarchical clustering' was performed on the data. Data were previously normalized by substracting the minimum and dividing by the maximum of all observations."),
                 p("This analysis permitted to classify Pacific countries into 3 groups :"),
             tags$div(tags$ul(
               tags$li(tags$span( "Group 1 : American Samoa , Cook Islands , Fiji , French Polynesia , Guam , New Caledonia , Niue , Northern Mariana Islands , Palau , Wallis and Futuna.",style="color:#E41A1C;"  ),
                       tags$div(tags$ul(  
                         tags$li(tags$span(emoji("chart_with_upwards_trend"),"High median age, high life expectancy at birth.")),
                        tags$li(tags$span(emoji("chart_with_downwards_trend"),"Low infant mortality rate, low total fertility rate.")),
                        tags$li(tags$span(emoji("chart_with_upwards_trend"),"High GDP per capita and high trade balance deficit")))),
                       tags$li(tags$span("Group 2 : Kiribati , Marshall Islands , Micronesia, Federated States of , Nauru , Samoa , Tokelau , Tonga , Tuvalu.",style="color: #377EB8;"),
                               tags$div(tags$ul(  
                                 tags$li(tags$span(emoji("chart_with_upwards_trend"),"High population density and high percentage of population living above 1km from coast.")),
                                 tags$li(tags$span(emoji("bar_chart"),"Medium values for others indicators.")))))),
               tags$span("Group 3 : Papua New Guinea , Solomon Islands , Vanuatu.",style="color:#4DAF4A;"),
               tags$div(tags$ul(  
                 tags$li(tags$span(emoji("chart_with_downwards_trend"),"Low median age, low life expectancy at birth.")),
                 tags$li(tags$span(emoji("chart_with_upwards_trend"),"High infant mortality rate, high total fertility rate.")),
                 tags$li(tags$span(emoji("chart_with_downwards_trend"),"Low GDP per capita and low trade balance deficit."))))
                 )),
             p("Summarised statistics for each group on the table below :"),
             gt_output("table_cluster")) ,

             
             box(title = p("Heatmap of clusters", style='font-size:22px;'),
                 p("This side allows you to visualize indicators and clusters groups with heatmaps."),
                 p("Visualizing the data matrix in this way can help to find the variables that appear to be characteristic for each sample cluster."),
                 p("Countries are ordering by their cluster group (colors in the right), data are normalized so values are from 0 to 1. More the cell is brown and more the value is high, more the cell is green and more the value is low."),
              plotlyOutput( "heatmap1",height = "56em"))),
            fluidRow(box(title = p("Map of the clusters", style='font-size:22px;'),  leafletOutput("map_cluster",height = "68em")),
             box(title = p("Heatmap of the correlation matrix", style='font-size:22px;'), p("The second heatmap allow you to visualize the correlation matrix betwen variables."),
               p("A correlation matrix is a table showing correlation coefficients between variables. Each cell in the table shows the correlation between two variables."),
               p("More the value is high, more there is a positive correlation betwen the 2 variables. More the value is negative, more there is a negative correlation betwen the 2 variables."),
               plotlyOutput( "heatmap2",height = "40em"),
               height = "48em")))
          
           
           )

   )))




    
    server <- function(input, output, session) {
      
###about

      
      output$map_about <-   renderLeaflet({ 
        pal_about <- colorNumeric(
          palette = "BrBG",
          domain =  about_picts$pacific_community_membership,reverse = F
        )
        
        leaflet(data =about_picts) %>%  addTiles()%>%
          addPolylines(weight = 1,color = "black")%>%
          addPolygons(data = about_picts,stroke = FALSE, 
                      fillOpacity = 0.5, smoothFactor = 0.5, color =~pal_about(pacific_community_membership),label =~lapply(text,htmltools::HTML)  , labelOptions = labelOptions( 
                        style = list("font-weight" = "normal", padding = "3px 8px"), 
                        textsize = "15px", 
                        direction = "auto"))%>%
          addLegend("topright", pal = pal_about, values = ~pacific_community_membership,title ="Member of SPC since",labFormat = labelFormat(big.mark="")) })

      indicateurs_age <- reactive({  
        validate(
          need(input$age_country != "", "Please select at least one country")
        )
        structure_age_pacific%>%
          filter(indicator==input$age_indicator & Name %in% c(input$age_country))})
      
      
      
output$table_about <- renderTable({
  about_picts_table%>%
    mutate(`Pacific community membership`=as.character(`Pacific community membership`))%>%
    unique()%>%
    arrange(Country)

  
  
})      
      
      
##Pyramide 1      
structure_age1 <- reactive({structure_sexe_age_pacific%>%
    filter(Name==input$pyr_country1 & time_period==input$pyr_year1)%>%
  group_by(sex,age)%>%
    summarise(n=sum(obs_value))%>%
    ungroup()%>%
    mutate(percent=n/sum(n),
           percent=ifelse(sex=="Male",percent*-1,percent),
           text=ifelse(percent>0, paste(scales::percent(percent,accuracy = 0.01),"of the population"),
                        paste(scales::percent(percent*-1,accuracy = 0.01),"of the population"))
                       
                       )})

pyr_table1 <-reactive({structure_age1()%>%select(age,percent,sex)%>%mutate(percent=ifelse(percent<0,percent*-1,percent))%>%
  pivot_wider(names_from = sex,values_from = percent)%>%
    select(age,Male,Female)%>%
janitor::adorn_totals("col",name = "Total (%)")%>%janitor::adorn_totals(name = "Total","row")%>%
    rename("Male (%)"=2,"Female (%)"=3)%>%
    mutate_if(is.numeric,function(x){scales::percent(x,accuracy = 0.1)})%>%
    left_join(  structure_age1()%>%select(age,n,sex)%>%
                  pivot_wider(names_from = sex,values_from = n)%>%
                  select(age,Male,Female)%>%janitor::adorn_totals("col",name = "Total (n)")%>%janitor::adorn_totals(name = "Total","row")%>%
                  rename("Male (n)"=2,"Female (n)"=3)%>%
                  mutate_if(is.numeric,function(x){as.integer(x)}))
    
    })



output$pyr_table1 <-  renderTable(pyr_table1() )
output$pyramide_age1<-renderPlotly({ggplotly(
  ggplot(structure_age1(),aes(x=age,y=percent,fill=sex,text=text))+
    geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Set1")+
    coord_flip()+
    scale_y_continuous(labels = function(x){ifelse(x<0,scales::percent(x*-1,accuracy=1),
                                                   scales::percent(x,accuracy=1))},breaks = c(-0.06,-0.05,-0.04,-0.03,-0.02,-0.01,0,
                                                                                                0.01,0.02,0.03,0.04,0.05,0.06))+
    theme_bw(base_family = "Montserrat")+labs(x="",y="",fill=""),tooltip = "text") })



output$pyr_text1<- renderUI({
  
  if(as.numeric(input$pyr_year1)>year(today()) ){
   HTML(paste("The total population of",input$pyr_country1,"in",input$pyr_year1, "will be of",
        "<b>",  format(round(sum(structure_age1()$n)),big.mark=" "),"<b>",  "habs" )) }else{
            
          HTML(paste("The total population of",input$pyr_country1,"in",input$pyr_year1, "is of",
                  "<b>",   format(round(sum(structure_age1()$n)),big.mark=" "), "<b>","habs" ) )  }} )


observe(print(input$pyr_update1))
###Bouton
observeEvent(input$pyr_button1, {
  updateBoxSidebar("pyr_update1")
})




###Pyramide 2
structure_age2 <- reactive({structure_sexe_age_pacific%>%
    filter(Name==input$pyr_country2 & time_period==input$pyr_year2)%>%
    group_by(sex,age)%>%
    summarise(n=sum(obs_value))%>%
    ungroup()%>%
    mutate(percent=n/sum(n),
           percent=ifelse(sex=="Male",percent*-1,percent),
           text=ifelse(percent>0, paste(scales::percent(percent,accuracy = 0.01),"of the population"),
                        paste(scales::percent(percent*-1,accuracy = 0.01),"of the population")))})

output$pyr_text2<- renderUI({
  
  if(as.numeric(input$pyr_year2)>year(today()) ){
    HTML(paste("The total population of",input$pyr_country2,"in",input$pyr_year2, "will be of",
               "<b>",  format(round(sum(structure_age2()$n)),big.mark=" "),"<b>",  "habs" )) }else{
                 
                 HTML(paste("The total population of",input$pyr_country2,"in",input$pyr_year2, "is of",
                            "<b>",   format(round(sum(structure_age2()$n)),big.mark=" "), "<b>","habs" ) )  }} )
      

    pyr_table2 <-reactive({structure_age2()%>%select(age,percent,sex)%>%mutate(percent=ifelse(percent<0,percent*-1,percent))%>%
        pivot_wider(names_from = sex,values_from = percent)%>%
        select(age,Male,Female)%>% 
        janitor::adorn_totals("col",name = "Total (%)")%>%janitor::adorn_totals(name = "Total","row")%>%
        rename("Male (%)"=2,"Female (%)"=3)%>%
        mutate_if(is.numeric,function(x){scales::percent(x,accuracy = 0.1)})%>%
        left_join(  structure_age2()%>%select(age,n,sex)%>%
                      pivot_wider(names_from = sex,values_from = n)%>%
                      select(age,Male,Female)%>%janitor::adorn_totals("col",name = "Total (n)")%>%janitor::adorn_totals(name = "Total","row")%>%
                      rename("Male (n)"=2,"Female (n)"=3)%>%
                      mutate_if(is.numeric,function(x){as.integer(x)}))
        
      })
    
output$indicator_pyr1 <- renderText({paste(input$pyr_country1,"in",input$pyr_year1)})
output$indicator_pyr2 <- renderText({paste(input$pyr_country2,"in",input$pyr_year2)})

            
output$pyr_table2 <- renderTable(pyr_table2())    
    
output$pyramide_age2<-renderPlotly({ggplotly(
        ggplot(structure_age2(),aes(x=age,y=percent,fill=sex,text=text))+
          geom_bar(stat = "identity")+
          scale_fill_brewer(palette = "Set1")+
          coord_flip()+
          scale_y_continuous(labels = function(x){ifelse(x<0,scales::percent(x*-1,accuracy=1),
                                                         scales::percent(x,accuracy=1))},breaks = c(-0.06,-0.05,-0.04,-0.03,-0.02,-0.01,0,
                                                                                                      0.01,0.02,0.03,0.04,0.05,0.06))+
          theme_bw(base_family = "Montserrat")+labs(x="",y="",fill=""),tooltip = "text")})




observe(print(input$pyr_update2))
###Bouton
observeEvent(input$pyr_button2, {
  updateBoxSidebar("pyr_update2")
})





#####Indicateurs age


output$age_plot <- renderPlotly({ggplotly(
  ggplot(indicateurs_age(),aes(x=as.numeric(time_period),y=obs_value,color=Name,group=Name,text=text))+
    geom_point()+
    geom_line()+
    theme_bw(base_family = "Montserrat")+
    scale_color_manual(values = c25)+
    labs(x="Year",y=paste(as.character(indicateurs_age()$indicator[1]),paste0("(", as.character(indicateurs_age()$unit_measure[1]),")")  ) ) ,tooltip = "text")
    })


output$age_plot2 <- renderPlotly({ggplotly(
  ggplot(indicateurs_age()%>%
           group_by(Name)%>%
           filter(time_period==max(time_period))
         ,aes(x=obs_value,y=fct_reorder(Name,obs_value),text=text))+
    geom_segment( aes(yend=Name, xend=0)) +
    geom_point( size=4, aes(color=Name))+
    theme_bw(base_family = "Montserrat")+
    scale_color_manual(values = c25)+
    labs(y="",x=paste(as.character(indicateurs_age()$indicator[1]),paste0("(", as.character(indicateurs_age()$unit_measure[1]),")")  ))+
    theme(legend.position='none'),tooltip = "text") })

observe(print(input$age_update1))

###Bouton
observeEvent(input$age_button1, {
  updateBoxSidebar("age_update1")
})


###box info

text_age_indicators_filter <- reactive({
  text_age_indicators%>%
    filter(Indicator== input$age_indicator)
  
})

output$text_age <- renderTable(text_age_indicators_filter()%>%select(-c(3,4)))
output$indicator_age <- renderText({ HTML(input$age_indicator)})
output$text_age2 <- renderUI({ HTML(text_age_indicators_filter()%>%select(3)%>%as_vector())})
output$text_age3 <- renderUI({ HTML(text_age_indicators_filter()%>%select(4)%>%as_vector())})


age_max <- reactive({
  indicateurs_age()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_max(obs_value)%>%
    slice(1)
  
})

output$info_box_age_max <- renderInfoBox({infoBox(title = "Highest value",paste(age_max()$obs_value,
                                                                                age_max()$unit_measure),subtitle =paste(age_max()$Name,"in",
                  age_max()$time_period),icon = tags$i(class = "fas fa-thumbs-up", style="font-size: 48px; color: white")

)})


age_min <- reactive({
  indicateurs_age()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_min(obs_value)%>%
    slice(1)
  
})

output$info_box_age_min <- renderInfoBox({infoBox(title = "Lowest value",paste(age_min()$obs_value,
                                                                               age_min()$unit_measure
),subtitle =paste(age_min()$Name,"in",
                  age_min()$time_period),color = "red" ,icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 48px; color: white")

)})




###carte

indicateurs_age_geo <-reactive({indicateurs_age()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    inner_join(zee,by=c("Alpha_3"="ISO_Ter1"))%>%st_as_sf()
})



output$age_map <- renderLeaflet({
  

  pal <- colorNumeric(
    palette = "RdYlBu",
    domain = indicateurs_age_geo()$obs_value,reverse = T
  )
  
  leaflet(data =indicateurs_age_geo()) %>%  addTiles()%>%
    addPolylines(weight = 1,color = "black")%>%
    addPolygons(data = indicateurs_age_geo(),stroke = FALSE, 
                fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(obs_value),label =~text   , labelOptions = labelOptions( 
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  textsize = "15px", 
                  direction = "auto"))%>%
    addLegend("topright", pal = pal, values = ~obs_value,title = paste(as.character(indicateurs_age()$indicator[1]),paste0("(", as.character(indicateurs_age()$unit_measure[1]),")")  ) )
})



observe({
  
    pal <- colorNumeric(
    palette = "RdYlBu",
    domain = indicateurs_age_geo()$obs_value,reverse = T
  )

  leafletProxy("age_map") %>%
    clearMarkers()%>% 
    clearPopups()%>%
    addPolygons(data = indicateurs_age_geo(),stroke = FALSE, 
                fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(obs_value),label =~text   , labelOptions = labelOptions( 
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  textsize = "15px", 
                  direction = "auto"))

  
  })



##Indicateurs vitaux
vital_stats_filter <- reactive({
  
  validate(
    need(input$vital_country != "", "Please select at least one country")
  )  
if(input$vital_indicator=="Life expectancy at birth"){
vital_stats_pacific%>%
    filter(indicator==input$vital_indicator & sex==input$vital_sex & Name %in% c(input$vital_country))}else{
      vital_stats_pacific%>%
        filter(indicator==input$vital_indicator & Name %in% c(input$vital_country))}})



observe(print(input$vital_update1))

###Bouton
observeEvent(input$vital_button1, {
  updateBoxSidebar("vital_update1")
})




vital_stats_geo <-reactive({vital_stats_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    inner_join(zee,by=c("Alpha_3"="ISO_Ter1"))%>%st_as_sf()
})

output$vital_plot <- renderPlotly({ggplotly(
  ggplot(vital_stats_filter(),aes(x=as.numeric(time_period),y=obs_value,color=Name,group=Name,text=text))+
    geom_point()+
    geom_line()+
    theme_bw(base_family = "Montserrat")+
    scale_color_manual(values = c25)+
    labs(x="Year",y=paste(as.character(vital_stats_filter()$indicator[1]),paste0("(", as.character(vital_stats_filter()$unit_measure[1]),")")  ) ) ,tooltip = "text")})

output$vital_plot2 <- renderPlotly({ggplotly(
  ggplot(vital_stats_filter()%>%
           group_by(Name)%>%
           filter(time_period==max(time_period))
           ,aes(x=obs_value,y=fct_reorder(Name,obs_value),text=text))+
    geom_segment( aes(yend=Name, xend=0)) +
    geom_point( size=4, aes(color=Name))+
    theme_bw(base_family = "Montserrat")+
    scale_color_manual(values = c25)+
    theme(legend.position='none')+
    labs(y="", x=paste(as.character(vital_stats_filter()$indicator[1]),paste0("(", as.character(vital_stats_filter()$unit_measure[1]),")")  ) ),tooltip = "text") })



text_vital_indicators_filter <- reactive({
  text_vital_indicators%>%
    filter(Indicator== input$vital_indicator)
  
})

output$text_vital <- renderTable(text_vital_indicators_filter()%>%select(-c(3,4)))
output$indicator_vital <- renderText({text_vital_indicators_filter()%>%select(1)%>%as_vector()})
output$text_vital2 <- renderUI({HTML(text_vital_indicators_filter()%>%select(3)%>%as_vector())})
output$text_vital3 <- renderUI({HTML(text_vital_indicators_filter()%>%select(4)%>%as_vector())})

vital_max <- reactive({
  vital_stats_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_max(obs_value)%>%
    slice(1)
  
})

output$info_box_vital_max <- renderInfoBox({infoBox(title = "Highest value",paste(vital_max()$obs_value,
                                                                                   vital_max()$unit_measure
                                                                                   ),subtitle =paste(vital_max()$Name,"in",
                                                                                                     vital_max()$time_period
                                                                                                     ) ,icon = tags$i(class = "fas fa-thumbs-up", style="font-size: 48px; color: white")
                                                      
                                                    )})


vital_min <- reactive({
  vital_stats_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_min(obs_value)%>%
    slice(1)
  
})

output$info_box_vital_min <- renderInfoBox({infoBox(title = "Lowest value",paste(vital_min()$obs_value,
                                                                                  vital_min()$unit_measure
),subtitle =paste(vital_min()$Name,"in",
                  vital_min()$time_period),color = "red" ,icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 48px; color: white")

)})



output$vital_map <- renderLeaflet({
  
  pal2 <- colorNumeric(
    palette = "Spectral",
    domain = vital_stats_geo()$obs_value,reverse = T
  )
  leaflet(data =vital_stats_geo()) %>%  addTiles()%>%
    addPolylines(weight = 1,color = "black")%>%
    addPolygons(data = vital_stats_geo(),stroke = FALSE, 
                fillOpacity = 0.5, smoothFactor = 0.5, color =~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  textsize = "15px", 
                  direction = "auto"))%>%
    addLegend("topright", pal = pal2, values = ~obs_value,title = paste(as.character(vital_stats_geo()$indicator[1]),paste0("(", as.character(vital_stats_geo()$unit_measure[1]),")")  ) )
  
})



observe({
  
  pal2 <- colorNumeric(
    palette = "Spectral",
    domain = vital_stats_geo()$obs_value,reverse = T
  )
  
  leafletProxy("vital_map") %>%
    clearMarkers()%>%
    clearPopups()%>%
    addPolygons(data = vital_stats_geo(),stroke = FALSE, 
                fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  textsize = "15px", 
                  direction = "auto"))
  
  })

###gdp




gdp_filter <- reactive({
  validate(
    need(input$gdp_country != "", "Please select at least one country")
  )
  
  if(input$gdp_indicator %in% c('Trade Statistics : Total exports',        'Trade Statistics : Imports','Trade Statistics : Trade balance')){
    gdp_pacific%>%
      filter(indicator==input$gdp_indicator & Name %in% c(input$gdp_country) & unit_measure==input$gdp_unit)
    
    
  }else{
  gdp_pacific%>%
    filter(indicator==input$gdp_indicator & Name %in% c(input$gdp_country))}  })


observe(print(input$gdp_update1))

###Bouton
observeEvent(input$gdp_button1, {
  updateBoxSidebar("gdp_update1")
})



###graphs et carte
gdp_geo <-reactive({gdp_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    inner_join(zee,by=c("Alpha_3"="ISO_Ter1"))%>%st_as_sf()
})

output$gdp_plot <- renderPlotly({
  
  if(gdp_filter()$unit_measure[1]!="% of GDP"){
  ggplotly(
  ggplot(gdp_filter(),aes(x=as.numeric(time_period),y=obs_value,color=Name,group=Name,text=text))+
    geom_point()+
    geom_line()+
    theme_bw(base_family = "Montserrat")+
    scale_y_continuous(labels = function(x){scales::dollar(x)})+
    scale_color_manual(values = c25)+
    labs(color="",x="Year",y=paste(as.character(gdp_filter()$indicator[1]),paste0("(", as.character(gdp_filter()$unit_measure[1]),")")  ) ),tooltip = "text")}else{
  ggplotly(
    ggplot(gdp_filter(),aes(x=as.numeric(time_period),y=obs_value,color=Name,group=Name,text=text))+
      geom_point()+
      geom_line()+
      theme_bw(base_family = "Montserrat")+
      scale_y_continuous(labels = function(x){scales::percent(x/100)})+
      scale_color_manual(values = c25)+
      labs(color="",x="Year",y=paste(as.character(gdp_filter()$indicator[1]),paste0("(", as.character(gdp_filter()$unit_measure[1]),")")  ) ),tooltip = "text")}})



output$gdp_plot2 <- renderPlotly({
  if(gdp_filter()$unit_measure[1]!="% of GDP"){
  ggplotly(
  ggplot(gdp_filter()%>%
           group_by(Name)%>%
           filter(time_period==max(time_period))
         ,aes(x=obs_value,y=fct_reorder(Name,obs_value),text=text))+
    geom_segment( aes(yend=Name, xend=0)) +
    geom_point( size=4, aes(color=Name))+
    theme_bw(base_family = "Montserrat")+
    scale_color_manual(values = c25)+
    labs(color="",x=paste(as.character(gdp_filter()$indicator[1]),paste0("(", as.character(gdp_filter()$unit_measure[1]),")")  ),y="" )+
    scale_x_continuous(labels = function(x){scales::dollar(x)})+
    theme(legend.position='none'),tooltip = "text")}else{
      
      ggplotly(
        ggplot(gdp_filter()%>%
                 group_by(Name)%>%
                 filter(time_period==max(time_period))
               ,aes(x=obs_value,y=fct_reorder(Name,obs_value),text=text))+
          geom_segment( aes(yend=Name, xend=0)) +
          geom_point( size=4, aes(color=Name))+
          theme_bw(base_family = "Montserrat")+
          scale_color_manual(values = c25)+
          labs(color="",x=paste(as.character(gdp_filter()$indicator[1]),paste0("(", as.character(gdp_filter()$unit_measure[1]),")")  ),y="" )+
          scale_x_continuous(labels = function(x){scales::percent(x/100)})+
          theme(legend.position='none'),tooltip = "text")   
      
      
      
      
    } 
    
    
    
    
    })


##infobox


text_gdp_indicators_filter <- reactive({
  text_gdp_indicators%>%
    filter(Indicator== input$gdp_indicator)
  
})

output$text_gdp <- renderTable(text_gdp_indicators_filter()%>%select(-c(3,4)))
output$indicator_gdp <- renderText({ text_gdp_indicators_filter()%>%select(1)%>%as_vector()})

output$text_gdp2 <- renderUI({ HTML(text_gdp_indicators_filter()%>%select(3)%>%as_vector())})
output$text_gdp3 <- renderUI({ HTML(text_gdp_indicators_filter()%>%select(4)%>%as_vector())})

gdp_max <- reactive({
  gdp_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_max(obs_value)%>%
    slice(1)
  
})

output$info_box_gdp_max <- renderInfoBox({infoBox(title = "Highest value",paste(format(gdp_max()$obs_value,big.mark=" "),
                                                                                gdp_max()$unit_measure
),subtitle =paste(gdp_max()$Name,"in",
                  gdp_max()$time_period
),icon = tags$i(class = "fas fa-thumbs-up", style="font-size: 48px; color: white") 

)})


gdp_min <- reactive({
  gdp_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_min(obs_value)%>%
    slice(1)
  
})

output$info_box_gdp_min <- renderInfoBox({infoBox(title = "Lowest value",paste(format(gdp_min()$obs_value,big.mark=" "),
                                                                               gdp_min()$unit_measure
),subtitle =paste(gdp_min()$Name,"in",
                  gdp_min()$time_period),color = "red",icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 48px; color: white") 

)})



output$gdp_map <- renderLeaflet({
  
  pal2 <- colorNumeric(
    palette = "Spectral",
    domain = gdp_geo()$obs_value,reverse = T
  )
  
if(gdp_geo()$unit_measure[1]=="% of GDP" ){  
  leaflet(data =gdp_geo()) %>%  addTiles()%>%
    addPolylines(weight = 1,color = "black")%>%
    addPolygons(data = gdp_geo(),stroke = FALSE, 
                fillOpacity = 0.5, smoothFactor = 0.5, color =~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  textsize = "15px", 
                  direction = "auto"))%>%
    addLegend("topright", pal = pal2, values = ~obs_value,title = paste(as.character(gdp_geo()$indicator[1]),paste0("(", as.character(gdp_geo()$unit_measure[1]),")")  ), labFormat = labelFormat( suffix = "%" ))}else{
      
      
      leaflet(data =gdp_geo()) %>%  addTiles()%>%
        addPolylines(weight = 1,color = "black")%>%
        addPolygons(data = gdp_geo(),stroke = FALSE, 
                    fillOpacity = 0.5, smoothFactor = 0.5, color =~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                      style = list("font-weight" = "normal", padding = "3px 8px"), 
                      textsize = "15px", 
                      direction = "auto"))%>%
        addLegend("topright", pal = pal2, values = ~obs_value,title = paste(as.character(gdp_geo()$indicator[1]),paste0("(", as.character(gdp_geo()$unit_measure[1]),")")  ), labFormat = labelFormat( suffix = "$" ))  
    }
  
  
  
})



observe({
  
  pal2 <- colorNumeric(
    palette = "Spectral",
    domain = gdp_geo()$obs_value,reverse = T
  )
  
  leafletProxy("gdp_map") %>%
    clearMarkers()%>%
    clearPopups()%>%
    addPolygons(data = gdp_geo(),stroke = FALSE, 
                fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  textsize = "15px", 
                  direction = "auto"))
  
})

###geography




geography_filter <- reactive({
  
  validate(
    need(input$geography_country != "", "Please select at least one country")
  )
  
  if(input$geography_indicator =='Coastal population'){
    geography_pacific%>%
      filter(indicator==input$geography_indicator & Name %in% c(input$geography_country) & range==input$geography_range)
    
    
  }else{
    geography_pacific%>%
      filter(indicator==input$geography_indicator & Name %in% c(input$geography_country))}  })

observe(print(input$geography_update1))

###Bouton
observeEvent(input$geography_button1, {
  updateBoxSidebar("geography_update1")
})



###graph et carte
geography_geo <-reactive({geography_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    inner_join(zee,by=c("Alpha_3"="ISO_Ter1"))%>%st_as_sf()
})

output$geography_plot <- renderPlotly({
  validate(
    need(input$geography_indicator != "Coastal population", "No evolution available for this indicator, same values for all the period. Please check the 'Last available values' tab.")
  )
  if(geography_filter()$unit_measure[1]!="% of the population"){
    ggplotly(
      ggplot(geography_filter(),aes(x=as.numeric(time_period),y=obs_value,color=Name,group=Name,text=text))+
        geom_point()+
        geom_line()+
        theme_bw(base_family = "Montserrat")+
        scale_y_continuous(labels = function(x){paste(x,"per km²")})+
        scale_color_manual(values = c25)+
        labs(color="",x="Year",y=paste(as.character(geography_filter()$indicator[1]),paste0("(", as.character(geography_filter()$unit_measure[1]),")")  ) ),tooltip = "text")}else{
          ggplotly(
            ggplot(geography_filter(),aes(x=as.numeric(time_period),y=obs_value,color=Name,group=Name,text=text))+
              geom_point()+
              geom_line()+
              theme_bw(base_family = "Montserrat")+
              scale_y_continuous(labels = function(x){scales::percent(x/100)})+
              scale_color_manual(values = c25)+
              labs(color="",x="Year",y=paste(as.character(geography_filter()$indicator[1]),paste0("(", as.character(geography_filter()$unit_measure[1]),")")  ) ),tooltip = "text")}})



output$geography_plot2 <- renderPlotly({

  
  if(geography_filter()$unit_measure[1]!="% of the population"){
    ggplotly(
      ggplot(geography_filter()%>%
               group_by(Name)%>%
               filter(time_period==max(time_period))
             ,aes(x=obs_value,y=fct_reorder(Name,obs_value),text=text))+
        geom_segment( aes(yend=Name, xend=0)) +
        geom_point( size=4, aes(color=Name))+
        theme_bw(base_family = "Montserrat")+
        scale_color_manual(values = c25)+
        labs(color="",x=paste(as.character(geography_filter()$indicator[1]),paste0("(", as.character(geography_filter()$unit_measure[1]),")")  ),y="" )+
        scale_x_continuous(labels = function(x){paste(x,"per km²")})+
        theme(legend.position='none'),tooltip = "text")}else{
          
          ggplotly(
            ggplot(geography_filter()%>%
                     group_by(Name)%>%
                     filter(time_period==max(time_period))
                   ,aes(x=obs_value,y=fct_reorder(Name,obs_value),text=text))+
              geom_segment( aes(yend=Name, xend=0)) +
              geom_point( size=4, aes(color=Name))+
              theme_bw(base_family = "Montserrat")+
              scale_color_manual(values = c25)+
              labs(color="",x=paste(as.character(geography_filter()$indicator[1]),paste0("(", as.character(geography_filter()$unit_measure[1]),")")  ),y="" )+
              scale_x_continuous(labels = function(x){scales::percent(x/100)})+
              theme(legend.position='none'),tooltip = "text")   
          
          
          
          
        } 
  
  
  
  
})



##infobox

text_geography_indicators_filter <- reactive({
  text_geography_indicators%>%
    filter(Indicator== input$geography_indicator)
  
})

output$text_geography <- renderTable(text_geography_indicators_filter()%>%select(-c(3,4)))
output$indicator_geography <- renderText({ text_geography_indicators_filter()%>%select(1)%>%as_vector()})
output$text_geography2 <- renderUI({ HTML(text_geography_indicators_filter()%>%select(3)%>%as_vector())})
output$text_geography3 <- renderUI({ HTML(text_geography_indicators_filter()%>%select(4)%>%as_vector())})

geography_max <- reactive({
  geography_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_max(obs_value)%>%
    slice(1)
  
})

output$info_box_geography_max <- renderInfoBox({infoBox(title = "Highest value",paste(geography_max()$obs_value,
                                                                                      geography_max()$unit_measure
),subtitle =paste(geography_max()$Name,"in",
                  geography_max()$time_period
),icon = tags$i(class = "fas fa-thumbs-up", style="font-size: 48px; color: white") 

)})


geography_min <- reactive({
  geography_filter()%>%
    group_by(Name)%>%
    filter(time_period==max(time_period))%>%
    ungroup()%>%
    slice_min(obs_value)%>%
    slice(1)
  
})

output$info_box_geography_min <- renderInfoBox({infoBox(title = "Lowest value",paste(geography_min()$obs_value,
                                                                                     geography_min()$unit_measure
),subtitle =paste(geography_min()$Name,"in",
                  geography_min()$time_period),color = "red",icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 48px; color: white") 

)})



output$geography_map <- renderLeaflet({
  
  pal2 <- colorNumeric(
    palette = "Spectral",
    domain = geography_geo()$obs_value,reverse = T
  )
  
  if(geography_geo()$unit_measure[1]=="% of the population" ){  
    leaflet(data =geography_geo()) %>%  addTiles()%>%
      addPolylines(weight = 1,color = "black")%>%
      addPolygons(data = geography_geo(),stroke = FALSE, 
                  fillOpacity = 0.5, smoothFactor = 0.5, color =~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "15px", 
                    direction = "auto"))%>%
      addLegend("topright", pal = pal2, values = ~obs_value,title = paste(as.character(geography_geo()$indicator[1]),paste0("(", as.character(geography_geo()$unit_measure[1]),")")  ), labFormat = labelFormat( suffix = "%" ))}else{
        
        
        leaflet(data =geography_geo()) %>%  addTiles()%>%
          addPolylines(weight = 1,color = "black")%>%
          addPolygons(data = geography_geo(),stroke = FALSE, 
                      fillOpacity = 0.5, smoothFactor = 0.5, color =~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                        style = list("font-weight" = "normal", padding = "3px 8px"), 
                        textsize = "15px", 
                        direction = "auto"))%>%
          addLegend("topright", pal = pal2, values = ~obs_value,title = paste(as.character(geography_geo()$indicator[1]),paste0("(", as.character(geography_geo()$unit_measure[1]),")")  ), labFormat = labelFormat( suffix = " habs per km²" ))  
      }
  
  
  
})



observe({
  
  pal2 <- colorNumeric(
    palette = "Spectral",
    domain = geography_geo()$obs_value,reverse = T
  )
  
  leafletProxy("geography_map") %>%
    clearMarkers()%>%
    clearPopups()%>%
    addPolygons(data = geography_geo(),stroke = FALSE, 
                fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal2(obs_value),label =~text   , labelOptions = labelOptions( 
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  textsize = "15px", 
                  direction = "auto"))
  
})


###cluster et heatmap

output$heatmap1 <-renderPlotly({ heatmaply(
  percentize(indicators_matrice%>%
               column_to_rownames("Name")%>%select(-c(Cluster))),
  xlab = "",
  ylab = "", margins = c(40, 130,100,100), 
  main = "",colors = colorRampPalette(rev(brewer.pal(3, "BrBG")))(256),k_col = 3,k_row = 3, grid_gap = 1,
  row_side_colors = indicators_matrice$cluster,show_dendrogram = F,row_side_palette = colorRampPalette(brewer.pal(3, "Set1")),column_text_angle = 90,  plot_method = c("plotly"), hide_colorbar = TRUE) %>%
    plotly::layout(showlegend = FALSE,
                   annotations = list(
                     visible = FALSE
                   ))
})

output$heatmap2 <- renderPlotly({
  
  heatmaply_cor(
    cor(indicators_matrice%>%
          column_to_rownames("Name")%>%select(-c(Cluster,`Volume of remittances`))),
    xlab = "",
    ylab = "", margins = c(40, 130), plot_method = c("plotly"),
    main = "")
  
})

output$table_cluster <- render_gt(
 gtsummary::tbl_summary(indicators_matrice%>%select(-c(Name)),
    by=Cluster , 
    statistic = all_continuous() ~ "{mean} ({min}, {max})"
  ) %>%    
    add_p( )%>%bold_p()%>%
    bold_labels()%>%modify_table_styling(
      columns = label,
      rows = label == "Life expectancy at birth",
      footnote = "For females"
    )%>%modify_table_styling(
      columns = label,
      rows = label %in% c("Trade balance","Volume of remittances"),
      footnote = "In % of GDP"
    )%>%add_overall(last=T)%>% as_gt()%>% 
    tab_header(
      title = "Characteristics of the hierarchical clustering groups",
      subtitle = "Made with last available values for each country"
    ) %>%tab_style(
      style = list(
        cell_fill(color =  "#E41A1C")),
      locations = cells_column_labels(
        columns = vars(stat_1)))%>%
   tab_style(
     style = list(
       cell_fill(color = "#377EB8")),
     locations =cells_column_labels(
       columns = vars(stat_2)))%>%
   tab_style(
     style = list(
       cell_fill(color = "#4DAF4A")),
     locations = cells_column_labels(
       columns = vars(stat_3)))
  
  )

output$map_cluster <- renderLeaflet({
  factpal <- colorFactor(brewer.pal(3,"Set1"), indicators_matrice$Cluster)
  
  
   leaflet(indicators_matrice_b%>%left_join(iso)%>%left_join(text_indicators_matrice)%>%mutate(text=paste(paste(Name,paste0("(",Cluster ,")")),text,sep=" : </p>"))%>%
            inner_join(zee,by=c("Alpha_3"="ISO_Ter1"))%>%st_as_sf())%>%addTiles()%>%
     addPolylines(weight = 1,color = "black")%>%
     addPolygons(stroke = FALSE, smoothFactor = 0.5, fillOpacity = 0.5,label =~lapply(text,htmltools::HTML)   , labelOptions = labelOptions( 
                   style = list("font-weight" = "normal", padding = "3px 8px"), 
                   textsize = "15px", 
                   direction = "auto"),color = ~factpal(Cluster))%>%addLegend("topright", pal = factpal, values = ~Cluster)
     
})
    }
    

    
    shinyApp(ui = ui, server = server)
    
