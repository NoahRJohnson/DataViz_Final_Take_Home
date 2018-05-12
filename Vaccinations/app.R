library(shiny)
library(tidyverse)
library(plotly)
library(dslabs)

# Load Vaccine data
load(file="data/MMR.Rda")
load(file="data/HepA.Rda")
load(file="data/DTaP.Rda")

#####################################################
###       Prepare Data                            ###
#####################################################

diseases <- us_contagious_diseases %>% filter(1995 <= year & year <= 2011)

diseases <- diseases %>% 
  filter(!(disease == 'Hepatitis A' & year < 2002))

diseases <- diseases %>% mutate(weighted_rate = (count*52 / weeks_reporting) / population)

diseases.MMR <- diseases %>% 
  filter(disease == 'Measles' | 
           disease == 'Mumps' | 
           disease == 'Rubella') %>% 
  select(state, year, weighted_rate)

diseases.HepA <- diseases %>% 
  filter(disease == 'Hepatitis A') %>% 
  select(state, year, weighted_rate)

diseases.DTaP <- diseases %>% 
  filter(disease == 'Pertussis') %>% 
  select(state, year, weighted_rate)

MMR <- MMR %>% select(state, as.character(1995:2002))

HepA <- HepA %>% select(state, as.character(2002:2011)) # also remove first two column of NA's

DTaP <- DTaP %>% select(state, as.character(1995:2011))

postal_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", 
                          "CO", "CT", "DE", "DC", "FL", 
                          "GA", "HI", "ID", "IL", "IN", 
                          "IA", "KS", "KY", "LA", "ME", 
                          "MD", "MA", "MI", "MN", "MS", 
                          "MO", "MT", "NE", "NV", "NH", 
                          "NJ", "NM", "NY", "NC", "ND", 
                          "OH", "OK", "OR", "PA", "RI", 
                          "SC", "SD", "TN", "TX", "UT", 
                          "VT", "VA", "WA", "WV", "WI", 
                          "WY")

#####################################################
###            Create Choropleths                 ###
#####################################################

MMR.avg.over.year <- MMR %>% select(as.character(1995:2002)) %>% rowMeans(na.rm = TRUE)

MMR$avg.year <- MMR.avg.over.year
MMR$state.abr <- postal_abbreviations

MMR.avg.over.year <- MMR %>% select(state.abr, avg.year)
head(MMR.avg.over.year)

HepA.avg.over.year <- HepA %>% select(as.character(2002:2011)) %>% rowMeans(na.rm = TRUE)

HepA$avg.year <- HepA.avg.over.year
HepA$state.abr <- postal_abbreviations

HepA.avg.over.year <- HepA %>% select(state.abr, avg.year)
head(HepA.avg.over.year)

DTaP.avg.over.year <- DTaP %>% select(as.character(1995:2011)) %>% rowMeans(na.rm = TRUE)

DTaP$avg.year <- DTaP.avg.over.year
DTaP$state.abr <- postal_abbreviations

DTaP.avg.over.year <- DTaP %>% select(state.abr, avg.year)

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

viz.choropleth.MMR <- MMR.avg.over.year %>% plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~avg.year, locations = ~state.abr, #text = ~hover,
    color = ~avg.year, colors = 'Purples'
  ) %>%
  colorbar(title = "Percent<br>Vaccine<br>Coverage") %>%
  layout(
    title = 'Average MMR Vaccine Coverage<br>Among Children 19-35 Months Old<br>(1995 - 2002)',
    geo = g
  )

viz.choropleth.HepA <- HepA.avg.over.year %>% plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~avg.year, locations = ~state.abr, #text = ~hover,
    color = ~avg.year, colors = 'Purples'
  ) %>%
  colorbar(title = "Percent<br>Vaccine<br>Coverage") %>%
  layout(
    title = 'Average Hepatitis A Vaccine Coverage<br>Among Children 19-35 Months Old<br>(2002 - 2011)',
    geo = g
  )

viz.choropleth.DTaP <- DTaP.avg.over.year %>% plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~avg.year, locations = ~state.abr, #text = ~hover,
    color = ~avg.year, colors = 'Purples'
  ) %>%
  colorbar(title = "Percent<br>Vaccine<br>Coverage") %>%
  layout(
    title = 'Average DTaP Vaccine Coverage<br>Among Children 19-35 Months Old<br>(1995 - 2011)',
    geo = g
  )

diseases.MMR.over.year <- diseases.MMR %>% 
  group_by(state) %>% 
  summarise(avg.year = mean(weighted_rate, na.rm=TRUE))
diseases.MMR.over.year$state.abr <- postal_abbreviations

diseases.HepA.over.year <- diseases.HepA %>% 
  group_by(state) %>% 
  summarise(avg.year = mean(weighted_rate, na.rm=TRUE))
diseases.HepA.over.year$state.abr <- postal_abbreviations

diseases.DTaP.over.year <- diseases.DTaP %>% 
  group_by(state) %>% 
  summarise(avg.year = mean(weighted_rate, na.rm=TRUE))
diseases.DTaP.over.year$state.abr <- postal_abbreviations

viz.choropleth.diseases.MMR <- diseases.MMR.over.year %>% plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~avg.year, locations = ~state.abr,
    color = ~avg.year, colors = 'Purples'
  ) %>%
  colorbar(title = "Percent<br>Occurrence") %>%
  layout(
    title = 'Average Measles, Mumps, or Rubella Rates<br>(1995 - 2002)',
    geo = g
  )

viz.choropleth.diseases.HepA <- diseases.HepA.over.year %>% plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~avg.year, locations = ~state.abr,
    color = ~avg.year, colors = 'Purples'
  ) %>%
  colorbar(title = "Percent<br>Occurrence") %>%
  layout(
    title = 'Average Hepatitis A Rates<br>(2002 - 2011)',
    geo = g
  )


viz.choropleth.diseases.DTaP <- diseases.DTaP.over.year %>% plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~avg.year, locations = ~state.abr,
    color = ~avg.year, colors = 'Purples'
  ) %>%
  colorbar(title = "Percent<br>Occurrence") %>%
  layout(
    title = 'Average Pertussis (Whooping Cough) Rates<br>(1995 - 2011)',
    geo = g
  )


#####################################################
###            Create Time Series                 ###
#####################################################

MMR.avg.over.state <- MMR %>% 
  select(as.character(1995:2002)) %>%  # get the rate data for years we have data for
  summarise_all(funs(mean(.))) %>%  # grab column means
  reshape2::melt()

HepA.avg.over.state <- HepA %>% 
  select(as.character(2002:2011)) %>%  # get the rate data for each year
  summarise_all(funs(mean(.))) %>%  # grab column means
  reshape2::melt()

DTaP.avg.over.state <- DTaP %>% 
  select(as.character(1995:2011)) %>%  # get the rate data for each year
  summarise_all(funs(mean(.))) %>%  # grab column means
  reshape2::melt()

viz.ts.MMR <- MMR.avg.over.state %>% 
  plot_ly(x = ~variable,
          y = ~value, 
          type = 'scatter',
          mode = 'lines'
  ) %>%
  layout(title = "Average US MMR Vaccine Coverage<br>Among Children 19-35 Months Old",
         xaxis = list(title="Year"), 
         yaxis = list(title="Percent Coverage"))

viz.ts.HepA <- HepA.avg.over.state %>% 
  plot_ly(x = ~variable,
          y = ~value, 
          type = 'scatter',
          mode = 'lines'
  ) %>%
  layout(title = "Average US Hepatitis A Vaccine Coverage<br>Among Children 19-35 Months Old",
         xaxis = list(title="Year"), 
         yaxis = list(title="Percent Coverage"))

viz.ts.DTaP <- DTaP.avg.over.state %>% 
  plot_ly(x = ~variable,
          y = ~value, 
          type = 'scatter',
          mode = 'lines'
  ) %>%
  layout(title = "Average US DTaP Vaccine Coverage<br>Among Children 19-35 Months Old",
         xaxis = list(title="Year"), 
         yaxis = list(title="Percent Coverage"))

diseases.MMR.avg.over.state <- diseases.MMR %>% 
  group_by(year) %>% 
  summarise(avg.state = mean(weighted_rate, na.rm=TRUE)) %>% 
  drop_na()

diseases.HepA.avg.over.state <- diseases.HepA %>% 
  group_by(year) %>% 
  summarise(avg.state = mean(weighted_rate, na.rm=TRUE)) %>% 
  drop_na()

diseases.DTaP.avg.over.state <- diseases.DTaP %>% 
  group_by(year) %>% 
  summarise(avg.state = mean(weighted_rate, na.rm=TRUE)) %>% 
  drop_na()


viz.ts.diseases.MMR <- diseases.MMR.avg.over.state %>% 
  plot_ly(x = ~year,
          y = ~avg.state, 
          type = 'scatter',
          mode = 'lines'
  ) %>%
  layout(title = "Average Measles, Mumps, or Rubella Rates<br>(1995 - 2002)",
         xaxis = list(title="Year"), 
         yaxis = list(title="Percent Coverage"))

viz.ts.diseases.HepA <- diseases.HepA.avg.over.state %>% 
  plot_ly(x = ~year,
          y = ~avg.state, 
          type = 'scatter',
          mode = 'lines'
  ) %>%
  layout(title = "Average Hepatitis A Rates<br>(2002 - 2011)",
         xaxis = list(title="Year"), 
         yaxis = list(title="Percent Coverage"))

viz.ts.diseases.DTaP <- diseases.DTaP.avg.over.state %>% 
  plot_ly(x = ~year,
          y = ~avg.state, 
          type = 'scatter',
          mode = 'lines'
  ) %>%
  layout(title = "Average Pertussis (Whooping Cough) Rates<br>(1995 - 2011)",
         xaxis = list(title="Year"), 
         yaxis = list(title="Percent Coverage"))

#####################################################

# Define UI as tabbed page
ui <- navbarPage(
  theme = "app.css",
  title = "Vaccinations",
  tabPanel("MMR",
           fluidRow(
             column(width = 6, plotlyOutput("MMR.choropleth.vaccine")),
             column(width = 6, plotlyOutput("MMR.choropleth.disease"))
           ),
           fluidRow(
             column(width = 6, plotlyOutput("MMR.ts.vaccine")),
             column(width = 6, plotlyOutput("MMR.ts.disease"))
           )
  ),
  tabPanel("Hep A",
           fluidRow(
             column(width = 6, plotlyOutput("HepA.choropleth.vaccine")),
             column(width = 6, plotlyOutput("HepA.choropleth.disease"))
           ),
           fluidRow(
             column(width = 6, plotlyOutput("HepA.ts.vaccine")),
             column(width = 6, plotlyOutput("HepA.ts.disease"))
           )
  ),
  tabPanel("DTaP",
           fluidRow(
             column(width = 6, plotlyOutput("DTaP.choropleth.vaccine")),
             column(width = 6, plotlyOutput("DTaP.choropleth.disease"))
           ),
           fluidRow(
             column(width = 6, plotlyOutput("DTaP.ts.vaccine")),
             column(width = 6, plotlyOutput("DTaP.ts.disease"))
           )
  )
)


# Define server logic required to draw vizzes
server <- function(input, output) {
   
   # Vaccine Choropleths
   output$MMR.choropleth.vaccine <- renderPlotly({
     return(viz.choropleth.MMR)
   })
   
   output$HepA.choropleth.vaccine <- renderPlotly({
     return(viz.choropleth.HepA)
   })
   
   output$DTaP.choropleth.vaccine <- renderPlotly({
     return(viz.choropleth.DTaP)
   })
   
   # Disease Choropleths
   output$MMR.choropleth.disease <- renderPlotly({
     return(viz.choropleth.diseases.MMR)
   })
   
   output$HepA.choropleth.disease <- renderPlotly({
     return(viz.choropleth.diseases.HepA)
   })
   
   output$DTaP.choropleth.disease <- renderPlotly({
     return(viz.choropleth.diseases.DTaP)
   })
   
   # Vaccine Time Series
   output$MMR.ts.vaccine <- renderPlotly({
     return(viz.ts.MMR)
   })
   
   output$HepA.ts.vaccine <- renderPlotly({
     return(viz.ts.HepA)
   })
   
   output$DTaP.ts.vaccine <- renderPlotly({
     return(viz.ts.DTaP)
   })
   
   # Disease Time Series
   output$MMR.ts.disease <- renderPlotly({
     return(viz.ts.diseases.MMR)
   })
   
   output$HepA.ts.disease <- renderPlotly({
     return(viz.ts.diseases.HepA)
   })
   
   output$DTaP.ts.disease <- renderPlotly({
     return(viz.ts.diseases.DTaP)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

