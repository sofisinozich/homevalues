# Visualizations

library(ggplot2)
library(tidyverse)
library(magrittr)
library(sf)
library(shiny)
library(leaflet)

mocodata_viz <- read_rds("mocodata_with_scores.rds") %>%  select(contains("score"),valuerank,bill_total_median) %>% 
  mutate(html = paste0("<strong>Rank:</strong> ",valuerank,
                       "<br><em>Overall Score:</em> ",round(valuescore),
                       "<br><em>Median Property Tax Bill:</em> ",round(bill_total_median),
                       "<br><small>Schools: ",round(score_school),
                                            "<br>Crime: ",round(score_crime),
                                            "<br>Home values: ",round(score_homevalue),
                                            "<br>Services: ",round(score_services),"</small>"))


# Shiny -------------------------------------------------------------------
ui <- fluidPage(
  includeCSS("style.css"),
  
  titlePanel("Montgomery County 'Value Score'"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("displayscore","Map type",
                   c("Overall value score vs. property tax bill rank (lower is better)"="valuerank",
                     "Overall value score"="valuescore",
                     "Schools"="score_school",
                     "Crime"="score_crime",
                     "Home values"="score_homevalue",
                     "Services"="score_services",
                     "Median property tax bill" = "bill_total_median")
                   ),
      checkboxInput("customweights", "Use custom weights?", value = FALSE),
      numericInput("weight_schools", "Schools", value = .25, min = 0, max = 1),
      numericInput("weight_crime", "Crime", value = .25, min = 0, max = 1),
      numericInput("weight_homevalue", "Home values", value = .25, min = 0, max = 1),
      numericInput("weight_services", "Services", value = .25, min = 0, max = 1),
      actionButton("go_weights","Reweight")
      ),
  
   mainPanel(
      leafletOutput("mocomap")
  )
)
)


server <- function(input, output, session) {
  
  values <- reactiveValues(mdata = mocodata_viz %>% st_transform(crs=4326))

  
  # General map
  output$mocomap <- renderLeaflet({
    if (input$customweights) {
      validate(
        need(input$weight_schools + input$weight_crime + input$weight_homevalue + input$weight_services == 1, 'Weights must add up to 1.')
      )
    }
    leaflet(values$mdata) %>% addTiles %>% setView(-77.157165,39.156025,zoom=10)
  })
  
  # Regenerate color palette depending on selected variable
  palette <- reactive({
    colorNumeric("RdYlBu", values$mdata[[input$displayscore]])
  })
  
  # Update leaflet map polygons based on selected variable values
  observe({
    pal <- palette()
    
    leafletProxy("mocomap", data = values$mdata) %>%
      clearShapes() %>%
      addPolygons(color="#000000",weight=.5,fillOpacity=.75,
                  fillColor = ~pal(values$mdata[[input$displayscore]]), 
                  popup = values$mdata[["html"]]) 
    
  })
  
  # Update legend based on selected variable values and new scale
  observe ({
    pal <- palette()
    leafletProxy("mocomap", data = values$mdata) %>%
      clearControls() %>%
      addLegend("topright",pal=pal,values=values$mdata[[input$displayscore]])
  })
  
  observeEvent(input$go_weights, {
    values$mdata$valuescore <- values$mdata$score_school*input$weight_schools + values$mdata$score_crime*input$weight_crime + values$mdata$score_homevalue*input$weight_homevalue + values$mdata$score_services*input$weight_services
    
    values$mdata$valuerank <- resid(lm(bill_total_median ~ valuescore,data=values$mdata,na.action = na.exclude)) %>% rank(na.last="keep")
    
    values$mdata$html <- paste0("<strong>Rank:</strong> ",values$mdata$valuerank,
                                       "<br><em>Overall Score:</em> ",round(values$mdata$valuescore),
                                       "<br><em>Median Property Tax Bill:</em> ",round(values$mdata$bill_total_median),
                                       "<br><small>Schools: ",round(values$mdata$score_school),
                                       "<br>Crime: ",round(values$mdata$score_crime),
                                       "<br>Home values: ",round(values$mdata$score_homevalue),
                                       "<br>Services: ",round(values$mdata$score_services),"</small>")
  })

}

shinyApp(ui, server)
