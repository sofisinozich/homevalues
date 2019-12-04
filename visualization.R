# Visualizations

library(ggplot2)
library(leaflet)
library(tidyverse)
library(DT)

# Descriptions here
descriptions <- read_csv("varref.csv")

library(shiny)
library(leaflet)

mocodata %<>% select(STATEFP:assessment_mean,assessment_median,bill_total_mean,bill_total_median)
# Data cleanup - rounding and conversions
mocodata %<>% mutate_at(vars(avg.num.providers:bill_total_median),round,2) %>% #Rounding
  mutate_at(vars(ends_with("dist")),funs(./1609.34)) #Convert meters to miles

# Shiny -------------------------------------------------------------------
ui <- fluidPage(
  includeCSS("style.css"),
  
  titlePanel("Montgomery County Characteristics by Tract"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("factor",label="Select characteristic:",choices=descriptions$name[13:40]),
      htmlOutput("factor_desc"),
      radioButtons("scaling","Scale counts by:",
                   c("No scaling"="none",
                     "Population (per 1000 people)"="pop",
                     "Area (per square mile)"="area")),
      textOutput("warning")
    ),
    
    mainPanel(
      leafletOutput("mocomap"),
      DT::dataTableOutput("dt")
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive data to scale the counts by population and area based on user preference
  mdata <- reactive({
    data <- mocodata %>% st_transform(crs=4326)
    data <- switch (input$scaling,
                    "none" = data,
                    "pop" = data %<>% mutate_at(vars(ends_with("count")),funs(1000*./population)),
                    "area" = data %<>% mutate_at(vars(ends_with("count")),funs(2590000*./ALAND))
    )
    data
  })
  
  # General map
  output$mocomap <- renderLeaflet({
    mdata <- mdata()
    leaflet(mdata) %>% addTiles %>% setView(-77.157165,39.156025,zoom=10)
  })
  
  # Regenerate color palette depending on selected variable
  palette <- reactive({
    mdata <- mdata()
    values <- mdata[[input$factor]]
    # Do something diff for factors and those with low unique values
    if (values %>% is.character | values %>% is.factor)
    {
      colorFactor("RdYlBu",mdata[[input$factor]])
    }
    else {
      if (grepl("dist|bill_total",input$factor)) {
        # Bigger distance/higher taxes = bad!
        colorBin("RdYlBu",mdata[[input$factor]],reverse=TRUE)
      }
      else {
        # For everything else, higher is "better"
        colorBin("RdYlBu",mdata[[input$factor]])
      }
    }
  })
  
  # Update leaflet map polygons based on selected variable values
  observe({
    mdata <- mdata()
    pal <- palette()
    
    leafletProxy("mocomap", data = mdata) %>%
      clearShapes() %>%
      addPolygons(color="#000000",weight=.5,fillOpacity=.75,
                  fillColor = ~pal(mdata[[input$factor]]), 
                  label = round(mdata[[input$factor]],2)) 
    
  })
  
  # Update legend based on selected variable values and new scale
  observe ({
    mdata <- mdata()
    pal <- palette()
    leafletProxy("mocomap", data = mdata) %>%
      clearControls() %>%
      addLegend("topright",pal=pal,values=mdata[[input$factor]])
  })
  
  # Update sidebar with information about the variable
  output$factor_desc <- renderText(paste0("<h3>",descriptions$descname[descriptions$name==input$factor],"</h3><p />",
                                          descriptions$desctext[descriptions$name == input$factor],
                                          "<p /><em>Source: ",descriptions$source[descriptions$name == input$factor],"</em><hr>"))
  
  # Update table to sort by the chosen variable
  output$dt <- DT::renderDataTable({
    DT::datatable(mocodata %>% st_drop_geometry %>% select(NAME,avg.num.providers:bill_total_median), 
                  rownames = FALSE,
                  options=list(scrollX = TRUE,
                               pageLength = 10,
                               dom='tp',
                               order=list(which(descriptions$name == input$factor)-12, "desc")))
  })
  
  # A warning regarding weird results when dividing small numbers by potentially large numbers
  output$warning <- renderText("Warning! Scaling may have unexpected display results on infrequently occurring features. Scaling only impacts _count variables.")
}

shinyApp(ui, server)
