# Visualizations

library(ggplot2)
library(tidyverse)
library(magrittr)
library(sf)
library(shiny)
library(leaflet)

mocodata_viz <- read_rds("mocodata_with_scores.rds") %>%  select(contains("score"),valuerank,bill_total_median) %>% 
  mutate(html = paste0("<strong>Adjusted value rank:</strong> ",valuerank,
                       "<br><em>Raw value score:</em> ",round(valuescore),
                       "<br><em>Median property tax bill:</em> ",round(bill_total_median),
                       "<br><small><hr><strong>Raw subscores</strong>",
                       "<br>Schools: ",round(score_school),
                                            "<br>Crime: ",round(score_crime),
                                            "<br>Home values: ",round(score_homevalue),
                                            "<br>Services: ",round(score_services),"</small>"))

mocodata_viz %<>% mutate(valueresid = resid(lm(bill_total_median ~ valuescore,data=mocodata_viz,na.action = na.exclude)),
                     valuerankrev = -valueresid %>% rank(na.last="keep"))


vardesc <- c("disprank" = "Tracts are ranked by the difference between their actual median property tax bill and their predicted bill based on their value score. Lower is better.",
             "dispscore" = "A composite score of quality and services for a tract, with four equally-weighted components: schools, crime, home values, and services. Higher is better.",
             "score_school" = "A composite score of high school quality based on SAT scores, % passing AP or IB tests, % meeting UMD graduation requirements, and graduation rate, on average, for all schools serving the tract.",
             "score_school_esol" = "A composite score of high school quality based on SAT scores, % passing AP or IB tests, % meeting UMD graduation requirements, and graduation rate, on average, for all schools serving the tract, then further adjusted based on % of English for Speakers of Other Languages students served.",
             "score_school_farms" ="A composite score of high school quality based on SAT scores, % passing AP or IB tests, % meeting UMD graduation requirements, and graduation rate, on average, for all schools serving the tract, then further adjusted based on % of students eligible for Free And Reduced Meals.",
             "score_crime" = "Based on crime reported in 2018 per capita within the tract, compared to other tracts.",
             "score_homevalue" ="Median home values as reported in 5-year ACS estimates within the tract.",
             "score_services" = "A composite score based on access to public transportation, emergency services, community facilities, polling places, and high-speed internet.",
             "bill_total_median" ="Median property tax bill for all principal residences in the tract.")

# Shiny -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Montgomery County 'Value Score' Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("displayscore","Map type",
                   c("Adjusted value rank"="disprank",
                     "Raw value score"="dispscore",
                     "Median property tax bill" = "bill_total_median",
                     list("Schools" = c("Unadjusted" = "score_school",
                                        "Adjust for ESOL" = "score_school_esol",
                                        "Adjust for FARMS" = "score_school_farms")),
                     "Crime"="score_crime",
                     "Home values"="score_homevalue",
                     "Services"="score_services"),
                  selected = "Adjusted value rank"
                   ),
      
      textOutput("vardesc"),
      
      hr(),
      checkboxInput("customweights", "Customize?", value = FALSE),
      numericInput("weight_schools", "Schools", value = .25, min = 0, max = 1),
      radioButtons("adjust_school", "Adjust school score for...", inline=TRUE, c("None" = "score_school", "ESOL" = "score_school_esol", "FARMS" = "score_school_farms")),
      numericInput("weight_crime", "Crime", value = .25, min = 0, max = 1),
      numericInput("weight_homevalue", "Home values", value = .25, min = 0, max = 1),
      numericInput("weight_services", "Services", value = .25, min = 0, max = 1),
      actionButton("go_weights","Reweight"),
      
      HTML("<hr><small>Made with ✨ for SURV727 at the University of Maryland by <a href = 'https://github.com/sofisinozich'> Sofi Sinozich</a></small>")
           ),
   mainPanel(
      leafletOutput("mocomap")
  )
)
)


server <- function(input, output, session) {
  
  values <- reactiveValues(mdata = mocodata_viz %>% st_transform(crs=4326) %>% 
                             mutate(disprank = valuerankrev,
                                    dispscore = valuescore))

  
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
    if (!is.na(input$displayscore) & input$displayscore == "disprank") {
      leafletProxy("mocomap", data = values$mdata) %>%
        clearControls() %>%
        addLegend("topright",pal=pal,values=values$mdata[[input$displayscore]], 
                  labFormat = labelFormat(transform = function(x) 213-x))
    }
    
    else {
      leafletProxy("mocomap", data = values$mdata) %>%
        clearControls() %>%
        addLegend("topright",pal=pal,values=values$mdata[[input$displayscore]]) 
    }
  })
  
  observeEvent(input$customweights, {
    if (input$customweights == 0) {
      values$mdata$dispscore <- values$mdata$valuescore
      values$mdata$disprank <- values$mdata$valuerankrev
      
      updateNumericInput(session,"weight_schools", value = 0.25)
      updateNumericInput(session,"weight_crime", value = 0.25)
      updateNumericInput(session,"weight_homevalue", value = 0.25)
      updateNumericInput(session,"weight_services", value = 0.25)
      
      values$mdata$html <- paste0("<strong>Rank:</strong> ",values$mdata$valuerank,
                                  "<br><em>Overall Score:</em> ",round(values$mdata$valuescore),
                                  "<br><em>Median Property Tax Bill:</em> ",round(values$mdata$bill_total_median),
                                  "<br><small>Schools: ",round(values$mdata$score_school),
                                  "<br>Crime: ",round(values$mdata$score_crime),
                                  "<br>Home values: ",round(values$mdata$score_homevalue),
                                  "<br>Services: ",round(values$mdata$score_services),"</small>")
      
      updateSelectInput(session,"displayscore", choices = 
                          c("Adjusted value rank"="disprank",
                            "Raw value score"="dispscore",
                            "Median property tax bill" = "bill_total_median",
                            list("Schools" = c("Unadjusted" = "score_school",
                                               "Adjust for ESOL" = "score_school_esol",
                                               "Adjust for FARMS" = "score_school_farms")),
                            "Crime"="score_crime",
                            "Home values"="score_homevalue",
                            "Services"="score_services"),
                        selected = "Adjusted value rank")
      
      
      pal <- palette()
      
      leafletProxy("mocomap", data = values$mdata) %>%
        clearShapes() %>%
        addPolygons(color="#000000",weight=.5,fillOpacity=.75,
                    fillColor = ~pal(values$mdata[[input$displayscore]]), 
                    popup = values$mdata[["html"]]) 
      

      leafletProxy("mocomap", data = values$mdata) %>%
        clearControls() %>%
        addLegend("topright",pal=pal,values=values$mdata[[input$displayscore]])
    }
    
    else {
      updateSelectInput(session,"displayscore", choices = 
                          c("Adjusted value rank"="disprank",
                          "Raw value score"="dispscore"),
                        selected = "Adjusted value rank")
    }
  })
  
  observeEvent(input$go_weights, {
    if(input$customweights) {
      if (input$adjust_school == "score_school") {
        values$mdata$dispscore <- values$mdata$score_school*input$weight_schools + values$mdata$score_crime*input$weight_crime + values$mdata$score_homevalue*input$weight_homevalue + values$mdata$score_services*input$weight_services
        values$mdata$disprank <- -resid(lm(bill_total_median ~ dispscore,data=values$mdata,na.action = na.exclude)) %>% rank(na.last="keep")
        values$mdata$html <- paste0("<strong>Rank <span style='color:red'>(customized)</span>:</strong> ",values$mdata$disprank,
                                      "<br><em>Overall Score <span style='color:red'>(customized)</span>:</em> ",round(values$mdata$dispscore),
                                      "<br><em>Median Property Tax Bill:</em> ",round(values$mdata$bill_total_median),
                                      "<br><small>Schools: ",round(values$mdata$score_school),
                                      "<br>Crime: ",round(values$mdata$score_crime),
                                      "<br>Home values: ",round(values$mdata$score_homevalue),
                                      "<br>Services: ",round(values$mdata$score_services),"</small>")
      }
      if (input$adjust_school == "score_school_esol") {
        values$mdata$dispscore <- values$mdata$score_school_esol*input$weight_schools + values$mdata$score_crime*input$weight_crime + values$mdata$score_homevalue*input$weight_homevalue + values$mdata$score_services*input$weight_services
        values$mdata$disprank <- -resid(lm(bill_total_median ~ dispscore,data=values$mdata,na.action = na.exclude)) %>% rank(na.last="keep")
        values$mdata$html <- paste0("<strong>Rank <span style='color:red'>(customized)</span>:</strong> ",values$mdata$disprank,
                                    "<br><em>Overall Score <span style='color:red'>(customized)</span>:</em> ",round(values$mdata$dispscore),
                                    "<br><em>Median Property Tax Bill:</em> ",round(values$mdata$bill_total_median),
                                    "<br><small>Schools <span style='color:red'>(ESOL adjusted)</span>: ",round(values$mdata$score_school_esol),
                                    "<br>Crime: ",round(values$mdata$score_crime),
                                    "<br>Home values: ",round(values$mdata$score_homevalue),
                                    "<br>Services: ",round(values$mdata$score_services),"</small>")
        
      }
      if (input$adjust_school == "score_school_farms") {
        values$mdata$dispscore <- values$mdata$score_school_farms*input$weight_schools + values$mdata$score_crime*input$weight_crime + values$mdata$score_homevalue*input$weight_homevalue + values$mdata$score_services*input$weight_services
        values$mdata$disprank <- -resid(lm(bill_total_median ~ dispscore,data=values$mdata,na.action = na.exclude)) %>% rank(na.last="keep")
        values$mdata$html <- paste0("<strong>Rank <span style='color:red'>(customized)</span>:</strong> ",values$mdata$disprank,
                                    "<br><em>Overall Score <span style='color:red'>(customized)</span>:</em> ",round(values$mdata$dispscore),
                                    "<br><em>Median Property Tax Bill:</em> ",round(values$mdata$bill_total_median),
                                    "<br><small>Schools <span style='color:red'>(FARMS adjusted)</span>: ",round(values$mdata$score_school_farms),
                                    "<br>Crime: ",round(values$mdata$score_crime),
                                    "<br>Home values: ",round(values$mdata$score_homevalue),
                                    "<br>Services: ",round(values$mdata$score_services),"</small>")
      
      }

      
      pal <- palette()
      
      leafletProxy("mocomap", data = values$mdata) %>%
        clearShapes() %>%
        addPolygons(color="#000000",weight=.5,fillOpacity=.75,
                    fillColor = ~pal(values$mdata[[input$displayscore]]), 
                    popup = values$mdata[["html"]]) 
      
      
      leafletProxy("mocomap", data = values$mdata) %>%
        clearControls() %>%
        addLegend("topright",pal=pal,values=values$mdata[[input$displayscore]])
    }
  })

  output$vardesc <- renderText(vardesc[[input$displayscore]])
  
}

shinyApp(ui, server)
