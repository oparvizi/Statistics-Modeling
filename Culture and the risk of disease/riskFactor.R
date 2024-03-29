# source: https://shiny.rstudio.com/gallery/risk-watch.html
library(shiny)
library(dplyr)
library(forcats)
library(leaflet)
library(opencage)
library(highcharter)
library(readxl)

ukh_risk <- function(lat,
                      lng,
                      date = NULL
){
  
  # if date is used
  if (is.null(date) == FALSE) {
    
    result <- read_excel("riskFactor.xlsx")
    # ukp_api(glue::glue("api/crimes-street/all-crime?lat={lat}&lng={lng}&date={date}"))
    
    # else if no date is specified
  } else if (is.null(date) == TRUE) {
    
    result <- read_excel("riskFactor.xlsx")
    #ukp_api(glue::glue("api/crimes-street/all-crime?lat={lat}&lng={lng}"))
    
  }
  
  extract_result <- purrr::map_dfr(.x = result$content,
                                   .f = ukp_crime_unlist)
  
  # rename the data
  extract_result <- dplyr::rename(
    extract_result,
    lat = location.latitude,
    long = location.longitude,
    street_id = location.street.id,
    street_name = location.street.name,
    date = month,
    outcome_status = outcome_status.category,
    outcome_date = outcome_status.date
  )
  
  
  final_result <- dplyr::mutate(extract_result,
                                lat = as.numeric(lat),
                                long = as.numeric(long))
  
  final_result <- dplyr::select(final_result,
                                category,
                                persistent_id,
                                date,
                                lat,
                                long,
                                street_id,
                                street_name,
                                context,
                                id,
                                location_type,
                                location_subtype,
                                outcome_status,
                                category)
  
  return(final_result)
} 

ukh_risk <- read_excel("riskFactor.xlsx")

rank <- factor(ukh_risk$category)
#View(riskFactor)

ui <- bootstrapPage(
  
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
#    includeHTML("meta.html"),
#    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
#                type="text/javascript"),
    tags$script('
                $(document).ready(function () {
                  navigator.geolocation.getCurrentPosition(onSuccess, onError);
                
                  function onError (err) {
                    Shiny.onInputChange("geolocation", false);
                  }
                
                  function onSuccess (position) {
                    setTimeout(function () {
                      var coords = position.coords;
                      console.log(coords.latitude + ", " + coords.longitude);
                      Shiny.onInputChange("geolocation", true);
                      Shiny.onInputChange("lat", coords.latitude);
                      Shiny.onInputChange("long", coords.longitude);
                    }, 1100)
                  }
                });
                ')
  ),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    top = 10, right = 10, style = "z-index:500; text-align: right;",
    tags$h2("Culture and the risk of disease"),
    tags$a("About this tool", href="https://xxxx")
  ),
  
  absolutePanel(
    top = 100, left = 10, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",
    textInput("geocode", "Type an address or location", placeholder = "in England, Wales or NI"),
    checkboxInput("use_location", "Or use your current location?"),
    actionButton("go", "Risk Factor!", class = "btn-primary"),
    highchartOutput("selectstat")
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-3, 54.3, zoom = 6)
  })
  
  observeEvent(input$go, {
    
    withProgress(
      message = 'Fetching data from xxxx...',
      value = 1/5, {
        
        if (input$use_location) {
          
          validate(
            need(input$geolocation, message = FALSE)
          )
          
          lat <- input$lat
          long <- input$long
          
          place <- opencage_reverse(lat, long, countrycode = "GB")
          place <- as.character(place$results$components.postcode[1])
          
        } else {
          
          validate(
            need(nchar(input$geocode > 2), message = FALSE)
          )
          
          geoc <- opencage_forward(placename = input$geocode, countrycode = "GB")
          
          lat <- geoc$results$geometry.lat[1]
          long <- geoc$results$geometry.lng[1]
          
          place = input$geocode
          
        }
        
        incProgress(1/5)
        
        tryCatch({
          risk <- ukh_risk(lat, long) %>% 
            mutate(date = as.Date(paste0(date, "-01"))) %>% 
            mutate(date = format(date, format = "%B %Y")) %>% 
            mutate(top5 = fct_lump(factor(category), n = 5, other_level = "hover-for-detail"))
          
          risk_rank <- risk %>%
            count(category) %>%
            arrange(desc(n))
          
        },
        error = function(e) {
          risk <- NULL
        }
        )
        
        incProgress(1/5)
        
        tryCatch({
          pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")
          leafPal <- colorFactor(pal, risk$top5)
          
          leafletProxy("map", data = risk) %>%
            setView(long, lat, zoom = 14) %>% 
            clearShapes() %>% 
            clearControls() %>% 
            addCircles(~long, ~lat, stroke = FALSE, fill = TRUE, fillOpacity = .7, 
                       color = ~leafPal(top5), label = ~category, radius = 30) %>% 
            addLegend("bottomright", pal = leafPal, values = ~top5, title = "Category")
          
          incProgress(1/5)
          
          output$selectstat <- renderHighchart({
            
            hchart(risk_rank, "bar", hcaes(category, n)) %>% 
              hc_colors("SteelBlue") %>% 
              hc_title(text = paste("risks within 1 mile of", isolate(place))) %>% 
              hc_subtitle(text = unique(risk$date)) %>% 
              hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>% 
              hc_yAxis(title = list(text = "Incidents"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
              hc_legend(enabled = FALSE) %>% 
              hc_tooltip(pointFormat = "Incidents: <b>{point.y}</b>") %>% 
              hc_plotOptions(series = list(cursor = "default")) %>% 
              hc_add_theme(hc_theme_smpl()) %>% 
              hc_chart(backgroundColor = "transparent")
            
          })
          
        },
        error = function(e) {
          showModal(modalDialog(title = "Sorry!", 
                                tags$p("We couldn't find any data for that location."),
                                tags$p("Give another one a try!")))
        }
        )
        
        incProgress(1/5)
        
      })
  })
  
  
}
shinyApp(ui, server)