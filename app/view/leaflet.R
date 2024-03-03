# app/view/leaflet.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column,observe,req,icon,
        uiOutput,renderUI,img,observeEvent],
  leaflet[makeIcon,leafletOutput,renderLeaflet,addTiles,setView,addProviderTiles,addMarkers,leaflet],
  dplyr[`%>%`,filter],
  utils[head],
)
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(width = 12,
            leafletOutput(outputId = ns("leafletMap")),
            uiOutput(ns("imageOutput"))
    ))
  )
}

#' @export
server <- function(id, data, session) {
  moduleServer(id, function(input, output, session) {

    # data <- data.frame(
    #   id = c(1, 2, 3),
    #   latitudeDecimal = c(51.505, 51.51, 51.52),
    #   longitudeDecimal = c(-0.09, -0.1, -0.12),
    #   individualCount = c("Marker 1", "Marker 2", "Marker 3")
    # )
    observe({
      output$leafletMap <- renderLeaflet({
        req(data$occurence_filtered)
        #leaflet(data = data) %>%
        leaflet(data = data$occurence_filtered) %>%
          addTiles() %>%
          addMarkers(
            ~longitudeDecimal, ~latitudeDecimal,
            icon = icon("location-dot"), popup = ~individualCount, label = ~individualCount,
            layerId = ~id
          )
      })
    })

    ##### THIS WORKS ONLY WHEN THE DATA PROVIDED TO LEAFLET IS NOT REACTIVE I DON'T GET WHY ####
    observeEvent(input$leafletMap_marker_click, {
      print("triggermarker")
      # Get the information about the clicked marker
      click_info <- input$leafletMap_marker_click
      print(click_info)

      # Check if a marker was clicked
      if (!is.null(click_info)) {
        # Extract the id of the clicked marker
        clicked_id <- data$id[data$longitudeDecimal == click_info$lng & data$latitudeDecimal == click_info$lat]

        # Print the id to the console (you can use it as needed)
        print(clicked_id)
      }
    })
  })
}
