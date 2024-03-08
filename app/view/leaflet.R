# app/view/leaflet.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column,observe,req,icon, withProgress,
        uiOutput,renderUI,img,observeEvent, reactiveVal],
  leaflet[makeIcon,leafletOutput,renderLeaflet,addTiles,setView,addProviderTiles,addMarkers,leaflet],
  dplyr[`%>%`,filter, mutate, case_when],
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
    icons <- leaflet::iconList(blue = leaflet::makeIcon("www/location-dot-blue.svg", iconWidth = 24, iconHeight =32),
                      yellow = leaflet::makeIcon("www/location-dot-yellow.svg", iconWidth = 24, iconHeight =32),
                      green = leaflet::makeIcon("www/location-dot-green.svg", iconWidth = 24, iconHeight =32),
                      red = leaflet::makeIcon("www/location-dot-red.svg", iconWidth = 24, iconHeight =32))

    leaflet_occurence <- reactiveVal()
    observe({
      req(data$occurence_filtered)
      scientificNames <- unique(data$occurence_filtered$scientificName)
      leaflet_occurence_tmp <- data$occurence_filtered
      leaflet_occurence_tmp$color <- "blue"
      if(length(scientificNames) == 2) {
        print("length(scientificNames) : ")
        print(length(scientificNames))
        leaflet_occurence_tmp <- leaflet_occurence_tmp %>%
          mutate(color = case_when(
            scientificName == scientificNames[1]  ~ 'blue',
            scientificName == scientificNames[2]  ~ 'green'
          ))
      } else if(length(scientificNames) == 3){
        leaflet_occurence_tmp <- leaflet_occurence_tmp %>%
          mutate(color = case_when(
            scientificName == scientificNames[1]  ~ 'blue',
            scientificName == scientificNames[2]  ~ 'green',
            scientificName == scientificNames[3]  ~ 'yellow'
          ))
      }
      leaflet_occurence(leaflet_occurence_tmp)
    })

    #observe({
      output$leafletMap <- renderLeaflet({
        req(leaflet_occurence())

        withProgress(message = 'Rendering maps', value = 0, {
        leaflet(data =  leaflet_occurence()) %>%
          addTiles() %>%
          addMarkers(
            ~longitudeDecimal, ~latitudeDecimal,
            icon = ~icons[color], popup = ~individualCount, label = ~individualCount,
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
