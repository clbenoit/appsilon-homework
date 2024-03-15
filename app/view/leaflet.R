# app/view/leaflet.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow,
        column, observe, req, icon, withProgress,
        uiOutput, renderUI, img, observeEvent, reactiveVal,
        div, reactive, bindCache],
  leaflet[makeIcon, leafletOutput, renderLeaflet, addTiles,
          setView, addMarkers, leaflet, iconList],
  dplyr[`%>%`, filter, mutate, case_when],
  utils[head],
  bslib[card_header, card, card_body]
)
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
        card(height = "100%", full_screen = TRUE, card_header("Locations"),
          card_body(
            class = "p-0",
            uiOutput(ns("leafletMap"))
            #leaflet::leafletOutput(ns("leafletMap"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, data, session) {
  moduleServer(id, function(input, output, session) {

    icons <- iconList(blue = makeIcon("www/location-dot-blue.svg",
                                      iconWidth = 24, iconHeight = 32),
                      yellow = makeIcon("www/location-dot-yellow.svg",
                                        iconWidth = 24, iconHeight = 32),
                      green = makeIcon("www/location-dot-green.svg",
                                       iconWidth = 24, iconHeight = 32),
                      red = makeIcon("www/location-dot-red.svg",
                                     iconWidth = 24, iconHeight = 32))

    leaflet_occurence <- reactive({
      req(data$occurence_filtered)
      if (nrow(data$occurence_filtered > 0)) {
        scientificNames <- unique(data$occurence_filtered$scientificName)
        leaflet_occurence_tmp <- data$occurence_filtered
        leaflet_occurence_tmp$color <- "blue"
        if (length(scientificNames) == 2) {
          leaflet_occurence_tmp <- leaflet_occurence_tmp %>%
            mutate(color = case_when(
              scientificName == scientificNames[1]  ~ "blue",
              scientificName == scientificNames[2]  ~ "green"
            ))
        } else if (length(scientificNames) == 3){
          leaflet_occurence_tmp <- leaflet_occurence_tmp %>%
            mutate(color = case_when(
              scientificName == scientificNames[1]  ~ "blue",
              scientificName == scientificNames[2]  ~ "green",
              scientificName == scientificNames[3]  ~ "yellow"
            ))
        }
        return(leaflet_occurence_tmp)
      } else {
        return(data.frame())
      }
    }) %>% 
      bindCache(list(data$occurence_filtered))

    #observe({
      output$leafletMap <- renderUI({
      #output$leafletMap <- renderLeaflet({
        req(leaflet_occurence())
        if (nrow(leaflet_occurence()) == 0 ) {
          return(div(class = "empty-blue",
                     "0 observation currently passing the filters"))
        } else {
          withProgress(message = "Rendering maps", value = 0, {
          leaflet(data =  leaflet_occurence()) %>%
            addTiles() %>%
            addMarkers(
              ~longitudeDecimal, ~latitudeDecimal,
              icon = ~icons[color], popup = ~individualCount,
              label = ~individualCount, layerId = ~id
            )
          })
        }
      }) %>% 
        bindCache(leaflet_occurence())

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
