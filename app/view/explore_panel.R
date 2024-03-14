# app/view/explore_panel.R

box::use(
  shiny[h3, moduleServer, NS, tagList, fluidRow,
        column, req, observe, renderUI, uiOutput,
        reactive, reactiveVal, observeEvent, actionButton, img, div, br, HTML],
)

## Import shiny modules
box::use(
  app/view/photo_viewer,
  app/view/leaflet,
  app/view/timeline,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("explorePanel"))
  )
}

#' @export
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data$filtered_data$selected_species, {
      if (0 %in% data$filtered_data$selected_species) {
        output$explorePanel <- renderUI({
          HTML("<div class ='empty-blue'>
                  <h4>No species selected</h4>
                  <p>Please select either a scientificName or a verncularName to start</p>
               </div> ")
        })
      } else {
        output$explorePanel <- renderUI({
          tagList(
            timeline$ui(session$ns("timeline")), br(),
            fluidRow(
              column(
                width = 7,
                fluidRow(photo_viewer$ui(session$ns("photos")))
              ),
              column(
                width = 5,
                leaflet$ui(session$ns("exploremap"))
              ),
            ),
          )
        })
      }
    })

    timeline$server("timeline",
                    data = data$filtered_data,
                    variables = variables)
    leaflet$server("exploremap",
                   data = data$filtered_data,
                   session)
    photo_viewer$server("photos",
                        data = data,
                        variables = variables)

  })
}
