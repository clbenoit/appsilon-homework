# app/view/explore_panel.R

box::use(
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req, observe, renderUI, uiOutput,
        reactive, reactiveVal, observeEvent, actionButton,img, div, br, HTML],
)

## Import shiny modules
box::use(
  #app/view/render_table,
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

    ns <- session$ns

    observeEvent(data$filtered_data$selected_species,{
      if(0 %in% data$filtered_data$selected_species){
        output$explorePanel <- renderUI({
          HTML("<div class ='empty-red'>
                  <h4>Disclaimer:</h4>
                  <p>No data is currently available for display.</p>
                  <p>This could be due to various reasons, including insufficient data,
                     an error in data retrieval, or no relevant records found.</p>
                  <p>Please check back later or adjust your filters to explore the available data.</p>
                  </div> ")
        })
      } else {
        output$explorePanel <- renderUI({
          tagList(
            timeline$ui(session$ns("timeline")),
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
            #render_table$ui(session$ns("occurence_filtered"))
            )
          })
        }
    })

    timeline$server("timeline", data = data$filtered_data, variables = variables)
    #render_table$server("occurence_filtered", data = data$filtered_data)
    leaflet$server("exploremap", data = data$filtered_data, session)
    photo_viewer$server("photos", data = data, variables = variables)

  })
}
