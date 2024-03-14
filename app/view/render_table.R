# app/view/render_table.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req, observe],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(width = 12,
      reactable$reactableOutput(ns("table"))
    ))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(data$occurence_filtered)
      output$table <- reactable$renderReactable(
        reactable$reactable(data$occurence_filtered)
      )
    })
  })
}
