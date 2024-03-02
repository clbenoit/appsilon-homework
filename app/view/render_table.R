# app/view/render_table.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Table"),
    fluidRow(column(width = 12,
                    reactable$reactableOutput(ns("table"))
    ))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- reactable$renderReactable(
      reactable$reactable(data)
    )
  })
}