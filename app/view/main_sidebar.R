# app/view/main_sidebar.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req, observe, reactive, 
        reactiveVal, observeEvent, 
        selectizeInput, updateSelectizeInput, reactiveValues, bindEvent],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      selectizeInput(inputId = ns("lifeStage"), label = "lifeStage", multiple = TRUE, selected = NULL, choices = NULL)
      )
    )

}

#' @export
server <- function(id, con, data, variables) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data$filtered_data$occurence_filtered,{  
    lifeStage_values <- unique(data$filtered_data$occurence_filtered$lifeStage)
    updateSelectizeInput(session, "lifeStage", choices = lifeStage_values, selected = lifeStage_values, server = TRUE)
    })
    
  })
}
