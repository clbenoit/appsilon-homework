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
      #selectizeInput(inputId = ns("taxonRank", label = "taxonRank", multiple = TRUE, selected = NULL)
      )
    )

}

#' @export
server <- function(id, con, data, variables) {
  moduleServer(id, function(input, output, session) {
    
    #observeEvent({data$filtered_data$occurence_filtered,  
    
    # dbGetQuery(self$con,
    #            paste("SELECT * FROM occurence WHERE scientificName IN (",
    #                  paste0("'", paste(scientificNameFilter, collapse = "','"), "'"),
    #                  ");", sep = "")
    
   
   #  
   # updateSelectizeInput(session, "taxonRank", choices = unique(data$filtered_data$occurence_filtered$taxonRank), selected = )
  #})
    
  })
}
