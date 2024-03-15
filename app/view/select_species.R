# app/view/select_species.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList,
        fluidRow, column, req, observe, reactive,
        reactiveVal, observeEvent, modalDialog,
        selectizeInput, updateSelectizeInput, reactiveValues, bindEvent],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
        #pickerInput would have been a better to select All choices option
        #but https://github.com/dreamRs/shinyWidgets/issues/460
        selectizeInput(inputId = ns("taxonRank"), label = "taxonRank",
                       width = "100%", multiple = TRUE,
                       choices = c("species" = 1, "multispecies" = 2,
                                   "subspecies" = 3, "synonym" = 4,
                                   "forma" = 5, "hybrid" = 6, "variety" = 7),
                       selected = 1),
      ),
      column(width = 6,
        selectizeInput(inputId = ns("kingdom"), label = "kingdom",
                       width = "100%",
                       multiple = FALSE,
                       choices = c("Animalia", "Plantae", "Fungi"),
                       selected = c("Animalia"))
      ),
      column(width = 12,
             class = "specie-area",
             selectizeInput(inputId = ns("scientificName"),
                            label = "scientificName",
                            choices = NULL, width = "100%",
                            multiple  = TRUE,
                            options = list(maxItems = 3, persist = FALSE)))
    )
  )
}

#' @export
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {

    observeEvent(c(input$taxonRank, input$kingdom), ignoreInit = TRUE, {
      req(input$taxonRank); req(input$kingdom)
      print("loadingDB")
      data$loadDb(input$taxonRank, input$kingdom)
    })

    observeEvent(data$species_choices$scientificName_choices_selectize, {
      updateSelectizeInput(session = session, inputId = "scientificName",
                           choices = data$species_choices$scientificName_choices_selectize,
                           server = TRUE)
    })

    selected_species <- reactiveVal(0)
    observeEvent(input$scientificName, ignoreInit = TRUE, ignoreNULL = FALSE, {
      req(input$kingdom)
      if (!is.null(input$scientificName)) {
        print("update specie id")
        data$filtered_data$selected_species <- input$scientificName
        data$filterbyscientificName(names(data$species_choices$scientificName_choices[data$species_choices$scientificName_choices %in%
                                                                                        input$scientificName]),
                                    input$kingdom)
      } else {
        data$filtered_data$selected_species <- 0
      }
    })
    
  })
}
