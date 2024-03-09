# app/view/select_species.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req, observe, reactive, 
        reactiveVal, observeEvent, 
        selectizeInput, updateSelectizeInput, reactiveValues, bindEvent],
  # shinyWidgets[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
                      column(width = 12, 
                             #pickerInput would have been a better to select All choices option but https://github.com/dreamRs/shinyWidgets/issues/460 
                             selectizeInput(inputId = ns("taxonRank"), label = "taxonRank",
                                                       width = '100%',
                                                       multiple = TRUE,
                                                         options = list(`actions-box` = TRUE,
                                                                        `deselect-all-text` = "None...",
                                                                        `select-all-text` = "Yeah, all !",
                                                                        `none-selected-text` = "zero"),
                                                         choices = c("All","species",
                                                                    "multispecies","subspecies",
                                                                    "synonym", "forma",
                                                                    "hybrid", "variety"),
                                                       selected = "species")
                             ),
                      column(width = 6, class = "specie-area",
                             selectizeInput(inputId = ns("scientificName"),
                                            label = "scientificName",
                                            choices = NULL, width = "100%",
                                            multiple  = TRUE,
                                            options = list(maxItems = 3))),
                      column(width = 6,class = "specie-area",
                             selectizeInput(inputId = ns("vernacularName"),
                                            label = "vernacularName",
                                            choices = NULL, width = "100%",
                                            multiple  = TRUE,
                                            options = list(maxItems = 3)))
    )
  )
}

#' @export
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$taxonRank, ignoreInit = TRUE, {
      req(input$taxonRank)
      data$loadDb(input$taxonRank)
    })
    
    observeEvent(data$species_choices$scientificName_choices_selectize,{
      updateSelectizeInput(session = session, inputId = "scientificName", choices = data$species_choices$scientificName_choices_selectize, server = TRUE)
      updateSelectizeInput(session = session, inputId = "vernacularName", choices = data$species_choices$vernacularName_choices_selectize, server = TRUE)
    })
    
    # keep track of whether we are allowed to update the inputs. Default both to TRUE
    allow_update <- reactiveValues(vernacularName = TRUE, scientificName = TRUE, time = 10)
    
    # create reactives that will fire when the input's change. They will return
    # * NULL, if this vernacularName cannot be updated currently
    # * The current system time otherwise (a monotonically increasing value)
    update_vernacularName <- reactive(if (allow_update$vernacularName) Sys.time()) |>
      bindEvent(input$vernacularName)
    
    update_scientificName <- reactive(if (allow_update$scientificName) Sys.time()) |>
      bindEvent(input$scientificName)
    
    # observe the event of these update_*() reactives changing. We need to make sure to not bind to the event when on
    # initialisation, otherwise we will get stuck in a loop
    observe({
      allow_update$scientificName <- FALSE
      allow_update$time <- Sys.time()
      #   updateSelectizeInput(session, "scientificName", selected = data$species_names_match() %>% dplyr::filter(`vernacularName` == input$vernacularName)[,"scientificName"])
      updateSelectizeInput(session, inputId = "scientificName", choices = data$species_choices$scientificName_choices_selectize,
                           selected = data$species_choices$species_names_match[data$species_choices$species_names_match$id %in% input$vernacularName, "id"]
      )
      
    }) |>
      bindEvent(update_vernacularName(), ignoreInit = FALSE)
    
    observe({
      allow_update$vernacularName <- FALSE
      allow_update$time <- Sys.time()
      #updateSelectizeInput(session, "vernacularName", selected = data$species_names_match() %>% dplyr::filter(`scientificName` == input$scientificName)[,"vernacularName"])
      updateSelectizeInput(session, inputId = "vernacularName", choices = data$species_choices$vernacularName_choices_selectize,
                           selected = data$species_choices$species_names_match[data$species_choices$species_names_match$id %in% input$scientificName, "id"]
      )
      
      #input$scientificName)
    }) |>
      bindEvent(update_scientificName(), ignoreInit = FALSE)
    
    # in order to re-enabled the inputs, we want to wait a short period of time. create a monotonically increasing
    # reactive which is triggered by the allow_update reactiveValues
    last_update_trigger <- reactive(Sys.time()) |>
      bindEvent(allow_update$scientificName, allow_update$vernacularName) |>
      shiny::debounce(100)
    
    # when the last_update_trigger is fired, re-enable the inputs
    observe({
      if(as.numeric(Sys.time() - allow_update$time) > 2 ){
        allow_update$vernacularName <- TRUE
        allow_update$scientificName <- TRUE
      } else {
        Sys.sleep(1)
        allow_update$vernacularName <- TRUE
        allow_update$scientificName <- TRUE
      }
    }) |>
      bindEvent(last_update_trigger())
    
    
    selected_species <- reactiveVal(0)
    observeEvent(input$scientificName, ignoreInit = TRUE,{
      req(input$scientificName)
      print("update specie id (scientificaly based)")
      data$filtered_data$selected_species <- input$scientificName
      variables$set_scientificName(names(data$species_choices$scientificName_choices[data$species_choices$scientificName_choices %in% input$scientificName]))
    })
    observeEvent(input$vernacularName,ignoreInit = TRUE, {
      print("update specie id (vernacularaly based)")
      data$filtered_data$selected_species <- input$vernacularName
      
    })
    # observeEvent(data$filtered_data$selected_species, ignoreInit = TRUE,{
    #     req(data$filtered_data$selected_species)
    #     print("update select inputs")
    #       updateSelectizeInput(session = session, inputId = "scientificName",
    #                            choices = data$species_choices$scientificName_choices_selectize(),
    #                            selected = data$filtered_data$selected_species, server = TRUE)
    #       updateSelectizeInput(session = session, inputId = "vernacularName",
    #                            choices = data$species_choices$vernacularName_choices()_selectize(),
    #                            selected = data$filtered_data$selected_species, server = TRUE)
    # })
    
    observeEvent(variables$filters$scientificName, ignoreInit = TRUE,{
      req(variables$filters$scientificName)
      data$filterbyscientificName(variables$filters$scientificName)
    })    
    
  })
}