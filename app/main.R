#options(shiny.trace=TRUE)

## Import dependancies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput, shinyOptions, bindCache,
        observe,reactive, req ],
  utils[head],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect],
  dplyr[`%>%`,filter],
)
## Import shiny modules
box::use(
  app/view/render_table,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    selectizeInput(inputId = ns("specie"),label = "Specie", choices = NULL, width = "100%",
                   multiple  = TRUE),
    uiOutput(ns("message")),
    render_table$ui(ns("occurence_filtered"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## LOAD APP PARAMETERS ##
    Sys.setenv(R_CONFIG_ACTIVE = "devel")
    config <- config::get()
    
    if (config::get("cache_directory") == "tempdir"){
      tempdir <- tempdir()
      dir.create(file.path(tempdir,"cache"))
      print(paste0("using following cache directory : ", file.path(tempdir,"cache")))
      shinyOptions(cache = cachem::cache_disk(file.path(tempdir,"cache")))
    } else {
      print(paste0("using following cache directory : ", 
                   config::get("cache_directory")))
      shinyOptions(cache = cachem::cache_disk(config::get("cache_directory")))
    }
    
    ## UPLOAD SQLITE DATABASE ##
    con <- dbConnect(SQLite(), config$db_path)
    occurence <- dbReadTable(con,"occurence")
    multimedia <- dbReadTable(con,"multimedia")
    
    updateSelectizeInput(session = session, inputId = "specie", choices = unique(occurence$scientificName), server = TRUE)
    
    specie_occurence <- reactive({
      req(input$specie)
      req(occurence)
      print(head(input$specie))
      occurence %>%
        filter(scientificName %in% input$specie)
    }) # %>% bindCache(list(input$specie,occurence))
    
    render_table$server("occurence_filtered", data = specie_occurence())
    
    observe({
      req(specie_occurence())
      print(head(specie_occurence()))
    })

  })
}
