#options(shiny.trace=TRUE)

## Import dependancies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput, shinyOptions, bindCache,bindEvent,
        observe, observeEvent ,reactive, req, fluidRow, p, icon, h2, sliderInput, column,
        tagList, reactiveVal],
  utils[head],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect,dbGetQuery],
  dplyr[`%>%`,filter,select],
  bslib[bs_theme,page_navbar,
        nav_item, nav_menu, nav_panel, nav_spacer],
  shinydashboard[dashboardHeader,dashboardPage,dashboardBody,dashboardSidebar,
                 sidebarMenu,menuItem, box]
)
## Import shiny modules
box::use(
  app/view/render_table,
  app/view/leaflet,
)

link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
      page_navbar(
      title = "MyApp",
          header = fluidRow(class = "specie-area",
                      column(width = 6, class = "specie-area",
                             selectizeInput(inputId = ns("scientificName"),
                                            label = "scientificName",
                                            choices = NULL, width = "100%",
                                            multiple  = TRUE)),
                      column(width = 6,class = "specie-area",
                             selectizeInput(inputId = ns("vernacularName"),
                                            label = "vernacularName",
                                            choices = NULL, width = "100%",
                                            multiple  = TRUE))
                      ),
        nav_panel(title = "Explore", 
                dashboardPage(
                  dashboardHeader(title = NULL),
                  dashboardSidebar(sidebarMenu(
                    menuItem(tabName = "home", text = "Home", icon = icon("home")),
                    menuItem(tabName = "another", text = "Another Tab", icon = icon("heart"))
                  )),
                  dashboardBody(
                    fluidRow(
                      leaflet$ui(ns("exploremap")),
                      #render_table$ui(ns("occurence_filtered"))
                    )
                  )
                )
              ),
      nav_panel(title = "Count",
                dashboardPage(
                  dashboardHeader(title = NULL),
                  dashboardSidebar(sidebarMenu(
                    menuItem(tabName = "home", text = "Home", icon = icon("home")),
                    menuItem(tabName = "another", text = "Another Tab", icon = icon("heart"))
                  )),
                  dashboardBody(
                    fluidRow(
                    )
                  )
            )),
      nav_panel("Countributors", p("Third page content.")),
      nav_spacer(),
      nav_menu(
        title = "Links",
        align = "right",
        nav_item(link_posit)
      )
    )
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
    species_names_match <- dbGetQuery(con, "SELECT DISTINCT vernacularName, scientificName FROM occurence;")
    species_names_match$id <- seq(1:nrow(species_names_match))
    species_names_match[is.na(species_names_match[,1]),] <- species_names_match[is.na(species_names_match[,1]),2]
    species_names_match[is.na(species_names_match[,2]),] <- species_names_match[is.na(species_names_match[,2]),1]
    scientificName_choices <-  species_names_match$id
    names(scientificName_choices) <- species_names_match$scientificName
    vernacularName_choices <- species_names_match$id
    names(vernacularName_choices) <- species_names_match$vernacularName
    updateSelectizeInput(session = session, inputId = "scientificName", choices = scientificName_choices, server = TRUE)
    updateSelectizeInput(session = session, inputId = "vernacularName", choices = vernacularName_choices, server = TRUE)
    selected_specie <- reactiveVal(0)
    observeEvent(input$scientificName, ignoreInit = TRUE,{
      req(input$scientificName)
      print("update specie id (scientificaly based)")
      selected_specie(input$scientificName)
    })
    observeEvent(input$vernacularName,ignoreInit = TRUE, {
      print("update specie id (vernacularaly based)")
        selected_specie(input$vernacularName)
    })
    observeEvent(selected_specie(), ignoreInit = TRUE,{                    
        req(selected_specie())
        print("update select inputs")
          updateSelectizeInput(session = session, inputId = "scientificName", 
                               choices = scientificName_choices,
                               selected = selected_specie(), server = TRUE)
          updateSelectizeInput(session = session, inputId = "vernacularName",
                               choices = vernacularName_choices,
                               selected = selected_specie(), server = TRUE)
      })

    occurence_filtered <- reactive({
      req(input$scientificName)
      req(occurence)
      print("filtering occurences : ")
      occurence_filtered <-occurence %>% filter(scientificName %in% names(scientificName_choices[as.numeric(input$scientificName)]))
      print(paste0(nrow(occurence_filtered)," kept after filtering"))
      return(occurence_filtered)
    }) %>% bindEvent(selected_specie()) # %>% bindCache(list(input$scientificName,occurence))
    
    render_table$server("occurence_filtered", data = occurence_filtered)
    leaflet$server("exploremap", data = occurence_filtered, session)
  

  })
}
