#options(shiny.trace=TRUE)

## Import dependancies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput, shinyOptions, bindCache,
        observe,reactive, req, fluidRow, p, icon, h2, sliderInput, column,
        tagList],
  utils[head],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect],
  dplyr[`%>%`,filter],
  bslib[bs_theme,page_navbar,
        nav_item, nav_menu, nav_panel, nav_spacer],
  shinydashboard[dashboardHeader,dashboardPage,dashboardBody,dashboardSidebar,
                 sidebarMenu,menuItem, box]
)
## Import shiny modules
box::use(
  app/view/render_table,
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
    # fluidRow(
    #   selectizeInput(inputId = ns("specie"),label = "Specie", 
    #                  choices = NULL, width = "100%",
    #                  multiple  = TRUE),
    #   render_table$ui(ns("occurence_filtered"))
    # ),
      page_navbar(
      title = "MyApp",
          header = fluidRow(class = "specie-area",
                      column(width = 6,class = "specie-area",
                             selectizeInput(inputId = ns("specie"),label = "Specie",
                                choices = NULL, width = "100%",
                                multiple  = TRUE)),
                      column(width = 6,class = "specie-area",
                             selectizeInput(inputId = ns("specie2"),
                                            label = "Specie",
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
                      render_table$ui(ns("occurence_filtered"))
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
