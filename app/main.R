#options(shiny.trace=TRUE)

## Import dependancies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput, shinyOptions, bindCache,bindEvent,
        observe, observeEvent ,reactive, req, fluidRow, p, icon, h2, sliderInput, column,
        tagList, reactiveVal, conditionalPanel],
  utils[head],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect,dbGetQuery],
  dplyr[`%>%`,filter,select],
  bslib[bs_theme,page_navbar,
        nav_item, nav_menu, nav_panel, nav_spacer,sidebar],
  shinydashboard[dashboardHeader,dashboardPage,dashboardBody,dashboardSidebar,
                 sidebarMenu,menuItem, box],
  shinyjs[useShinyjs, show, hide],
)
## Import shiny modules
box::use(
  app/view/render_table,
  app/view/leaflet,
  app/view/timeline,
  app/logic/variablesManager[Variables],
  app/logic/dataManager[DataManager],
)

link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()  # Initialize shinyjs
  bootstrapPage(
      page_navbar(id = ns("page_navbar"),
      sidebar = sidebar(id = ns("main_sidebar"),
                        #'sidebar text'),
          conditionalPanel(ns=ns,
            "input.page_navbar === 'Explore' || input.page_navbar === 'Count'",
            "Page 1 sidebar"
          ),
          conditionalPanel(ns=ns,
            "input.page_navbar === 'Contributors'",
            "Page 2 sidebar"
          ),
          conditionalPanel(ns=ns,
            "input.page_navbar === 'Test'",
            "Page 3 sidebar"
          )),
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
                # dashboardPage(
                #   #dashboardHeader(title = NULL),
                #   dashboardHeader(disable = TRUE),
                #   dashboardSidebar(disable = TRUE),
                #   # dashboardSidebar(sidebarMenu(
                  #   menuItem(tabName = "home", text = "Home", icon = icon("home")),
                  #   menuItem(tabName = "another", text = "Another Tab", icon = icon("heart"))
                  # )),
                  # dashboardBody(
                  #   fluidRow(
                      timeline$ui(ns("timeline")),
                      #timelinetest$ui(ns("timeline")),
                      leaflet$ui(ns("exploremap")),
                      render_table$ui(ns("occurence_filtered"))
                #     )
                #   )
                # )
              ),
      nav_panel(title = "Count",
                # dashboardPage(
                #   #dashboardHeader(title = NULL),
                #   dashboardHeader(disable = TRUE),
                #   dashboardSidebar(disable = TRUE),
                  # dashboardSidebar(sidebarMenu(
                  #   menuItem(tabName = "home", text = "Home", icon = icon("home")),
                  #   menuItem(tabName = "another", text = "Another Tab", icon = icon("heart"))
                  # )),
                  # dashboardBody(
                  #   fluidRow(
                  #   )
                  # )
      #      )
      ),
      nav_panel("Contributors", p("Third page content.")),
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

    Variables <- Variables$new()
    DataManager <- DataManager$new()
    DataManager$loadDb()

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

    updateSelectizeInput(session = session, inputId = "scientificName", choices = DataManager$scientificName_choices, server = TRUE)
    updateSelectizeInput(session = session, inputId = "vernacularName", choices = DataManager$vernacularName_choices, server = TRUE)
    selected_specie <- reactiveVal(0)
    observeEvent(input$scientificName, ignoreInit = TRUE,{
      req(input$scientificName)
      print("update specie id (scientificaly based)")
      selected_specie(input$scientificName)
      Variables$set_scientificName(names(DataManager$scientificName_choices[as.numeric(input$scientificName)]))
    })
    observeEvent(input$vernacularName,ignoreInit = TRUE, {
      print("update specie id (vernacularaly based)")
        selected_specie(input$vernacularName)
    })
    observeEvent(selected_specie(), ignoreInit = TRUE,{
        req(selected_specie())
        print("update select inputs")
          updateSelectizeInput(session = session, inputId = "scientificName",
                               choices = DataManager$scientificName_choices,
                               selected = selected_specie(), server = TRUE)
          updateSelectizeInput(session = session, inputId = "vernacularName",
                               choices = DataManager$vernacularName_choices,
                               selected = selected_specie(), server = TRUE)
      })

    observeEvent(Variables$filters$scientificName, ignoreInit = TRUE,{
      req(Variables$filters$scientificName)
      DataManager$filterbyscientificName(Variables$filters$scientificName)
    })

    # observe({
    #   print(head(DataManager$filtered_data$occurence_filtered))
    # })

    #timelinetest$server("timeline", data = DataManager$filtered_data)
    timeline$server("timeline", data = DataManager$filtered_data)
    render_table$server("occurence_filtered", data = DataManager$filtered_data)
    leaflet$server("exploremap", data = DataManager$filtered_data, session)

  })
}
