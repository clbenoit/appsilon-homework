#options(shiny.trace=TRUE)

## Import dependancies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput, shinyOptions, bindCache,bindEvent,
        observe, observeEvent ,reactive, req, fluidRow, p, icon, h2, sliderInput, column,
        tagList, reactiveVal, conditionalPanel, renderText, HTML],
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
  app/view/explore_panel,
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
                                            multiple  = TRUE,
                                            options = list(maxItems = 3))),
                      column(width = 6,class = "specie-area",
                             selectizeInput(inputId = ns("vernacularName"),
                                            label = "vernacularName",
                                            choices = NULL, width = "100%",
                                            multiple  = TRUE,
                                            options = list(maxItems = 3)))
                      ),
      nav_panel(title = "Explore",
               explore_panel$ui(ns("explorepanel"))
               ),
      nav_panel(title = "Count",
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

    future::plan("multisession")
    #future::plan("multicore")

    Variables <- Variables$new()
    DataManager <- DataManager$new()
    DataManager$loadDb()

    updateSelectizeInput(session = session, inputId = "scientificName", choices = DataManager$scientificName_choices, server = TRUE)
    updateSelectizeInput(session = session, inputId = "vernacularName", choices = DataManager$vernacularName_choices, server = TRUE)
    selected_species <- reactiveVal(0)
    observeEvent(input$scientificName, ignoreInit = TRUE,{
      req(input$scientificName)
      print("update specie id (scientificaly based)")
      DataManager$filtered_data$selected_species <- input$scientificName
      Variables$set_scientificName(names(DataManager$scientificName_choices[DataManager$scientificName_choices %in% input$scientificName]))
    })
    observeEvent(input$vernacularName,ignoreInit = TRUE, {
      print("update specie id (vernacularaly based)")
        DataManager$filtered_data$selected_species <- input$vernacularName

    })
    observeEvent(DataManager$filtered_data$selected_species, ignoreInit = TRUE,{
        req(DataManager$filtered_data$selected_species)
        print("update select inputs")
          updateSelectizeInput(session = session, inputId = "scientificName",
                               choices = DataManager$scientificName_choices,
                               selected = DataManager$filtered_data$selected_species, server = TRUE)
          updateSelectizeInput(session = session, inputId = "vernacularName",
                               choices = DataManager$vernacularName_choices,
                               selected = DataManager$filtered_data$selected_species, server = TRUE)
    })

    observeEvent(Variables$filters$scientificName, ignoreInit = TRUE,{
      req(Variables$filters$scientificName)
      DataManager$filterbyscientificName(Variables$filters$scientificName)
    })

    explore_panel$server("explorepanel", data = DataManager, variables = Variables)

  })
}
