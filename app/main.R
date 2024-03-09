#options(shiny.trace=TRUE)

## Import dependancies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput, shinyOptions, bindCache,bindEvent,
        observe, observeEvent ,reactive, req, fluidRow, p, icon, h2, sliderInput, column,
        tagList, reactiveVal, conditionalPanel, renderText, HTML, reactiveValues],
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
  app/view/select_species,
  app/view/main_sidebar,
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
      header = select_species$ui(ns("select_species")),
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
    DataManager$loadDb("All")
    
    select_species$server("select_species", data= DataManager, variables = Variables)
    explore_panel$server("explorepanel", data = DataManager, variables = Variables)

  })
}
