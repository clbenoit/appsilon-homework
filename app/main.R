#options(shiny.trace=TRUE)

## Import dependancies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput, shinyOptions, bindCache,bindEvent,
        observe, observeEvent ,reactive, req, fluidRow, p, icon, h2, sliderInput, column,
        tagList, reactiveVal, conditionalPanel, renderText, HTML, reactiveValues, a],
  utils[head],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect,dbGetQuery],
  dplyr[`%>%`,filter,select],
  bslib[bs_theme,page_navbar,
        nav_item, nav_menu, nav_panel, nav_spacer,sidebar],
  shinydashboard[dashboardHeader,dashboardPage,dashboardBody,dashboardSidebar,
                 sidebarMenu,menuItem, box],
  shinyjs[useShinyjs, show, hide],
  shiny.router[router_ui, router_server, route, route_link], 
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
  icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)
link_doc <- tags$a(
    icon("book"),"Documentation", 
    href = route_link("documentation")
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()  # Initialize shinyjs
  bootstrapPage(
    router_ui(
      route("main",
      bootstrapPage(
      page_navbar(id = ns("page_navbar"),theme = bs_theme(bootswatch = "lumen", bg = "#FCFDFD", fg = "rgb(25, 125, 85)"),
      sidebar = sidebar(id = ns("main_sidebar"), open = "desktop",
          title = "Filter observations",
          conditionalPanel(ns=ns,
            "input.page_navbar === 'Explore' || input.page_navbar === 'Count'",
            main_sidebar$ui(ns("main_sidebar"))
          ),
          conditionalPanel(ns=ns,
            "input.page_navbar === 'Contributors'",
            "Page 2 sidebar"
          ),
          conditionalPanel(ns=ns,
            "input.page_navbar === 'Test'",
            "Page 3 sidebar"
          )),
      title = "Biodata Discovery Board",
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
        nav_item(link_posit),
        nav_item(link_doc)
      ),
    ),
    footer = fluidRow(column(width = 12, 
                             div(style = "text-align: right; padding: 1%;",
                              shiny::HTML("Copyright <a href='https://observation-international.org'
                                         target='_blank'>Observation International</a> 2024")
                             )
       ))
      )
    ), 
    route("documentation", 
               tagList(
                div(class ="padding",
                  a("Go back to the app", href = route_link("main"))
               ),
               div(class ="padding",
               shiny::includeMarkdown("inst/md/documentation.md"))
               ))
    )
  )
}

#' @export
server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    router_server("main")
    future::plan("multisession")
    #future::plan("multicore")

    Variables <- Variables$new()
    DataManager <- DataManager$new()
    DataManager$loadDb("species","Animalia")
    
    select_species$server("select_species", data= DataManager, variables = Variables)
    explore_panel$server("explorepanel", data = DataManager, variables = Variables)
    main_sidebar$server("main_sidebar", data = DataManager, variables = Variables)

  })
}
