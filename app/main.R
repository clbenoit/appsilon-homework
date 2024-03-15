## Import dependencies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS,
        renderUI, tags, uiOutput,
        selectizeInput, updateSelectizeInput,
        shinyOptions, bindCache, bindEvent,
        observe, observeEvent, req, fluidRow,
        p, icon, h2, column,
        tagList, conditionalPanel, HTML, a],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect, dbGetQuery],
  bslib[bs_theme, page_navbar,
        nav_item, nav_menu, nav_panel, nav_spacer, sidebar],
  shinyjs[useShinyjs],
  shiny.router[router_ui, router_server, route, route_link],
)
## Import shiny modules
box::use(
  app/view/select_species,
  app/view/main_sidebar,
  app/view/explore_panel,
  app/view/count_panel,
  app/logic/variablesManager[Variables],
  app/logic/dataManager[DataManager],
)

link_posit <- tags$a(
  icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)
link_doc <- tags$a(
  icon("book"), "Documentation",
  href = route_link("documentation")
)

link_appsilon <- tags$a(
  shiny::img(src = "appsilon.svg", style="height:40px"),
  href = "https://www.appsilon.com/",
  target = "_blank"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  bootstrapPage(
    div(style = "height:100%", router_ui(
      route("main",
        bootstrapPage(
          page_navbar(id = ns("page_navbar"),
                      theme = bs_theme(bootswatch = "lumen",
                                       bg = "#FCFDFD",
                                       fg = "rgb(25, 125, 85)"),
            sidebar = sidebar(id = ns("main_sidebar"), open = "desktop",
            title = "Filter observations",
            conditionalPanel(ns = ns,
              "input.page_navbar === 'Explore' || input.page_navbar === 'Count'",
              main_sidebar$ui(ns("main_sidebar"))
            ),
            conditionalPanel(ns = ns,
              "input.page_navbar === 'Contributors'",
              "Page 2 sidebar"
            ),
            conditionalPanel(ns = ns,
              "input.page_navbar === 'Test'",
              "Page 3 sidebar"
            )),
            title = "Biodata Discovery Board",
            header = select_species$ui(ns("select_species")),
            nav_panel(title = "Explore",
              explore_panel$ui(ns("explore_panel"))
            ),
            nav_panel(title = "Count", count_panel$ui(ns("count_panel"))),
            nav_panel("Contributors", p("Third page content.")),
            nav_spacer(),
            nav_menu(
              title = "Links", align = "right",
              nav_item(link_appsilon),
              nav_item(link_posit),
              nav_item(link_doc)
            ),
          )
        )
      ),
      route("documentation",
        bootstrapPage(
          tagList(
            div(class = "padding",
              a("Go back to the app", href = route_link("main"))
            ),
            div(class = "padding",
              shiny::includeMarkdown("app/static/md/documentation.md")
            )
          )
        )
      )
    )),
    footer = HTML(
                  '<footer>
                    <!-- SVG image with a clickable link -->
                     <a href="https://www.appsilon.com/" target="_blank">
                     <img src="appsilon.svg"></img>
                     </a>
                    <!-- Text on the right side of the footer -->
                    <span> Copyright <a href="https://observation-international.org" target="_blank">
                      Observation International</a> 2024
                    </span>
                   </footer>')
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
    DataManager$loadDb(1, "Animalia")

    select_species$server("select_species",
                          data = DataManager,
                          variables = Variables)
    main_sidebar$server("main_sidebar",
                        data = DataManager,
                        variables = Variables)
    count_panel$server("count_panel",
                         data = DataManager,
                         variables = Variables)
    explore_panel$server("explore_panel",
                        data = DataManager,
                        variables = Variables)

  })
}
