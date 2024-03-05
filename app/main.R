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
               uiOutput(ns("explorePanel"))
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

    Variables <- Variables$new()
    DataManager <- DataManager$new()
    DataManager$loadDb()

    updateSelectizeInput(session = session, inputId = "scientificName", choices = DataManager$scientificName_choices, server = TRUE)
    updateSelectizeInput(session = session, inputId = "vernacularName", choices = DataManager$vernacularName_choices, server = TRUE)
    selected_specie <- reactiveVal(0)
    observeEvent(input$scientificName, ignoreInit = TRUE,{
      req(input$scientificName)
      print("update specie id (scientificaly based)")
      selected_specie(input$scientificName)
      Variables$set_scientificName(names(DataManager$scientificName_choices[DataManager$scientificName_choices %in% input$scientificName]))
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

    observeEvent(selected_specie(),{
      if(selected_specie() == 0){
        output$explorePanel <- renderUI({
            HTML("<div style='text-align: center; padding: 20px;
                  background-color: #f8d7da;
                  color: #721c24; border: 1px solid #f5c6cb;
                  border-radius: 5px; margin: 20px;'>
                  <h4>Disclaimer:</h4>
                  <p>No data is currently available for display.</p>
                  <p>This could be due to various reasons, including insufficient data,
                     an error in data retrieval, or no relevant records found.</p>
                  <p>Please check back later or adjust your filters to explore the available data.</p>
                  </div> ")
        })
      } else {
        output$explorePanel <- renderUI({
          tagList(
                  timeline$ui(session$ns("timeline")),
                  uiOutput(session$ns("imageUI")),
                  leaflet$ui(session$ns("exploremap")),
                  render_table$ui(session$ns("occurence_filtered"))
              )
          })
      }
    })

    observeEvent(Variables$filters$scientificName, ignoreInit = TRUE,{
      req(Variables$filters$scientificName)
      DataManager$filterbyscientificName(Variables$filters$scientificName)
    })

    timeline$server("timeline", data = DataManager$filtered_data, variables = Variables)
    render_table$server("occurence_filtered", data = DataManager$filtered_data)
    leaflet$server("exploremap", data = DataManager$filtered_data, session)

    observe({
      if(is.null(Variables$markers$timeline)){
        output$imageUI <- renderUI({
          tagList("Select an observation in timeline first")
        })
      } else {
        DataManager$selectPhoto(Variables$markers$timeline)
        src <- DataManager$multimedia$selected_photo
        if(length(src) == 0){
          print(Variables$markers$timeline)
          output$imageUI <- renderUI({
            tagList("This obesvation does not have associated pictures")
          })
        } else {
          output$imageUI  <- renderUI({
            tags$img(src=src, width = 200, height = 100)
          })
        }
      }
    })

  })
}
