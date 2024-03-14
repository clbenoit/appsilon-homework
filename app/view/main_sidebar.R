# app/view/main_sidebar.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow,
        column, req, observe, reactive,
        observeEvent,  bindCache, selectizeInput,
        updateSelectizeInput, checkboxGroupInput,
        icon, dateRangeInput,
        updateDateRangeInput, br],
  dplyr[`%>%`, filter]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      selectizeInput(inputId = ns("lifeStage"), label = "lifeStage",
                     multiple = TRUE, selected = NULL, choices = NULL), br(),
      checkboxGroupInput(ns("sex"), "sex", width = "100%", inline = TRUE,
                         choiceNames = list(icon("mars"), icon("venus"), icon("question")),
                         choiceValues = list("male", "female", "undetermined"),
                         selected = c("male", "female", "undetermined")), br(),
      dateRangeInput(ns("dateRange"), "Date Range",
                     start = as.Date("2004-01-01", "%Y-%m-%d"),
                     end = Sys.Date(), format = "yyyy-mm-dd"),
      selectizeInput(inputId = ns("continent"), label = "Continent",
                     multiple = TRUE, selected = NULL, choices = NULL)
    )
  )
}

#' @export
server <- function(id, con, data, variables) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data$filtered_data$occurence_specie, priority = 100, {
      req(data$filtered_data$occurence_specie)
      lifeStage_values <- unique(data$filtered_data$occurence_specie$lifeStage)
      updateSelectizeInput(session, "lifeStage", choices = lifeStage_values,
                           selected = lifeStage_values, server = FALSE)
      updateDateRangeInput(session, "dateRange",
                          min = min(data$filtered_data$occurence_specie$eventDate),
                          max = max(data$filtered_data$occurence_specie$eventDate),
                          start = min(data$filtered_data$occurence_specie$eventDate),
                          end = max(data$filtered_data$occurence_specie$eventDate)
                          )
      continent_values <- unique(data$filtered_data$occurence_specie$continent)
      updateSelectizeInput(session, "continent", choices = continent_values, selected = continent_values, server = FALSE)
      print("input filters updated")
    })
    
    occurence_specie_lifeStage <- reactive({
      req(data$filtered_data$occurence_specie)
      if (!is.null(input$lifeStage)) {
        return(data$filtered_data$occurence_specie %>%
                 filter(lifeStage %in% input$lifeStage))
      } else {
        return(data$filtered_data$occurence_specie[0, ])
      }
    }) %>% 
      bindCache(list(input$lifeStage,
                     data$filtered_data$occurence_specie))

    occurence_specie_lifeStage_continent <- reactive({
      req(occurence_specie_lifeStage())
      if (!is.null(input$continent)) {
        return(occurence_specie_lifeStage() %>%
                 filter(continent %in% input$continent))
      } else {
        return(occurence_specie_lifeStage()[0, ])
      }
    }) %>%
      bindCache(list(input$continent,
                     occurence_specie_lifeStage()))

    occurence_specie_lifeStage_continent_date <- reactive({
      req(occurence_specie_lifeStage_continent())
      if (!is.null(input$dateRange)) {
        return(occurence_specie_lifeStage_continent() %>%
                 filter(eventDate >= input$dateRange[1]) %>%
                 filter(eventDate <= input$dateRange[2]))
      } else {
        return(occurence_specie_lifeStage_continent())
      }
    }) %>%
      bindCache(list(input$dateRange,
                     occurence_specie_lifeStage_continent()))

    occurence_specie_lifeStage_continent_date_sex <- reactive({
      req(occurence_specie_lifeStage_continent_date())
      if (!is.null(input$sex)) {
        return(occurence_specie_lifeStage_continent_date() %>% 
                 filter(sex %in% input$sex))
      } else {
        return(occurence_specie_lifeStage_continent_date())
      }
    }) %>%
      bindCache(list(occurence_specie_lifeStage_continent_date(), input$sex))

    observeEvent(occurence_specie_lifeStage_continent_date_sex(), {
      data$filtered_data$occurence_filtered <- occurence_specie_lifeStage_continent_date_sex()
    })
  })
}
