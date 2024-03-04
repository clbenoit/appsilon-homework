# app/view/timeline.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req, observe, observeEvent,
        reactive,
        HTML, renderUI, uiOutput],
  timevis[timevis, renderTimevis,timevisOutput],
  utils[head],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(width = 12,
                    timevisOutput(ns("timevisui"))
                    #uiOutput(ns("timeline"))
    ))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(data$occurence_filtered)
      print("inside timeline module")
      print(head(data$occurence_filtered))
    })

    timeline_dataframe <- reactive({
      req(data$occurence_filtered)

      data$occurence_filtered <- data$occurence_filtered[1:100, ]

      content_html <- c()
      content_html <- apply(data$occurence_filtered,1, function(observation){
        id <- paste0("ID = ", observation["id"])
        sex <- paste0("sex = ", observation["sex"])
        content_html <- c(content_html,paste(id,sex, sep = "<br/>", collapse = "br/"))
        return(content_html)
      })

      print(head(content_html))

      return(data.frame(
        id = data$occurence_filtered$id,
        start = data$occurence_filtered$eventDate,
        end = rep(NA,nrow(data$occurence_filtered)),
        content = content_html)
        )

    })

    ## HANDLE NA VALUES timevis: 'data' must contain a 'start' date for each item

    output$timevisui <- renderTimevis({
    #output$timeline <- renderUI({renderTimevis(
      timevis(timeline_dataframe())
     # )
    })

    observeEvent(input$timevisui_selected, {
      idString <- input$timevisui_selected
      print("selected")
      print(idString)
    })

  })
}
