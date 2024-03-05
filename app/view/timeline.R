# app/view/timeline.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req, observe, observeEvent,
        reactive,
        HTML, renderUI, uiOutput],
  timevis[timevis, renderTimevis,timevisOutput],
  utils[head],
  shiny.info[display],
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
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {

    timeline_dataframes <- reactive({
      req(data$occurence_filtered)

      #data$occurence_filtered <- data$occurence_filtered[1:100, ]

      timeline_occurence <- data$occurence_filtered
      #save(file = "/home/ptngs/test.rda", "timeline_occurence")
      if(TRUE %in% unique(is.na(data$occurence_filtered$eventDate))){
          timeline_occurence <- data$occurence_filtered[!is.na(data$occurence_filtered$eventDate),]
          print("display")
          display("WARNING : Some occurence has been filtered out because they contains no date data", position = "top right", type = )
        } else {
          print("no display")
          timeline_occurence <- data$occurence_filtered
      }

      content_html <- c()
      content_html <- apply(timeline_occurence,1, function(observation){
        id <- paste0("ID = ", observation["id"])
        sex <- paste0("sex = ", observation["sex"])
        content_html <- c(content_html,paste(id,sex, sep = "<br/>", collapse = "br/"))
        return(content_html)
      })

      timeline_occurence <- data.frame(
        id = timeline_occurence$id,
        start = timeline_occurence$eventDate,
        end = rep(NA,nrow(timeline_occurence)),
        content = content_html,
        group_content = factor(timeline_occurence$scientificName)
      )

      timeline_occurence$group <- as.numeric(timeline_occurence$group_content)

      timeline_groups <- data.frame(
        id = unique(timeline_occurence$group),
        content = levels(timeline_occurence$group_content)
      )
      return(list(
        timeline_occurence,
        timeline_groups
      ))

    })

    # observe({
    #   print("WARNING GROUPS ARE FALSE")
    #   print(head(timeline_dataframes()[[1]]))
    #   print(head(timeline_dataframes()[[2]]))
    # })

    output$timevisui <- renderTimevis({
    #output$timeline <- renderUI({renderTimevis(
      timevis(timeline_dataframes()[[1]],
      #timevis(timeline_dataframes(),
              options = list(cluster = TRUE,
                             cluster.maxItems = 3),
              groups = timeline_dataframes()[[2]]
              #groups = timeline_dataframes()
       )
    })

    observeEvent(input$timevisui_selected, {
      selected_marker <- tryCatch(as.numeric(input$timevisui_selected),
                                  warning = function(w){return(NULL)})
      variables$markers$timeline  <- selected_marker
    })

  })
}
