# app/view/timeline.R

box::use(
  reactable,
  shiny[h3, moduleServer, NS, tagList, fluidRow,
        column, req, observe, observeEvent,
        reactive, withProgress, reactiveVal, bindCache, div,
        HTML, renderUI, uiOutput],
  timevis[timevis, renderTimevis, timevisOutput],
  utils[head],
  dplyr[`%>%`],
  bslib[card_header, card, card_body],
  promises[future_promise, finally, `%...>%`]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
        card(height = "100%", full_screen = TRUE,
          collapsible = TRUE,
          card_header("Timeline"),
          card_body(
            class = "p-0",
            timevisOutput(ns("timevisui"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {

    timeline_occurence <- reactiveVal()
    observeEvent(data$occurence_filtered, {
      req(data$occurence_filtered)
      p <- shiny::Progress$new()
      p$set(value = NULL, message = "Computing timeline data...")
      occurence_filtered <- data$occurence_filtered
      future_promise({
        if (nrow(occurence_filtered) > 0) {
          timeline_occurence <- occurence_filtered
          if (TRUE %in% unique(is.na(occurence_filtered$eventDate))) {
            timeline_occurence <- occurence_filtered[!is.na(occurence_filtered$eventDate), ]
            display("WARNING : Some occurence has been filtered out because they contains no date data",
                    position = "top right", type = )
          } else {
            timeline_occurence <- occurence_filtered
          }

          content_html <- c()
          content_html <- apply(timeline_occurence, 1, function(observation) {
            id <- paste0("ID = ", observation["id"])
            sex <- paste0("sex = ", observation["sex"])
            vernacularName <- paste0("vernacularName = ", observation["vernacularName"])
            content_html <- c(content_html, paste(id, sex, vernacularName,
                                                  sep = "<br/>", collapse = "br/"))
            return(content_html)
          })

          timeline_occurence <- data.frame(
            id = timeline_occurence$id,
            start = timeline_occurence$eventDate,
            end = rep(NA, nrow(timeline_occurence)),
            content = content_html,
            group_content = factor(timeline_occurence$scientificName)
          )
          timeline_occurence$group <- as.numeric(timeline_occurence$group_content)
          timeline_occurence
        } else {
          timeline_occurence <- data.frame()
          timeline_occurence
        }
      }) %...>%  timeline_occurence() %>%
        finally(~p$close())
    })

    timeline_groups <- reactive({
      req(timeline_occurence())
      data.frame(
        id = unique(timeline_occurence()$group),
        content = levels(timeline_occurence()$group_content)
      )
    })

    #output$timevisui <- renderUI({
    output$timevisui <- renderTimevis({
      req(timeline_occurence())
      # if(nrow(timeline_occurence()) > 0){
        withProgress(message = "Rendering timeline data", value = 0, {
          timevis(timeline_occurence(),
            options = list(cluster = TRUE,
                           cluster.maxItems = 3),
            groups = timeline_groups()
          )
       })
       # } else {
       #   div(class = "empty-blue", "0 observation currently passing the filters")
       # }
    }) %>%
      bindCache(list(timeline_groups(), timeline_occurence()))

    observeEvent(data$occurence_filtered, {
      req(data$occurence_filtered)
      variables$markers$timeline <- NULL
    })

    observeEvent(input$timevisui_selected, {
      selected_marker <- tryCatch(as.numeric(input$timevisui_selected),
                                  warning = function(w) {
                                    return(NULL)
                                  })
      variables$markers$timeline  <- selected_marker
    })

  })
}
