# app/view/photo_viewer.R

box::use(
  shiny[h3, moduleServer, NS, tagList, fluidRow,
        column, req, observe, renderUI, uiOutput,
        reactive, reactiveVal, observeEvent,
        actionButton, img, div, br],
  bslib[card_header, card, card_body],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 12,
      card(height = "100%", full_screen = TRUE,
        card_header("Photo Viewer"),
        card_body(
          class = "p-0",
          uiOutput(ns("imageUI"))
        )
      )
    )
  )
}

#' @export
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    taglist <- reactive({
      if (is.null(variables$markers$timeline)) {
        return(div(class = "empty-blue",
                   "Select an observation in timeline to see its associated pictures")
        )
      } else {
        data$selectPhoto(variables$markers$timeline)
        if (length(data$multimedia$selected_photo) == 0) {
          return(div(class = "empty-blue",
                     "This observation does not have associated pictures"))
        } else if (length(data$multimedia$selected_photo) == 1) {
          return(
            tagList(
              img(src = data$multimedia$selected_photo[selected_picture()],
                  width = "100%", height = "80%"),
              br(), br(),
              div(class = "padding-left-right", paste0("Creator : ",
                                                       data$multimedia$creator[selected_picture()]))
            )
          )
        } else {
          return(
            tagList(
              img(src = data$multimedia$selected_photo[selected_picture()],
                  width = "100%", height = "80%"),
              br(), br(),
              div(class = "padding-left-right",
                div(paste0(" Creator : ",
                           data$multimedia$creator[selected_picture()])),
                br(),
                fluidRow(column(width = 6,
                                actionButton(ns("previousbutton"),
                                             "previous", width = "100%")),
                         column(width = 6,
                                actionButton(ns("nextbutton"),
                                             "next", width = "100%")))
              )
            )
          )
        }
      }
    })

    selected_picture <- reactiveVal(1)
    observeEvent(input$previousbutton, {
      if (selected_picture() == 1) {
        selected_picture(length(data$multimedia$selected_photo))
      } else {
        selected_picture(selected_picture() - 1 )
        print(selected_picture())
      }
    })
    observeEvent(input$nextbutton, {
      if (selected_picture() == length(data$multimedia$selected_photo)) {
        selected_picture(1)
      } else {
        selected_picture(selected_picture()  + 1 )
        print(selected_picture())
      }
    })

    output$imageUI  <- renderUI({
      req(taglist())
      tagList(
        taglist()
      )
    })
  })
}
