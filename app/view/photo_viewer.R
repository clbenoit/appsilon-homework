# app/view/photo_viewer.R

box::use(
  shiny[h3, moduleServer, NS, tagList, fluidRow, column, req, observe, renderUI, uiOutput,
        reactive, reactiveVal, observeEvent, actionButton,img, div, br],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("imageUI"))
  )
}

#' @export
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    taglist <- reactive({
      if(is.null(variables$markers$timeline)){
        print("is.null(variables$markers$timeline")
          return(tagList(div("Select an observation in timeline first"), br()))
      } else {
        data$selectPhoto(variables$markers$timeline)
        if(length(data$multimedia$selected_photo) == 0){
            return("This observation does not have associated pictures")
        } else {
            return(
              tagList(
                img(src=data$multimedia$selected_photo[selected_picture()], width = '100%', height = '80%'),
                div(paste0("Creator : ", data$multimedia$creator[selected_picture()]))
              )
            )
        }
      }
    })

    selected_picture <- reactiveVal(1)
    observeEvent(input$previousbutton,{
      if(selected_picture() == 1){
        selected_picture(length(data$multimedia$selected_photo))
      } else {
        selected_picture(selected_picture() - 1 )
        print(selected_picture())
      }
    })
    observeEvent(input$nextbutton ,{
      if(selected_picture() == length(data$multimedia$selected_photo)){
        selected_picture(1)
      } else {
        selected_picture(selected_picture()  + 1 )
        print(selected_picture())
      }
    })

    output$imageUI  <- renderUI({
      req(taglist())
      tagList(
        taglist(),
        fluidRow(column(width = 6, actionButton(ns('previousbutton'),"previous", width = "100%")),
        column(width = 6, actionButton(ns('nextbutton'),"next", width = "100%"))),
      )
    })

  })
}
