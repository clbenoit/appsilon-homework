# app/view/count_panel.R

box::use(
  shiny[h3, moduleServer, NS, tagList, fluidRow,
        column, req, observe, renderUI, uiOutput, icon, renderPlot, plotOutput,
        reactive, reactiveVal, observeEvent, actionButton, img, div, br, HTML],
  bslib[value_box],
  bslib[card_header, card, card_body],
)

## Import shiny modules
box::use(
  app/logic/plotingFunctions[plot_sum_data, create_sum_data],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("countPanel")),
    fluidRow(
    column(width = 6,
           card(height = "100%", full_screen = TRUE,
                card_header("Occurences per lifeStage"),
                card_body(
                  class = "p-0",
                  plotOutput(ns("lifeStage_sum_plot"))
                )
           )),
     column(width = 6,
                  card(height = "100%", full_screen = TRUE,
                       card_header("Occurences per country"),
                       card_body(
                         class = "p-0",
                         plotOutput(ns("country_sum_plot"))
                       )
                  ) 
            )
  ))
}

#' @export
server <- function(id, data, variables) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    taglist <- reactive({
      req(data$filtered_data$occurence_filtered)
      if (nrow(data$filtered_data$occurence_filtered) > 0) {
        return(
          tagList(
            fluidRow(
              column(width = 6,
                value_box(
                  title = "Vernacular Names",
                  value = paste(unique(data$filtered_data$occurence_filtered$vernacularName),
                                collapse = ", "),
                  showcase =icon("id-card"),
                  theme = "teal")
              ),
              column(width = 6,
                value_box(
                  title = "Total number of occurences",
                  value = sum(data$filtered_data$occurence_filtered$individualCount),
                  showcase = img(src = "sum-sign.svg", width = "75px", 
                                 height = "75px", 
                                 style = "filter: brightness(0) invert(1);"),
                  theme = "teal")       
              )            )
          )
        )
      } else {
        return(div(class = "empty-blue",
                   "0 observation currently passing the filters"))
      }
    })
    
    output$countPanel <- renderUI({
      if (0 %in% data$filtered_data$selected_species) {
          return(HTML("<div class ='empty-blue'>
                      <h4>No species selected</h4>
                      <p>Please select either a scientificName or a verncularName to start</p>
                      </div> ")
                )
      } else {
        return(taglist())
      }
    })
  
  country_sum_data <- reactive({
      req(data$filtered_data$occurence_filtered)
      if(nrow(data$filtered_data$occurence_filtered) > 0){
        create_sum_data(title = "My title", 
                        split_by = "country", 
                        count_by = "individualCount", 
                        data = data$filtered_data$occurence_filtered)
    
    }
  })
  output$country_sum_plot <- renderPlot(
    plot_sum_data(title = "My title", 
                  split_by = "country", 
                  count_by = "individualCount", 
                  data = country_sum_data())
    
  )
    
  lifeStage_sum_data <- reactive({
      req(data$filtered_data$occurence_filtered)
      if(nrow(data$filtered_data$occurence_filtered) > 0){
        create_sum_data(title = "My title", 
                        split_by = "lifeStage", 
                        count_by = "individualCount", 
                        data = data$filtered_data$occurence_filtered)
    
    }
  })
    
    output$lifeStage_sum_plot <- renderPlot(
      plot_sum_data(title = "My title", 
                    split_by = "lifeStage", 
                    count_by = "individualCount", 
                    data = lifeStage_sum_data())
    
    )
  })
}
