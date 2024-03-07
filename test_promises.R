# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("future", quietly = TRUE)) install.packages("future")
#if (!requireNamespace("future_promise", quietly = TRUE)) install.packages("future_promise")

library(shiny)
library(promises)
#library(future_promise)

# Set up a future plan (e.g., multiprocess plan)
plan("multisession")

ui <- fluidPage(
  titlePanel("Shiny App with future_promise"),

  mainPanel(
    actionButton("startButton", "Start Computation"),
    verbatimTextOutput("progressOutput")
  )
)

server <- function(input, output, session) {
  observeEvent(input$startButton, {
    # Function to perform computation in subprocess
    computeFunction <- function() {
      Sys.sleep(7)
    }

    # Create a progress bar
      # Start computation in a separate future_promise
      future_promise({
          promise_progress <- promise_progress$new()
          promise_progress$register(progressCallback)

          computeFunction()

          promise_progress$update(value = 1)
      })

  # Function to update progress
  progressCallback <- function(value) {
    incProgress(value)
  }
})
}

shinyApp(ui, server)
