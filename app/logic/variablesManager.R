box::use(
  R6[R6Class],
  shiny[reactiveValues, reactiveVal]
)

#' @export
Variables <- R6::R6Class(
  classname = "Variables",
  public = list(
    # scientificName = NULL,
    # vernacularName = NULL,
    markers =  reactiveValues(timeline = NULL)
  )
)
