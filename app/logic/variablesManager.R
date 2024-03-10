box::use(
  R6[R6Class],
  shiny[reactiveValues, reactiveVal]
)

#' @export
Variables <- R6::R6Class(
  classname = "Variables",
  public = list(
    scientificName = NULL,
    vernacularName= NULL,
    filters = reactiveValues(scientificName = NULL, ready = FALSE),
    markers =  reactiveValues(timeline = NULL),
    set_scientificName = function(scientificName) {
      self$filters$scientificName <- scientificName
    }
  )
)
