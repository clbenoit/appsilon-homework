box::use(
  R6[R6Class],
  shiny[reactiveValues]
)

#' @export
Variables <- R6::R6Class(
  classname = "Variables",
  public = list(
    scientificName = NULL,
    vernacularName= NULL,
    #taxonRank = ractiveVal("All"),
    #set_speciedID = NULL,
    filters = reactiveValues(vernacularName = NULL),
    markers =  reactiveValues(timeline = NULL),
    # set_vernacularName = function(vernacularName) {
    #   self$filters$vernacularName <- vernacularName
    # },
    set_scientificName = function(scientificName) {
      self$filters$scientificName <- scientificName
    }#,
    # set_speciesID = function(speciesID) {
    #   self$filters$speciesID <- speciesID
    # }
  )
)
