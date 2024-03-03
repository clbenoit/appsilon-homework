box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect,dbGetQuery],
  dplyr[`%>%`, filter],
  utils[head],
)

#' @export
DataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    species_names_match = NULL,
    scientificName_choices = NULL,
    vernacularName_choices = NULL,
    occurence = NULL,
    multimedia = NULL,
    filtered_data = reactiveValues(occurence_specie = NULL,
                                   occurence_specie_continent = NULL,
                                   occurence_filtered = NULL,
                                   multimedia_filtered = NULL),
    filterbyscientificName = function(scientificNameFilter) {
      observeEvent(scientificNameFilter,{
        self$filtered_data$occurence_filtered <- self$occurence %>%
          filter(scientificName %in% scientificNameFilter)
        print("final filtered")
        print(nrow(self$filtered_data$occurence_filtered))
      })
    },
    # filterbycontinent = function(continentFilter) {
    #   observeEvent(continentFilter,{
    #     # self$filtered_data$occurence_specie_continent <- self$filtered_data$occurence_specie %>%
    #     #   filter(continent %in% continentFilter)
    #     self$filtered_data$occurence_filtered<- self$filtered_data$occurence_specie %>%
    #       filter(continent %in% continentFilter)
    #   })
    #},
    loadDb = function() {
      print("inside load DB")
      Sys.setenv(R_CONFIG_ACTIVE = "devel") # rundant with server side for now
      config <- config::get() # rundant with server side for now
      con <- dbConnect(SQLite(), config$db_path)
      occurence <- dbReadTable(con,"occurence")
      multimedia <- dbReadTable(con,"multimedia")
      species_names_match <- dbGetQuery(con, "SELECT DISTINCT vernacularName, scientificName FROM occurence;")
      species_names_match$id <- seq(1:nrow(species_names_match))
      species_names_match[is.na(species_names_match[,1]),] <- species_names_match[is.na(species_names_match[,1]),2]
      species_names_match[is.na(species_names_match[,2]),] <- species_names_match[is.na(species_names_match[,2]),1]
      scientificName_choices <-  species_names_match$id
      names(scientificName_choices) <- species_names_match$scientificName
      vernacularName_choices <- species_names_match$id
      names(vernacularName_choices) <- species_names_match$vernacularName

      self$scientificName_choices <- scientificName_choices
      self$vernacularName_choices <- vernacularName_choices
      self$species_names_match <- species_names_match
      self$occurence <- occurence
      self$multimedia <- multimedia
      }
    )
)
