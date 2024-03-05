box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect,dbGetQuery],
  dplyr[`%>%`, filter],
  utils[head],
)

#' @export
DataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    con = NULL,
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
      observeEvent(scientificNameFilter, {
        self$filtered_data$occurence_filtered <- dbGetQuery(self$con,
          paste("SELECT * FROM occurence WHERE id IN (",
            paste0("'", paste(scientificNameFilter, collapse = "','"), "'"),
            ");", sep = "")
          )
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
      self$con <- con

      if (config::get("cache_directory") == "tempdir"){
        tempdir <- tempdir()
        dir.create(file.path(tempdir,"cache"))
        print(paste0("using following cache directory : ", file.path(tempdir,"cache")))
        shinyOptions(cache = cachem::cache_disk(file.path(tempdir,"cache")))
      } else {
        print(paste0("using following cache directory : ",
                     config::get("cache_directory")))
        shinyOptions(cache = cachem::cache_disk(config::get("cache_directory")))
      }

      species_names_match <- dbGetQuery(con, "SELECT DISTINCT id, vernacularName, scientificName FROM occurence;")
      species_names_match[is.na(species_names_match[,2]),] <- species_names_match[is.na(species_names_match[,2]),3]
      species_names_match[is.na(species_names_match[,3]),] <- species_names_match[is.na(species_names_match[,3]),2]
      scientificName_choices <-  species_names_match$id
      names(scientificName_choices) <- species_names_match$scientificName
      vernacularName_choices <- species_names_match$id
      names(vernacularName_choices) <- species_names_match$vernacularName

      self$scientificName_choices <- scientificName_choices
      self$vernacularName_choices <- vernacularName_choices
      self$species_names_match <- species_names_match
      }
    )
)
