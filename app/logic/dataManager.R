box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect, dbGetQuery],
  dplyr[`%>%`, filter],
  stats[setNames],
  shinybusy[remove_modal_spinner, show_modal_spinner]
)

#' @export
DataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    con = NULL,
    species_choices = reactiveValues(
      species_family_match = NULL,
      scientificName_choices = NULL,
      scientificName_choices_selectize = NULL),
    multimedia = reactiveValues(selected_photo = NULL, creator = NULL),
    filtered_data = reactiveValues(selected_species = 0,
                                   occurence_specie = NULL,
                                   occurence_filtered = NULL),
    filterbyscientificName = function(scientificNameFilter, kingdom) {
      observeEvent(scientificNameFilter, {
        show_modal_spinner(
          spin = "double-bounce",
          color = "#112446",
          text = "Extracting occurences from database")
        self$filtered_data$occurence_specie <- dbGetQuery(
          self$con,
          paste("SELECT * FROM occurence_",
                kingdom,
                " WHERE scientificName IN (",
                paste0("'", paste(scientificNameFilter, collapse = "','"), "'"),
                ");", sep = "")
        )
        remove_modal_spinner()
      })
    },
    selectPhoto = function(observation_id) {
      observeEvent(observation_id, {
        req(observation_id)
        query_result <- dbGetQuery(self$con,
          paste("SELECT accessURI, creator FROM multimedia WHERE id = ",
                observation_id,
                ";", sep = "")
        )
        self$multimedia$selected_photo <- query_result$accessURI
        self$multimedia$creator <- query_result$creator
      })
    },
    loadDb = function(taxonRank, kingdom, con) {
      print("inside load DB")
      self$con <- con
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#112446",
        text = "Loading database metadata")
      
      species_family_match <- dbGetQuery(
        con,
        paste("SELECT * FROM species_family_match_",
            kingdom,
            " WHERE taxonRank IN (",
            paste0("'", paste(taxonRank, collapse = "','"), "'"),
            ");", sep = "")
      )
      
      species_family_match$id <- seq_len(nrow(species_family_match))
      scientificName_choices <-  species_family_match$id
      names(scientificName_choices) <- species_family_match$scientificName
      self$species_choices$scientificName_choices <- scientificName_choices
      self$species_choices$species_family_match <- species_family_match

      species_family_match$family <- tolower(species_family_match$family)
      grouped_data <- split(species_family_match, species_family_match$family)
      scientificName_choices_selectize <- lapply(grouped_data, function(x) {
        return(setNames(x$id, x$scientificName))
      })
      self$species_choices$scientificName_choices_selectize <- scientificName_choices_selectize
      remove_modal_spinner()
    }
  )
)
