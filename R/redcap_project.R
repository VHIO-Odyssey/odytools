# Helper to get the name of the project
get_project_name <- function() {
  list.files(here::here(), ".Rproj$") |>
    stringr::str_remove(".Rproj$")
}

# Helper to extract the import date of a redcap_data object.
get_import_date <- function(redcap_data) {

  stringr::str_extract(
    attr(redcap_data, "import_date"), "....-..-.. ..:.."
  ) |>
    stringr::str_remove_all("-|:") |>
    stringr::str_replace(" ", "_")
}

# Helper function to set the directories structure of a RedCap project
rc_init_dirs_files <- function() {

  project_name <- get_project_name()

  # Directories
  dir.create(here::here("data", "imports"), recursive = TRUE)
  dir.create(here::here("data", "extra"))
  dir.create(here::here("datasets"))
  dir.create(here::here("docs"))

  # Templates
  file.copy(
    system.file(
      "redcap_templates", "Rprofile_template.R", package = "odytools"
    ),
    here::here(".Rprofile")
  )
  file.copy(
    system.file(
      "redcap_templates", "dependencies_template.R", package = "odytools"
    ),
    here::here(stringr::str_c(project_name, "_dependencies.R"))
  )
  file.copy(
    system.file(
      "redcap_templates", "datasets_template.R", package = "odytools"
    ),
    here::here("datasets", stringr::str_c(project_name, "_datasets.R"))
  )


}

# Helper function to download and store a RedCap Project
rc_store_data <- function(token, url) {

  project_name <- get_project_name()

  redcap_data <- odytools::ody_rc_import(token, url)

  import_date <- get_import_date(redcap_data)

  save(
    redcap_data,
    file = here::here(
      "data", "imports",
      stringr::str_c(project_name, "_import_", import_date, ".RData")
    )
  )

  redcap_data

}

# Helper function to store the datasets in an RData in ./datasets
rc_store_datasets <- function(redcap_data) {

  project_name <- get_project_name()

  import_date <- get_import_date(redcap_data)

  datasets_file <- list.files(here::here("datasets"), "_datasets.R")

  source(here::here("datasets", datasets_file), local = rlang::current_env())

  save(
    "datasets",
    file = here::here(
      "datasets",
      stringr::str_c(project_name, "_datasets_", import_date, ".RData")
    )
  )

  get("datasets")

}


#' Start/Update a RedCap Project
#'
#' @param token Project token. If not provided, a dialog promp will ask for it.
#' @param url URL of the RedCap server (VHIO server by default).
#'
#' @export
ody_rc_init_update <- function(token = NULL,
                               url = "https://redcap.vhio.net/redcap/api/") {

  if (length(get_project_name()) == 0) stop("No RStudio project detected.")

  imports <- list.files(here::here("data", "imports"), ".RData$")
  # If no imports, it is assumed the project must be started.
  if (length(imports) == 0) {
    print("Starting Project")
    rc_init_dirs_files()
  } else {
    print("Updating Data")
  }

  redcap_data <- rc_store_data(token, url)
  datasets <- rc_store_datasets(redcap_data)
  project_name <- get_project_name()

  save(
    redcap_data, datasets,
    file = here::here(stringr::str_c(project_name, ".RData"))
  )

}
