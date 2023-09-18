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


# get_last_rdata <- function(dir) {
#
#
#
# }

# Helper function to set the directories structure of a RedCap project
rc_init_dirs <- function() {

  project_name <- get_project_name()

  # Directories
  dir.create(here::here("data", "imports"), recursive = TRUE)
  dir.create(here::here("data", "extra"))
  dir.create(here::here("datasets"))
  dir.create(here::here("docs"))

  # Datasets template
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

# Helper function to store the datasets in an RData in ./datsets
rc_run_datasets <- function(redcap_data) {

  project_name <- get_project_name()

  import_date <- get_import_date(redcap_data)

  datasets_file <- list.files(here::here("datasets"), "_datasets.R")

  source(here::here("datasets", datasets_file))

  save(
    get("datasets"),
    file = here::here(
      "datasets",
      stringr::str_c(project_name, "_datasets_", import_date, ".RData")
    )
  )

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

  rc_init_dirs()

  redcap_data <- rc_store_data(token, url)

  rc_run_datasets(redcap_data)

}
