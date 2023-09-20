#' @importFrom utils View

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
  dir.create(here::here("analysis"))
  dir.create(here::here("quality", "verification"), recursive = TRUE)
  dir.create(here::here("quality", "completeness"))
  dir.create(here::here("quality", "cra_tables"))

  # Root Templates
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
      "redcap_templates", "sandbox_template.R", package = "odytools"
    ),
    here::here(stringr::str_c(project_name, "_sandbox.R"))
  )

  #Datasets Template
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

  redcap_data <- ody_rc_import(token, url)

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

  datasets_file <- list.files(here::here("datasets"), "datasets.R$")

  source(here::here("datasets", datasets_file), local = rlang::current_env())

  datasets <- get("datasets")

  attr(datasets, "import_date") <- attr(redcap_data, "import_date")
  attr(datasets, "project_title") <- attr(
    redcap_data, "project_info"
  )$project_title

  save(
    "datasets",
    file = here::here(
      datasets,
      stringr::str_c(project_name, "_datasets_", import_date, ".RData")
    )
  )

  datasets

}


# Start/Update a RedCap Project. Only Addin
rc_init_update <- function(token = NULL,
                           url = "https://redcap.vhio.net/redcap/api/") {

  if (length(get_project_name()) == 0) stop("No RStudio project detected.")


  rc_init_dirs_files()

  redcap_data <- rc_store_data(token, url)
  datasets <- rc_store_datasets(redcap_data)
  project_name <- get_project_name()

  save(
    redcap_data, datasets,
    file = here::here(stringr::str_c(project_name, ".RData"))
  )

  load(
    here::here(stringr::str_c(project_name, ".RData")),
    envir = .GlobalEnv
  )

  cat("Project successfully downloaded.")

}

# Refresh the Datasets List. Only Addin
rc_refresh_datasets <- function() {

  load(list.files(here::here(), ".RData$"))

  redcap_data <- get("redcap_data")
  datasets <- rc_store_datasets(redcap_data)
  project_name <- get_project_name()


  save(
    redcap_data, datasets,
    file = here::here(stringr::str_c(project_name, ".RData"))
  )

  load(
    here::here(stringr::str_c(project_name, ".RData")),
    envir = .GlobalEnv
  )

  cat("Datasets successfully refreshed.")

}


#' Get the name and current import of the project
#'
#' @param as_list If FALSE, the information is printed on the console. Otherwise, it returns a list (usefull for reports)
#'
#' @details
#' "Where am I?" addin calls to this function
#'
#' @export
ody_rc_current <- function(as_list = FALSE) {

  load(list.files(here::here(), ".RData$"))

  if (!exists("redcap_data", inherits = FALSE)) {
    cat("No Redcap project detected. \nYou can set up one by clicking on Addins/Odytools/Start|Update Redcap project.")
  }

  import_date <- attr(get("redcap_data"), "import_date") |>
    stringr::str_extract("....-..-.. ..:..")
  project_name <- attr(get("redcap_data"), "project_info")$project_title

  if (as_list) {

    list(project = project_name, date = import_date)

  } else {

    stringr::str_c(
      "Project: ", project_name, "\nCurrent import: ", import_date, "\n"
    ) |>
      cat()

  }

}

# View the metadata of the current project. Only Addin
rc_view_metadata <- function() {

  load(list.files(here::here(), ".RData$"))

  attr(get("redcap_data"), "metadata") |> View()

}
