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
  dir.create(here::here("data", "datasets"))
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
  file.copy(
    system.file(
      "redcap_templates", "functions_template.R", package = "odytools"
    ),
    here::here(stringr::str_c(project_name, "_functions.R"))
  )

  # Datasets Template
  file.copy(
    system.file(
      "redcap_templates", "datasets_template.R", package = "odytools"
    ),
    here::here("data", "datasets", stringr::str_c(project_name, "_datasets.R"))
  )

  # Report templates
  file.copy(
    system.file(
      "redcap_templates", "report_template.qmd", package = "odytools"
    ),
    here::here("analysis", stringr::str_c(project_name, "_analysis.qmd"))
  )
  file.copy(
    system.file(
      "redcap_templates", "report_template.qmd", package = "odytools"
    ),
    here::here(
      "quality", "verification", stringr::str_c(project_name, "_verification.qmd")
    )
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

  datasets_scripts <- list.files(here::here("data", "datasets"), ".R$")

  purrr::walk(
    here::here("data", "datasets", datasets_scripts),
    source, local = rlang::current_env()
  )

  current_objects <- ls()
  dataset_index <- purrr::map_lgl(
    current_objects, ~!is.null(attr(get(.), "is_dataset"))
  )
  to_datasets <- current_objects[dataset_index]
  datasets <- purrr::map(
    to_datasets,
    ~get(.)
  )
  names(datasets) <- to_datasets

  attr(datasets, "import_date") <- attr(redcap_data, "import_date")
  attr(datasets, "project_title") <- attr(
    redcap_data, "project_info"
  )$project_title

  save(
    datasets,
    file = here::here(
      "data", "datasets",
      stringr::str_c(project_name, "_datasets_", import_date, ".RData")
    )
  )

  datasets

}


# Start/Update a RedCap Project. Only Addin
rc_init_update <- function(token = NULL,
                           url = "https://redcap.vhio.net/redcap/api/") {

  if (length(get_project_name()) == 0) stop("No RStudio project detected.")
  project_name <- get_project_name()

  # It is an update?
  expected_rdata <- stringr::str_c(get_project_name(), ".RData")
  is_update <- any(expected_rdata == list.files(here::here()))

  if (is_update) {
    load(here::here(stringr::str_c(project_name, ".RData")))
    pre_update_project <- attr(get("redcap_data"), "project_info")$project_title
    rm(redcap_data, datasets)
  } else {
    rc_init_dirs_files()
  }

  redcap_data <- rc_store_data(token, url)

  if (is_update) {
    post_update_project <- attr(redcap_data, "project_info")$project_title
    if (pre_update_project != post_update_project) {
      stop(
        "The project associated with the token provided (",
        post_update_project, ") does not match the current project (",
        pre_update_project, "). Update canceled."
      )
    }
  }

  #Datasets must run after checking the downloaded project is the same as the
  #current project.

  message("Computing datasets...\n")
  datasets <- rc_store_datasets(redcap_data)

  save(
    redcap_data, datasets,
    file = here::here(stringr::str_c(project_name, ".RData"))
  )

  message("Project successfully downloaded.\n")

  source(here::here(stringr::str_c(project_name, "_dependencies.R")))

}

# Refresh the Datasets List. Only Addin
rc_refresh_datasets <- function() {

  message("Refreshing datasets...\n")

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

  message("Datasets successfully refreshed.\n")

}

#' Declare an object as belonging to datasets list
#'
#' @param object Object to add to datasets. Usually a data frame.
#' @param description Optional description of the object.
#'
#' @export
ody_add_to_datasets <- function(object, description = NULL) {

  attr(object, "is_dataset") <- TRUE
  attr(object, "description") <- description

  object

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

  rdatas <- list.files(here::here(), ".RData$")

  if (length(rdatas) != 1) {
    return(message("No Redcap project detected.\nYou can set up one by clicking on Addins/Odytools/Start|Update Redcap project.\n"))
  } else {

    load(here::here(list.files(here::here(), ".RData$")))

    if (!exists("redcap_data", inherits = FALSE)) {
      return(message("No Redcap project detected.\nYou can set up one by clicking on Addins/Odytools/Start|Update Redcap project.\n"))
    }
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
      message()

  }

}

# Helper function to copy a new analysis template.Onla addin
add_analysis_template <- function() {

  project_name <- get_project_name()

  n_analysis <- list.files(here::here("analysis")) |>
    stringr::str_detect(
      stringr::str_c(project_name, "_analysis_?\\d?\\.qmd")
    ) |>
    sum()

  if (n_analysis == 0) {
    analysis_name <- stringr::str_c(project_name, "_analysis.qmd")
  } else {
    analysis_name <- stringr::str_c(
      project_name, "_analysis_", n_analysis + 1, ".qmd"
    )
  }

  file.copy(
    system.file(
      "redcap_templates", "report_template.qmd", package = "odytools"
    ),
    here::here("analysis", analysis_name)
  )

}


# Helper function to propperly open the RStudio Viewer
myView <- function(x, title) {
  get("View", envir = as.environment("package:utils"))(x, title)
}
# View the metadata of the current project. Only Addin
rc_view_metadata <- function() {

  load(list.files(here::here(), ".RData$"))

  attr(get("redcap_data"), "metadata") |> myView("Metadata")

}

# View the description of the datasets. Only Addin
view_datasets <- function() {

  dplyr::tibble(
    dataset = names(get("datasets")),
    description = purrr::map_chr(
      get("datasets"),
      ~ifelse(is.null(attr(., "description")), NA, attr(., "description"))
    )
  ) |> myView("Datasets Description")

}

