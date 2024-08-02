#' @importFrom utils View
#' @importFrom utils packageVersion

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

  rlang::check_installed("conflicted")

  project_name <- get_project_name()

  # Directories
  dir.create(here::here("data", "imports"), recursive = TRUE)
  dir.create(here::here("data", "extra"))
  dir.create(here::here("data", "exports"))
  dir.create(here::here("data", "datasets"))
  dir.create(here::here("docs"))
  dir.create(here::here("analysis"))
  dir.create(here::here("functions"))
  dir.create(here::here("quality"))

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

  # Datasets Template
  file.copy(
    system.file(
      "redcap_templates", "datasets_template.R", package = "odytools"
    ),
    here::here("data", "datasets", stringr::str_c(project_name, "_datasets.R"))
  )

  # function template
  file.copy(
    system.file(
      "redcap_templates", "functions_template.R", package = "odytools"
    ),
    here::here("functions", stringr::str_c(project_name, "_functions.R"))
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
      "quality", stringr::str_c(project_name, "_quality.qmd")
    )
  )

  # hardcoding template
  file.copy(
    system.file(
      "redcap_templates", "hardcoded_values.csv", package = "odytools"
    ),
    here::here(
      "data", "imports", stringr::str_c(project_name, "_hardcoded_values.csv")
    )
  )

  # Gitignore template
  file.copy(
    system.file(
      "redcap_templates", "gitignore_template", package = "odytools"
    ),
    here::here(".gitignore")
  )

  # Memento template
  file.copy(
    system.file(
      "redcap_templates", "memento_template.md", package = "odytools"
    ),
    here::here("docs", stringr::str_c(project_name, "_memento.md"))
  )

}

# Helper function to make the datasets
rc_make_datasets <- function(redcap_data) {

  project_name <- get_project_name()

  import_date <- get_import_date(redcap_data)

  datasets_scripts <- list.files(here::here("data", "datasets"), ".R$")

  purrr::walk(
    here::here("data", "datasets", datasets_scripts),
    source, local = rlang::current_env()
  )

  # Objects declared as datasets
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

  # Export datasets declared as export
  export_index <- purrr::map_lgl(
    datasets, ~attr(., "export")
  )

  if (sum(export_index) > 0) {

    exported_tables <- datasets[export_index]
    file_names <- stringr::str_c(
      project_name, "_", names(exported_tables), "_", import_date, ".csv"
    )

    purrr::walk2(
      exported_tables, file_names,
      ~readr::write_csv2(.x, here::here("data", "exports", .y))
    )

  }

  datasets

}

# Start/Update a RedCap Project. Only Addin
rc_init_update <- function() {

  rlang::check_installed("git2r")

  project_name <- get_project_name()
  if (length(project_name) == 0) stop("No RStudio project detected.")

  # It is an update?
  # It is assumed to be an update if there is an RData named after the project.
  expected_rdata <- stringr::str_c(project_name, ".RData")
  is_update <- any(expected_rdata == list.files(here::here()))

  if (is_update) {
    load(here::here(stringr::str_c(project_name, ".RData")))
    pre_update_project <- attr(get("redcap_data"), "project_info")$project_title
    rm(redcap_data, datasets)
  } else {
    rc_init_dirs_files()
  }

  # Redcap Import
  # Is there a stored token for this project?
  api_renv_name <- project_name |>
    stringr::str_to_upper() |>
    c("_API_KEY") |>
    stringr::str_c(collapse = "")
  token <- Sys.getenv(api_renv_name)

  if (token == "") {
    token <- rstudioapi::askForPassword(
      prompt = "Please enter a RedCap token:"
    )
    is_new_token <- TRUE
  } else {
    is_new_token <- FALSE
  }

  # The import if wrapped with try in case the token has changed and the stored
  # one is not valid anymore
  redcap_data <- try(ody_rc_import(token))

  if (all(class(redcap_data) == "try-error")) {
    token <- rstudioapi::askForPassword(
      prompt = "The token stored in .Renviron is no longer valid, please enter the current token:"
    )
    is_new_token <- TRUE
    redcap_data <- ody_rc_import(token)
  }

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

  # Saving redcap_data and datasets must be done after checking the import
  # actually belongs to the expected project.

  # before saving any hardcode modification is applyed

  hardcoded_values <- readr::read_csv2(
    here::here(
      "data", "imports", stringr::str_c(project_name, "_hardcoded_values.csv")

    ), col_types = readr::cols(.default = readr::col_character())
  ) |> suppressMessages()

  if (nrow(hardcoded_values) > 0) {
    redcap_data <- hardcode_values(redcap_data, hardcoded_values)
    message("This project has hardcoded values. Check them with attr(redcap_data, \"hardcoded_values\")\n")
  }

  message("Computing datasets...\n")
  datasets <- rc_make_datasets(redcap_data) |> suppressMessages()

  save(
    redcap_data, datasets,
    file = here::here(stringr::str_c(project_name, ".RData"))
  )

  if (is_update) {
    message("Project successfully updated.\n")
  } else {
    message("Project successfully started.\n")
  }

  if (is_new_token) {
    token_renviron <- stringr::str_c(api_renv_name, "=", token)
    writeLines(
      c(readLines("~/.Renviron"), token_renviron),
      "~/.Renviron"
    )
    message(
      "The provided token has been stored in ~/.Renviron.\nIt will be available after restarting your R session.\n"
    )
  }

  source(here::here(stringr::str_c(project_name, "_dependencies.R")))

}

# Refresh the Datasets List. Only Addin.
rc_refresh_datasets <- function() {

  message("Refreshing datasets...\n")

  load(list.files(here::here(), ".RData$"))

  redcap_data <- get("redcap_data")
  datasets <- rc_make_datasets(redcap_data) |> suppressMessages()
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

# Save a copy of the current redcap_data and datasets. Only Addin
rc_back_up <- function() {

  load(list.files(here::here(), ".RData$"))

  # this to avoid package check complains about undefined objects
  redcap_data <- get("redcap_data")
  datasets <- get("datasets")

  project_name <- get_project_name()
  import_date <- get_import_date(redcap_data)

  backup_name <- stringr::str_c(
    project_name, "_import_", import_date, ".RData"
  )

  backup_date <- Sys.time()
  attr(redcap_data, "backup_date") <- backup_date
  attr(datasets, "backup_date") <- backup_date

  save(
    redcap_data, datasets,
    file = here::here("data", "imports", backup_name)
  )

  message(
    "A backup copy of the import and its derived datasets has been stored\nin data/imports with the name ", backup_name, "\n"
  )

}

#' Declare an object as belonging to datasets list
#'
#' @param object Object to add to datasets. Usually a data frame.
#' @param description Optional description of the object.
#' @param export Should the object be exported as a table? If TRUE (and the object is a data frame) the table is exported at data/exports/
#'
#' @export
ody_add_to_datasets <- function(object, description = NULL, export = FALSE) {

  attr(object, "is_dataset") <- TRUE
  attr(object, "description") <- description

  if (export && is.data.frame(object)) {
    attr(object, "export") <- TRUE
  } else {
    attr(object, "export") <- FALSE
  }

  object

}

#' Get the name and the import dates (last and loaded) of the project
#'
#' @param as_list If FALSE, the information is printed on the console. Otherwise, it returns a list (usefull for reports)
#'
#' @details
#' "Where am I?" addin calls to this function.
#' "Last" and "Loaded" imports can be different. If so, CAUTION, you are a timetraveller
#'
#' @export
ody_rc_current <- function(as_list = FALSE) {

  # Current Redcap data in main RData
  rdatas <- list.files(here::here(), ".RData$")
  if (length(rdatas) == 0) {
    stop("No Redcap project detected.\nYou can set up one by clicking on Addins/Odytools/Start|Update Redcap project.\n")
  }
  else {
    purrr::walk(here::here(rdatas), load, envir = rlang::current_env())
    if (!exists("redcap_data", inherits = FALSE)) {
      return(message("No Redcap project detected.\nYou can set up one by clicking on Addins/Odytools/Start|Update Redcap project.\n"))
    }
  }

  # Import date of the loaded redcap
  loaded_import_date  <- attr(
    get("redcap_data", envir = .GlobalEnv),
    "import_date"
  ) |>
    stringr::str_extract( "....-..-.. ..:..")

  # Info of the current data (the oine stored in the main RData)
  import_date <- attr(get("redcap_data"), "import_date") |>
    stringr::str_extract("....-..-.. ..:..")
  project_name <- attr(get("redcap_data"), "project_info")$project_title
  project_id <- attr(get("redcap_data"), "project_info")$project_id

  if (as_list) {
    list(
      project = stringr::str_c(project_name, " (PID ", project_id, ")"),
      last = import_date,
      loaded = loaded_import_date
    )
  }else {
    message(stringr::str_c(
      "Project: ", stringr::str_c(project_name, " (PID ", project_id, ")"),
      "\nLast import: ", import_date,
      "\nLoaded import: ", loaded_import_date,
      "\n"
    ))
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
    exported = purrr::map_lgl(get("datasets"), ~attr(., "export")),
    description = purrr::map_chr(
      get("datasets"),
      ~ifelse(is.null(attr(., "description")), NA, attr(., "description"))
    )
  ) |> myView("Datasets Description")

}

#' Paradox-Free Time Travelling
#'
#' Replace the current redcap_data and datasets with the ones from a previous backup stored in the data/imports folder.
#'
#' @param timepoint Timepoint pattern.
#'
#' @details The back-ups are named after the project and the import date. The timepoint pattern is a regular expression to match the name of the back-up file. The pattern must match one and only one back-up file.
#'
#' @export
ody_rc_timetravel <- function(timepoint) {

  import <- list.files(here::here("data", "imports"), ".RData$") |>
    stringr::str_subset(timepoint)


  if (length(import) == 0) {

    stop("No available timepoint")

  }

  if (length(import) > 1) {

    stop("Ambiguous timepoint")

  }

  load(here::here("data", "imports", import), envir = .GlobalEnv)

  ody_rc_current()

}

# Helper function to modify values in a longitudinal project
hardcode_value_longproj <- function(
    redcap_data,
    event,
    form,
    variable,
    id,
    instance,
    corrected_value) {

  event_index <- redcap_data$redcap_event_name == event

  form_index <- redcap_data$redcap_event_data[
    event_index
  ][[1]]$redcap_form_name == form

  subject_index <- redcap_data$redcap_event_data[
    event_index
  ][[1]]$redcap_form_data[
    form_index
  ][[1]][, 1] == id

  if (is.na(instance)) {

    case_index <- subject_index

  } else {

    instance_index <- redcap_data$redcap_event_data[
      event_index
    ][[1]]$redcap_form_data[
      form_index
    ][[1]]$redcap_instance_number == instance

    case_index <- subject_index & instance_index

  }

  selected <- redcap_data$redcap_event_data[
    event_index
  ][[1]]$redcap_form_data[
    form_index
  ][[1]][case_index, variable]

  if (nrow(selected) != 1) {
    stop("Ambiguous indications. Did you forget to specify the instance?")
  }

  redcap_data$redcap_event_data[
    event_index
  ][[1]]$redcap_form_data[
    form_index
  ][[1]][case_index, variable] <- corrected_value

  list(
    redcap_data,
    dplyr::pull(selected)
  )

}


# Helper function to modify values in a classic project
hardcode_value_clasproj <- function(
    redcap_data,
    form,
    variable,
    id,
    instance,
    corrected_value) {

  form_index <- redcap_data$redcap_form_name == form

  subject_index <- redcap_data$redcap_form_data[
    form_index
  ][[1]][, 1] == id

  if (is.na(instance)) {

    case_index <- subject_index

  } else {

    instance_index <- redcap_data$redcap_form_data[
      form_index
    ][[1]]$redcap_instance_number == instance

    case_index <- subject_index & instance_index

  }

  selected <- redcap_data$redcap_form_data[
    form_index
  ][[1]][case_index, variable]

  if (nrow(selected) != 1) {
    stop("Ambiguous indications. Did you forget to specify the instance?")
  }

  redcap_data$redcap_form_data[
    form_index
  ][[1]][case_index, variable] <- corrected_value

  list(
    redcap_data,
    dplyr::pull(selected)
  )

}

# Function to apply values modification according to the strcuture of the
# project.
hardcode_values <- function(redcap_data, hardcode_df) {

  orig_values <- character()

  if (all(!is.na(hardcode_df$event))) {

    for (i in 1:nrow(hardcode_df)) {

      correction <- hardcode_value_longproj(
        redcap_data,
        hardcode_df$event[i],
        hardcode_df$form[i],
        hardcode_df$variable[i],
        hardcode_df$id[i],
        hardcode_df$instance[i],
        hardcode_df$corrected_value[i]
      )

      redcap_data <- correction[[1]]

      orig_values <- c(orig_values, correction[[2]])

    }

  } else {
    for (i in 1:nrow(hardcode_df)) {

      correction <- hardcode_value_clasproj(
        redcap_data,
        hardcode_df$form[i],
        hardcode_df$variable[i],
        hardcode_df$id[i],
        hardcode_df$instance[i],
        hardcode_df$corrected_value[i]
      )

      redcap_data <- correction[[1]]

      orig_values <- c(orig_values, correction[[2]])
    }

  }

  corrections <- hardcode_df |>
    dplyr::mutate(
      original_value = orig_values,
      .before = "corrected_value"
    )

  attr(redcap_data, "hardcoded_values") <- corrections

  redcap_data

}
