#' Get a version date for a render.
#'
#' @param file_name File name.
#' @param extension File extension.
#' @param path The location the file will be stored
#'
#' @details The function looks for the possible files named as file_name followed by today's date in path. If none, the current date is returned as version indicator. If any, a number is added to the date.
#'
#' @return A character
#' @export
ody_add_version <- function(file_name, extension = "html", path = ".") {

  today_num <- lubridate::today() |>
    stringr::str_remove_all("-")
  today_present <- list.files(path = path) |>
    stringr::str_detect(stringr::str_c(file_name, "_", today_num)) |>
    any()
  today_present_mult <- list.files(path = path) |>
    stringr::str_detect(stringr::str_c(file_name, "_", today_num, "_\\d")) |>
    any()

  if (today_present & !today_present_mult) {
    current_ver <- stringr::str_c("_", today_num, "_2")
  } else if (today_present_mult) {
    current_ver <- list.files(path = path) |>
      stringr::str_extract(stringr::str_c(today_num, "_\\d")) |>
      unique() |>
      na.omit() |>
      stringr::str_extract("\\d$") |>
      as.numeric() |>
      max()

    current_ver <- stringr::str_c("_", today_num, "_", current_ver + 1)
  } else {

    current_ver <- stringr::str_c("_", today_num)

  }

  stringr::str_c(file_name, current_ver, ".", extension)

}



#' Change column names
#'
#' Modify column names according to specified changes in a variables data frame.
#'
#' @param data_frame Data frame the names should by modified
#' @param names_df 2 columns data frame. The first column must contain the new names and the second one, the current ones.
#'
#' @return data_frame with names changes according to names_df.
#' @export
ody_change_names <- function(data_frame, names_df) { #CANDIDATE

  data_frame_names <- names(data_frame)

  names(names_df) <- c("new", "current")

  new_names <- purrr::map_chr(
    data_frame_names,
    function(x) {

      name <- dplyr::filter(names_df, .data$current == x) |>
        dplyr::pull("new")

      if (length(name) == 0) x else name

    }
  )

  names(data_frame) <- new_names

  data_frame

}


#' Extra options for some odytools functions
#'
#' @param label_size ody_summarise_df: If the variable is labelled, ody_summarise_df shows the label below the var name. This argument controls its relative size.
#' @param minwidth_var ody_summarise_df: Minimum width of the Variable column.
#' @param n_dec ody_summarise_df: Number of decimals shown in a continuous variable description.
#' @param minwidth_level ody_summarise_df: Minimum width of the Level column in the details of a discrete variable.
#' @param width_density_plot ody_summarise_df: Total width of the details of continuous variable.
#' @param width_bar ody_summarise_df: Width of the percentage bars.
#' @param groups_plot_height ody_summarise_df: density plot height whem grouping_var != NULL
#' @param full_group_label ody_summarise_df: Add or not the grouping var name to the group name.
#' @param border_color Border color in reactable tables.
#'
#' @return An list of arguments internaly used in some odytools functions
#' @export
ody_options <- function(label_size = 1,
                        minwidth_var = 200,
                        n_dec = 1,
                        minwidth_level = 100,
                        width_density_plot = 700,
                        width_bar = 100,
                        groups_plot_height = 300,
                        full_group_label = FALSE,
                        border_color = "#DEDEDE") {

  list(
    label_size = label_size,                 # ody_summarise_df
    minwidth_var = minwidth_var,             # ody_summarise_df
    n_dec = n_dec,                           # ody_summarise_df
    minwidth_level = minwidth_level,         # ody_summarise_df
    width_density_plot = width_density_plot, # ody_summarise_df
    width_bar = width_bar,                   # ody_summarise_df
    groups_plot_height = groups_plot_height, # ody_summarise_df
    full_group_label = full_group_label,     # ody_summarise_df
    border_color = border_color              # ody_summarise_df
  )


}


#' Start a plain non-RedCap project
#'
#' @export
ody_proj_init <- function() {

  rlang::check_installed("conflicted")

  project_name <- get_project_name()

  # Directories
  dir.create(here::here("data"))
  dir.create(here::here("docs"))
  dir.create(here::here("analysis"))
  dir.create(here::here("functions"))
  dir.create(here::here("quality"))

  # Root Templates
  file.copy(
    system.file(
      "project_templates", "Rprofile_template.R", package = "odytools"
    ),
    here::here(".Rprofile")
  )
  file.copy(
    system.file(
      "project_templates", "dependencies_template.R", package = "odytools"
    ),
    here::here(stringr::str_c(project_name, "_dependencies.R"))
  )
  file.copy(
    system.file(
      "project_templates", "sandbox_template.R", package = "odytools"
    ),
    here::here(stringr::str_c(project_name, "_sandbox.R"))
  )

  # Data Template
  file.copy(
    system.file(
      "project_templates", "data_template.R", package = "odytools"
    ),
    here::here("data", stringr::str_c(project_name, "_data.R"))
  )

  # function template
  file.copy(
    system.file(
      "project_templates", "functions_template.R", package = "odytools"
    ),
    here::here("functions", stringr::str_c(project_name, "_functions.R"))
  )

  # Report templates
  file.copy(
    system.file(
      "project_templates", "report_template.qmd", package = "odytools"
    ),
    here::here("analysis", stringr::str_c(project_name, "_analysis.qmd"))
  )
  file.copy(
    system.file(
      "project_templates", "report_template.qmd", package = "odytools"
    ),
    here::here(
      "quality", stringr::str_c(project_name, "_quality.qmd")
    )
  )

}

# Helper function to create a lockfile
save_lock <- function() {

  lock_exists <- any(list.files(here::here()) == "renv.lock")

  if (lock_exists) {

    question <- rstudioapi::showQuestion(
      "Lock file already exists",
      "This action will overwrite an already existing Lock file. Are you sure?",
      ok = "Yes, overwrite.", cancel = "No, cancel."
    )

    if (!question) stop("Aborted by user")

  }

  renv::lockfile_create() |>
    renv::lockfile_write()

}

update_odytools <- function() {

  current_version <- as.character(packageVersion("odytools"))

  sure <- rstudioapi::showQuestion(
    "Update odytools from GitHub",
    stringr::str_c(
      "Do you want to update odytools? (current version ",
      current_version, ")"
    )
  )

  if (!sure) stop("Update aborted")

  if ("odytools" %in% (.packages())) {
    detach("package:odytools", unload = TRUE)
  }

  master_branch <- rstudioapi::showQuestion(
    "Update odytools from GitHub",
    "From which branch do you want to update?",
    ok = "Master", cancel = "Dev"
  )

  if (master_branch) {
    devtools::install_github("VHIO-Odyssey/odytools")
  } else {
    devtools::install_github("VHIO-Odyssey/odytools@dev")
  }

  require("odytools")
}

# Helper function of ody_compare_1_vs_others
compare_1_vs_others <- function(df) {

  level <- levels(df[[1]])

  p_values <- purrr::map_dbl(
    level,
    ~wilcox.test(
      df[[2]][df[[1]] == .],
      df[[2]][df[[1]] != .]
    )$p.value
  )

  median_group <- purrr::map_dbl(
    level, ~median(df[[2]][df[[1]] == .], na.rm = TRUE)
  )

  median_others <- purrr::map_dbl(
    level, ~median(df[[2]][df[[1]] != .], na.rm = TRUE)
  )

  tibble::tibble(
    group = level,
    median_group = median_group,
    median_others = median_others,
    p_value = p_values
  )

}

#' Compare 1 level vs all other
#'
#' The function compares the values that correspond to one level of a factor with the values of all the other levels.
#'
#' @param data_frame A data frame with a grouping variable in the first column and numeric variables in the rest of the columns.
#' @param p_method Method to adjust p-values. Default is "BH". Adjustment is performed within each variable.
#'
#' @export
ody_compare_1_vs_others <- function(data_frame, p_method = "BH") {

  result <- purrr::map_df(
    names(data_frame)[-1],
    ~compare_1_vs_others(
      data_frame |>
        select(1, .)
    ) |>
      mutate(variable = ., .before = 1)
  )

  result |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(
      adj_p = p.adjust(.data$p_value, method = p_method)
    ) |>
    dplyr::ungroup()

}
