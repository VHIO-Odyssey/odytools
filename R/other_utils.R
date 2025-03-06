#' @importFrom utils head
NULL

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
    stringr::str_detect(stringr::str_c(file_name, "_", today_num, ".", extension)) |>
    any()
  today_present_mult <- list.files(path = path) |>
    stringr::str_detect(stringr::str_c(file_name, "_", today_num, "_\\d", ".", extension)) |>
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

#' Generate a file path for saving output with versioning.
#'
#' This function constructs a save path using the provided components and appends a version (today's date) to the file name.
#'
#' @param ... Components of the path and file name.
#'
#' @return A character string representing the save path.
#'
#' @export
ody_save_path <- function(...) {

  path <- stringr::str_c(head(c(...), -1), collapse = "/")

  file <- tail(c(...), 1) |>
    stringr::str_split("\\.") |>
    unlist()

  here::here(
    path,
    ody_add_version(
      file[1], file[2],
      here::here(path)
    )
  )

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

  rlang::check_installed(c("conflicted", "git2r"))

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

  # Gitignore template
  file.copy(
    system.file(
      "project_templates", "gitignore_template", package = "odytools"
    ),
    here::here(".gitignore")
  )

  # Memento template
  file.copy(
    system.file(
      "project_templates", "memento_template.md", package = "odytools"
    ),
    here::here("docs", stringr::str_c(project_name, "_memento.md"))
  )

}

# Helper function to create a lockfile
save_lock <- function() {

  rlang::check_installed("renv")

  lock_exists <- any(list.files(here::here()) == "renv.lock")

  if (lock_exists) {

    question <- rstudioapi::showQuestion(
      "Save Lock file",
      "This action will overwrite an already existing Lock file. Are you sure?",
      ok = "Yes, overwrite.", cancel = "No, cancel."
    )

    if (!question) stop("Aborted by user")

  }

  renv::lockfile_create() |>
    renv::lockfile_write()

}

update_odytools <- function() {

  rlang::check_installed("devtools")

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


#' Glue to Languange
#'
#' Glue a string and convert it to a language object.
#'
#' @param ... Expressions to glue and transform into language objects.
#' @param .envir Environment to evaluate each expression in.
#' @param .eval If TRUE, the function evaluates the language object.
#'
#' @return A language object or the its evaluation if .eval = TRUE
#' @export
ody_glue2lang <- function(..., .envir = parent.frame(), .eval = FALSE) {

  rlang::check_installed("glue")

  glued_lang <- glue::glue(..., .envir = .envir) |>
    str2lang()

  if (.eval) eval(glued_lang, envir = .envir) else glued_lang

}

# Function to check if exists and updated renv.lock and a git repository
# update_threshold is the number of days to consider the lockfile outdated
check_renvlock <- function(update_threshold = 30) {

  git_last_modif <- file.mtime(here::here(".git"))
  renvlock_last_modif <- file.mtime(here::here("renv.lock"))

  if (!is.na(git_last_modif)) {
    repository <- git2r::repository(here::here())
    last_commit <- git2r::commits(repo = repository)[[1]]
  }

  messages <- list(
    "Please, take care of your future self:",
    "Consider adding a Lockfile to this project.",
    "Consider starting a git repository."
  )

  if (is.na(renvlock_last_modif) && is.na(git_last_modif)) {
    cli::cli_alert_warning(messages[1])
    cli::cli_ul(messages[2:3])
  } else if (is.na(renvlock_last_modif)) {
    cli::cli_alert_warning(messages[1])
    cli::cli_ul(messages[2])
  } else if (is.na(git_last_modif)) {
    cli::cli_alert_warning(messages[1])
    cli::cli_ul(messages[3])
  } else {
    last_commit_date <- last_commit$author$when |>
      lubridate::as_datetime(tz = Sys.timezone())
    last_renvlock_date <- renvlock_last_modif |>
      lubridate::as_datetime(tz = Sys.timezone())
    dif_time <- lubridate::time_length(
      last_renvlock_date - last_commit_date, "days"
    ) |>
      round(2)

    if (dif_time < -1 * update_threshold) {

    cli::cli_alert_warning(
      stringr::str_c("Last renv.lock: ", lubridate::as_date(last_renvlock_date))
    )
    cli::cli_alert_warning(
      stringr::str_c("Last commit: ", lubridate::as_date(last_commit_date))
    )
    cli::cli_alert_warning(
      stringr::str_c(
        "Time difference of ", dif_time, " days"
      )
    )

    }

    if (dif_time >= -1 * update_threshold) {

      cli::cli_alert_success(
        stringr::str_c(
          "Last renv.lock: ", lubridate::as_date(last_renvlock_date)
        )
      )
      cli::cli_alert_success(
        stringr::str_c(
          "Last commit: ", lubridate::as_date(last_commit_date)
        )
      )
      cli::cli_alert_success(
        stringr::str_c(
          "Time difference of ", dif_time, " days"
        )
      )
    }

  }

}

#' Convert GT Table to Image
#'
#' This function converts a GT table object into an image file. It supports outputting
#' the image either as a raster image directly or as a plot made with ggplot.
#'
#' @param gt_table The GT table object to be converted into an image.
#' @param type The type of output image. Either "raster" for a raster image or "ggplot"
#'        for a plot created with ggplot. Defaults to "raster".
#' @param zoom Zoom factor for the GT table rendering, where higher values result in
#'        higher resolution images. Defaults to 2.
#'
#' @details "raster" output can be used with `ggplot2::annotate_raster` to add the image on a ggplot. "ggplot" output is usefull in combination with `patchwork` (since gt 0.11.0 this last option is better achieved with the function `gt::as_gtable()`).
#'
#' @return An image object, either of class `magick-image` (for "raster" type) or
#'         a ggplot object (for "ggplot" type).
#'
#' @export
ody_gt2image <- function(gt_table, type = c("raster", "ggplot"), zoom = 2) {

  rlang::check_installed(c("webshot2", "magick", "grDevices"))

  type <- rlang::arg_match(type)

  path_gt_table_image <- tempfile(fileext = ".png")

  gt::gtsave(
    gt_table,
    filename = path_gt_table_image,
    zoom = zoom
  )

  table_image <- magick::image_read(path_gt_table_image)

  if (type == "raster") return(table_image)

  magick::image_ggplot(table_image, interpolate = TRUE)

}

#' Apply Function on Pattern
#'
#' This function applies a specified function to columns of a data frame that match a given pattern.
#'
#' @param df A data frame.
#' @param fn A function to apply to the columns that match the pattern.
#' @param pattern A regular expression pattern to match.
#' @param all_any A function (`all` or `any`) to determine whether all or any of the elements of a column should match the pattern (default is `any`).
#' @param exclude A character vector of column names to exclude from the pattern matching. That means that the function will not be applied to these columns even if they match the pattern.
#'
#' @return A data frame like the original input with the function applied to the matching columns.
#' @export
ody_apply_on_pattern <-  function(
    df,
    fn,
    pattern,
    all_any = any,
    exclude = NULL) {

  # Target cols to check for pattern are all character columns except those in
  # exclude
  target_cols <-
    df |>
    dplyr::select(-tidyselect::any_of(exclude)) |>
    dplyr::select(tidyselect::where(is.character)) |>
    names()

  df |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(target_cols),
        function(x) {
          if (all_any(stringr::str_detect(x, pattern), na.rm = TRUE)) {
            fn(x)
          } else {
            x
          }
        }
      )
    )

}

#' Label Data Frame
#'
#' This function labels a data frame according to a provided dictionary. It sets variable labels and value labels for specified variables within the data frame.
#'
#' @param raw_data A data frame containing the raw data to be labeled.
#' @param dictionary A dictionary data frame with the following columns:
#' - `variable`: The name of the variable to be labeled.
#' - `variable_label` (optional): The label to assign to the variable.
#' - `value`: The value to be labeled.
#' - `value_label`: The label to assign to the value.
#' Note that columns `variable` and `variable_label` will contain as many repeated values as the number of values in the `value` column.
#'
#' @return A data frame with labeled variables and corresponding values according to the provided dictionary.
#' @export
ody_label_df <- function(raw_data, dictionary) {

  # Nested dictionary with vector dictionary of each variable
  nested_dic <- dictionary |>
    dplyr::select(.data$variable, .data$value_label, .data$value) |>
    tidyr::nest(dic = c("value_label", "value")) |>
    dplyr::mutate(
      vec_dic = purrr::map_chr(
        .data$dic,
        ~stringr::str_c(
          "c(",
          stringr::str_c("`", .$value_label, "` = '", .$value, "'") |>
            stringr::str_c(collapse = ", "),
          ")"
        )
      )
    ) |>
    dplyr::filter(!is.na(.data$vec_dic))

  # Value labels of each variable in
  val_labels_text <- stringr::str_c("`", nested_dic$variable, "` = ", nested_dic$vec_dic) |>
    na.omit() |>
    stringr::str_c(collapse = ", ")

  # Las variables a etiquetar han de ser caracter
  raw_data_chr <- raw_data |>
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(nested_dic$variable), as.character)
    )

  if (any(names(dictionary) == "variable_label")) {

    var_labels_text <- dictionary |>
      dplyr::select(.data$variable_label, .data$variable) |>
      na.omit() |>
      unique() |>
      purrr::pmap(~stringr::str_c("`", .y, "`",  " = '", .x, "'")) |>
      stringr::str_c(collapse = ", ")

    labelled_vars_data <- stringr::str_c(
      "labelled::set_variable_labels(raw_data_chr,", var_labels_text, ")"
    ) |>
      str2lang() |>
      eval()

  } else {

    labelled_vars_data <- raw_data_chr

  }

  stringr::str_c(
    "labelled::set_value_labels(labelled_vars_data,", val_labels_text, ")"
  ) |>
    str2lang() |>
    eval()

}


