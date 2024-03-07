#' @importFrom stats na.omit
#' @importFrom utils tail

# Helper function of ody_verify_conformance to count format grouped in different
# ways
count_grouped_cases <- function(count_var, grouped) {
  count_var <- na.omit(count_var)

  if (grouped == "semi") {
    # fechas y números agrupados, el resto de formatos se mantiene separado.
    tibble::tibble(
      count_var,
      cases = dplyr::case_when(
        stringr::str_detect(count_var, "^-?\\d+$") ~ "integer",
        stringr::str_detect(count_var, "^-?\\d+[\\.,]\\d+$") ~ "dec_number",
        !is.na(lubridate::ymd(count_var, quiet = TRUE)) ~ "date_ymd",
        !is.na(lubridate::dmy(count_var, quiet = TRUE)) ~ "date_dmy",
        TRUE ~ stringr::str_c("'", count_var, "'")
      )
    ) |>
      dplyr::count(.data[["cases"]]) |>
      dplyr::arrange(dplyr::desc(.data[["n"]]))
  } else if (grouped == "format") {
    # integer y number no se distingue y tampoco se distingue entre
    # character symbol, alpha_num, alpha_sym, num_sym, alpha_num_sym.
    # Es el que se usa para calcular un índice de shannon
    tibble::tibble(
      count_var,
      cases = dplyr::case_when(
        stringr::str_detect(count_var, "^-?\\d+([\\.,]\\d+)?$") ~ "number",
        !is.na(lubridate::ymd(count_var, quiet = TRUE)) ~ "date_ymd",
        !is.na(lubridate::dmy(count_var, quiet = TRUE)) ~ "date_dmy",
        stringr::str_detect(count_var, "^[:blank:]+$") ~ "blank",
        TRUE ~ "character"
      )
    ) |>
      dplyr::count(.data[["cases"]]) |>
      dplyr::arrange(dplyr::desc(.data[["n"]]))
  } else if (grouped == "subformat") {
    # se agrupa todo
    tibble::tibble(
      count_var,
      cases = dplyr::case_when(
        stringr::str_detect(count_var, "^-?\\d+$") ~ "integer",
        stringr::str_detect(count_var, "^-?\\d+[\\.,]\\d+$") ~ "dec_number",
        !is.na(lubridate::ymd(count_var, quiet = TRUE)) ~ "date_ymd",
        !is.na(lubridate::dmy(count_var, quiet = TRUE)) ~ "date_dmy",
        stringr::str_detect(count_var, "^[:blank:]+$") ~ "blank",
        stringr::str_detect(count_var, "^[[:alpha:][:blank:]]+$") ~ "alpha",
        stringr::str_detect(count_var, "^[[:punct:][:symbol:][:blank:]]+$") ~ "symbol",
        stringr::str_detect(count_var, "^[[:alnum:][:blank:]]+$") ~ "alpha_num",
        stringr::str_detect(count_var, "^[[:punct:][:symbol:][:alpha:][:blank:]]+$") ~ "alpha_sym",
        stringr::str_detect(count_var, "^[[:punct:][:symbol:][:digit:][:blank:]]+$") ~ "num_sym",
        stringr::str_detect(count_var, "^[[:graph:][:blank:]]+$") ~ "alpha_num_sym",
        TRUE ~ "subformat_fail"
      )
    ) |>
      dplyr::count(.data[["cases"]]) |>
      dplyr::arrange(dplyr::desc(.data[["n"]]))
  } else if (grouped == "no") {
    # no se agrupa nada
    tibble::tibble(
      cases = stringr::str_c("'", count_var, "'"),
    ) |>
      dplyr::count(.data[["cases"]]) |>
      dplyr::arrange(dplyr::desc(.data[["n"]]))
  }
}


#' Verify Completeness
#'
#' Verifies the completeness of each variable of a data frame.
#'
#' @param data_frame The data frame to be checked.
#' @param missing_values Values considered as missing. Actual NA's and empty characters ("") are always considered missing values.
#' @param conditions_list A list to define the conditional presence of the variables. Needed when the presence of a variable depends on the value of other variable.
#' @param id_var The id variable. By default, the function uses row numbers.
#'
#' @return A completeness tibble.
#' @export
ody_verify_completeness <- function(
    data_frame,
    missing_values = NULL,
    conditions_list = NULL,
    id_var = "row_number") {
  if (!is.null(conditions_list)) {
    conditions_list <- complete_list(conditions_list)
  }

  if (id_var == "row_number") {
    data_frame <- data_frame |>
      dplyr::mutate(
        row_number = 1:dplyr::n(), .before = 1
      )
  }

  missing_values <- c("", missing_values)

  # All variable names except id_var name
  var_names <- data_frame |>
    dplyr::select(-tidyselect::all_of(id_var)) |>
    names()

  data_map <- tibble::tibble(
    data_list = purrr::map(
      dplyr::select(data_frame, -tidyselect::all_of(id_var)),
      function(value) {
        cbind(dplyr::select(data_frame, tidyselect::all_of(id_var)), value)
      }
    ),
    rel_index = var_names %in% names(conditions_list),
    name_var = var_names
  )

  attach(data_frame, warn.conflicts = FALSE)
  # Data with the expected values
  data_filtered <- purrr::pmap(
    data_map,
    function(data_list, rel_index, name_var) {
      if (rel_index) {
        data_list |>
          dplyr::filter(eval(str2lang(conditions_list[[name_var]])))
      } else {
        data_list
      }
    }
  )

  # Data with the unexpected values of the filtered variables
  data_antifiltered <- purrr::pmap(
    data_map,
    function(data_list, rel_index, name_var) {
      if (rel_index) {
        data_list |>
          dplyr::filter(!eval(str2lang(conditions_list[[name_var]])))
      } else {
        data_list
      }
    }
  )

  detach(data_frame)

  data_missing <- data_filtered |>
    purrr::map(
      ~ . |>
        dplyr::filter(is.na(value) | value %in% missing_values) |>
        dplyr::select(1)
    )

  missing_result <- tibble::tibble(
    variable = names(data_filtered),
    n_expected = purrr::map_dbl(data_filtered, nrow),
    n_missing = purrr::map_dbl(data_missing, nrow),
    ids_missing = purrr::map_chr(
      data_missing,
      ~ dplyr::pull(., 1) |>
        na.omit() |> # For the rare cases id is missing
        # (will never miss if id_var = row_number)
        stringr::str_c(collapse = ", ") %>%
        {
          ifelse(. == "", NA, .)
        }
    ),
    n_unexpected = purrr::map2_dbl(
      data_map$data_list, data_filtered, ~ nrow(.x) - nrow(.y)
    )
  )

  data_unexpected <- data_antifiltered[
    names(data_antifiltered) %in% names(conditions_list)
  ] |>
    purrr::map(
      ~ . |>
        dplyr::filter(!is.na(value) & !(value %in% missing_values)) |>
        dplyr::select(1)
    )

  unexpected_result <- tibble::tibble(
    variable = names(data_unexpected),
    n_antimissing = purrr::map_dbl(data_unexpected, nrow),
    ids_antimissing = purrr::map_chr(
      data_unexpected,
      ~ as.character(dplyr::pull(., 1)) |>
        na.omit() |> # For the rare cases id is missing
        # (will never miss if id_var = row_number)
        stringr::str_c(collapse = ", ") %>%
        {
          ifelse(. == "", NA, .)
        }
    )
  )

  cond_frame <- tibble::tibble(
    variable = names(conditions_list),
    condition = unlist(conditions_list)
  )

  if (nrow(cond_frame) != 0) {
    complet_data <- dplyr::left_join(missing_result, unexpected_result, by = "variable") |>
      dplyr::left_join(cond_frame, by = "variable") |>
      dplyr::select(
        .data$variable, .data$condition,
        tidyselect::starts_with("n_"), tidyselect::starts_with("ids")
      )

    attr(complet_data, "id_var") <- id_var
    return(complet_data)
  } else {
    complet_data <- dplyr::left_join(missing_result, unexpected_result, by = "variable") |>
      dplyr::mutate(
        condition = NA, .after = .data$variable
      )

    attr(complet_data, "id_var") <- id_var
    return(complet_data)
  }
}


#' Verify Conformance
#'
#' Cheks the format of the variables of a data frame.
#'
#' @param data_frame The data frame to be checked.
#' @param missing_values Values considered as missing. Actual NA's and empty characters ("") are always considered missing values.
#' @param max_integer_distinct Max number of different integer values that will be reported individually.
#'
#' @return A conformance tibble.
#' @export
ody_verify_conformance <- function(
    data_frame, missing_values = NULL, max_integer_distinct = 10) {
  # Todo se convierte a caracter para cazar "formatos" a partir str_detect
  data_frame <- data_frame |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), as.character)
    )

  # El "" siempre se considera un NA
  missing_values <- c("", missing_values)
  data_frame <- data_frame |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::everything(), ~ replace(., . %in% missing_values, NA)
      )
    )

  result_v0 <- dplyr::tibble(
    variable = names(data_frame),
    n_cases = purrr::map_dbl(data_frame, ~ sum(!is.na(.))),
    n_distinct = purrr::map_dbl(data_frame, ~ length(na.omit(unique(.)))),
    n_distinct_trimed = purrr::map_dbl(
      data_frame,
      ~ length(na.omit(unique(stringr::str_to_upper(stringr::str_remove_all(., "[:blank:]")))))
    ),
    uniqueness = round(.data$n_distinct_trimed / .data$n_distinct, 2),
    data_list = purrr::map(data_frame, na.omit),
    formats_table = purrr::map(.data$data_list, count_grouped_cases, grouped = "format"),
    subformats_table = purrr::map(.data$data_list, count_grouped_cases, grouped = "subformat"),
    heterogeneity = purrr::map_dbl(
      .data$formats_table,
      function(x) {
        n <- x$n
        p <- n / sum(n)
        round(-sum(p * log2(p)), 2)
      }
    ),
    formats = purrr::map_chr(
      .data$formats_table,
      function(x) {
        stringr::str_c(
          stringr::str_c(x[[1]]),
          stringr::str_c("[", x[[2]], "]"),
          collapse = ", "
        )
      }
    ),
    subformats = purrr::map_chr(
      .data$subformats_table,
      function(x) {
        stringr::str_c(
          stringr::str_c(x[[1]]),
          stringr::str_c("[", x[[2]], "]"),
          collapse = ", "
        )
      }
    ),
    # todo es integer? si es así y hay menos de max_integer_distinct el recuento de
    # esa variable se realiza sin agrupar para mostrar en los detalles.
    all_integer = purrr::map_lgl(.data$data_list, ~ all(stringr::str_detect(., "^\\d+$")))
  ) |>
    dplyr::select(
      -.data$formats_table, -.data$subformats_table,
      -.data$n_distinct_trimed
    )

  distinct_formats_detail_table <- tibble::tibble(
    n_distinct = result_v0$n_distinct,
    all_integer = result_v0$all_integer,
    data_list = result_v0$data_list
  )

  result_v0 |>
    dplyr::mutate(
      distinct_formats_detail = purrr::pmap(
        distinct_formats_detail_table,
        function(n_distinct, all_integer, data_list) {
          if (all_integer & n_distinct <= max_integer_distinct) {
            count_grouped_cases(data_list, grouped = "no")
          } else {
            count_grouped_cases(data_list, grouped = "semi")
          }
        }
      )
    ) |>
    dplyr::select(-.data$data_list, -.data$all_integer)
}

# Crea tablas html del resultado de verify_completeness
#   - completeness_table: tabla resultado de verify_completeness.
#   - text_pos: posición del texto respecto a las barras. Acepta los valores de
#   text_position de data_bars {reactablefmtr} [
#     Text labels can be displayed within the filled bars ("inside-end" or "inside-base"),
#     outside of the filled bars ("outside-end" or "outside-base"), within the
#     center of the filled bars ("center"), above the filled bars ("above"), or
#     not displayed at all ("none").
#   ]
report_completeness <- function(
    completeness_table, text_pos = "above", opt_reactable = ody_options()
  ) {
  wrong_bar_color <- "#FF0000"
  ok_bar_color <- "#00CC11"

  # min col width from n_expected to overall
  min_col_width <- 130
  # min col width of variable and condition
  min_col_width_var <- 200

  report_table <- completeness_table |>
    dplyr::mutate(
      completeness = round((.data$n_expected - .data$n_missing) / .data$n_expected, 2),
      uncompleteness = ifelse(
        .data$n_unexpected == 0, NA,
        round((.data$n_unexpected - .data$n_antimissing) / .data$n_unexpected, 2)
      ),
      overall = ifelse(
        is.na(.data$n_antimissing),
        round((.data$n_expected + .data$n_unexpected - .data$n_missing) / (.data$n_expected + .data$n_unexpected), 2),
        round((.data$n_expected + .data$n_unexpected - .data$n_missing - .data$n_antimissing) / (.data$n_expected + .data$n_unexpected), 2)
      ),
      condition = ifelse(
        is.na(.data$condition), "allways", .data$condition
      )
    ) |>
    dplyr::select(
      .data$variable, .data$condition, .data$overall,
      .data$n_expected, .data$n_missing, .data$completeness,
      .data$n_unexpected, .data$n_antimissing, .data$uncompleteness
    ) |>
    dplyr::mutate(
      overall_color = dplyr::case_when(
        .data$overall == 1 ~ ok_bar_color,
        TRUE ~ wrong_bar_color
      ),
      completeness_color = dplyr::case_when(
        .data$completeness == 1 ~ ok_bar_color,
        TRUE ~ wrong_bar_color
      ),
      uncompleteness_color = dplyr::case_when(
        .data$uncompleteness == 1 ~ ok_bar_color,
        TRUE ~ wrong_bar_color
      )
    ) |>
    dplyr::mutate(
      no = 1:dplyr::n(), .before = 1
    )

  issues_info <- completeness_table |>
    dplyr::filter(!is.na(.data$ids_missing) | !is.na(.data$ids_antimissing)) |>
    dplyr::select(.data$variable, .data$n_missing, .data$ids_missing, .data$n_antimissing, .data$ids_antimissing) |>
    tidyr::pivot_longer(2:5, names_to = c(".value", "issue"), names_pattern = "(.+)_(.+)") |>
    dplyr::filter(!is.na(.data$ids))


  if (any(!is.na(completeness_table$condition))) {
    report_table |>
      reactable::reactable(
        defaultColDef = reactable::colDef(
          align = "left"
        ),
        details = function(index) {
          issues <- issues_info |>
            dplyr::filter(.data$variable == report_table$variable[index]) |>
            dplyr::select(-.data$variable) |>
            dplyr::rename("{attr(completeness_table, 'id_var')}" := .data$ids)
          if (nrow(issues) != 0) {
            htmltools::div(
              style = "padding: 1rem",
              reactable::reactable(
                issues,
                columns = list(
                  issue = reactable::colDef(width = 100),
                  n = reactable::colDef(width = 50)
                ), highlight = TRUE, wrap = TRUE
              )
            )
          }
        },
        columns = list(
          no = reactable::colDef(width = 65),
          variable = reactable::colDef(
            sticky = "left",
            resizable = TRUE,
            minWidth = min_col_width_var
          ),
          condition = reactable::colDef(
            resizable = TRUE, minWidth = min_col_width_var
          ),
          overall = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              max_value = 1,
              fill_color_ref = "overall_color",
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          overall_color = reactable::colDef(show = FALSE),
          n_expected = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          n_missing = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              fill_color = wrong_bar_color,
              max_value = max(report_table$n_expected),
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          completeness = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              max_value = 1,
              fill_color_ref = "completeness_color",
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          completeness_color = reactable::colDef(show = FALSE),
          n_unexpected = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          n_antimissing = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              max_value = max(report_table$n_unexpected),
              fill_color = wrong_bar_color, text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          uncompleteness = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              max_value = 1,
              fill_color_ref = "uncompleteness_color",
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          uncompleteness_color = reactable::colDef(show = FALSE)
        ),
        highlight = TRUE,
        wrap = TRUE, pagination = FALSE,
        searchable = TRUE, onClick = "expand",
        theme = reactable::reactableTheme(
          borderWidth = 1, borderColor =opt_reactable$border_color
        )
      )
  } else {
    report_table |>
      reactable::reactable(
        defaultColDef = reactable::colDef(
          align = "left"
        ),
        details = function(index) {
          issues <- issues_info |>
            dplyr::filter(.data$variable == report_table$variable[index]) |>
            dplyr::select(-.data$variable) |>
            dplyr::rename("{attr(completeness_table, 'id_var')}" := .data$ids)
          if (nrow(issues) != 0) {
            htmltools::div(
              style = "padding: 1rem",
              reactable::reactable(
                issues,
                columns = list(
                  issue = reactable::colDef(width = 100),
                  n = reactable::colDef(width = 50)
                ), highlight = TRUE, wrap = TRUE
              )
            )
          }
        },
        columns = list(
          no = reactable::colDef(width = 65),
          variable = reactable::colDef(
            sticky = "left",
            resizable = TRUE,
            minWidth = min_col_width_var
          ),
          condition = reactable::colDef(show = FALSE),
          overall = reactable::colDef(show = FALSE),
          overall_color = reactable::colDef(show = FALSE),
          n_expected = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          n_missing = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              fill_color = wrong_bar_color,
              max_value = max(report_table$n_expected),
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          completeness = reactable::colDef(
            cell = reactablefmtr::data_bars(
              report_table,
              max_value = 1,
              fill_color_ref = "completeness_color",
              text_position = text_pos
            ),
            minWidth = min_col_width
          ),
          completeness_color = reactable::colDef(show = FALSE),
          n_unexpected = reactable::colDef(show = FALSE),
          n_antimissing = reactable::colDef(show = FALSE),
          uncompleteness = reactable::colDef(show = FALSE),
          uncompleteness_color = reactable::colDef(show = FALSE)
        ),
        highlight = TRUE, onClick = "expand",
        wrap = TRUE, pagination = FALSE, searchable = TRUE,
        theme = reactable::reactableTheme(
          borderWidth = 1, borderColor = opt_reactable$border_color
        )
      )
  }
}

# Helper function to report an ody_verify_conformance output
#   - conformance_table: output de verify_conformance.
report_conformance <- function(conformance_table) {
  distinct_formats_detail <- conformance_table$distinct_formats_detail

  conformance_table <- conformance_table |>
    dplyr::mutate(
      no = 1:dplyr::n(), .before = 1
    )

  conformance_table |>
    dplyr::select(-distinct_formats_detail) |>
    reactable::reactable(
      details = reactable::colDef(
        details = function(index) {
          htmltools::div(
            style = "padding: 1rem",
            reactable::reactable(
              distinct_formats_detail[[index]],
              columns = list(
                cases = reactable::colDef(minWidth = 150, resizable = TRUE),
                n = reactable::colDef(
                  minWidth = 200,
                  cell = reactablefmtr::data_bars(
                    distinct_formats_detail[[index]],
                    text_position = "above"
                  )
                )
              ),
              outlined = FALSE, fullWidth = FALSE, pagination = FALSE, highlight = TRUE
            )
          )
        }
      ),
      columns = list(
        no = reactable::colDef(width = 65),
        variable = reactable::colDef(minWidth = 200, resizable = TRUE),
        n_cases = reactable::colDef(maxWidth = 100),
        n_distinct = reactable::colDef(maxWidth = 100),
        uniqueness = reactable::colDef(maxWidth = 100),
        formats = reactable::colDef(minWidth = 200, resizable = TRUE),
        heterogeneity = reactable::colDef(maxWidth = 150),
        subformats = reactable::colDef(minWidth = 300, resizable = TRUE)
      ),
      borderless = TRUE, highlight = TRUE, onClick = "expand",
      wrap = TRUE, pagination = FALSE, searchable = TRUE
    )
}


#' Render a quality report
#'
#' @param project_name Name to use in the file name.
#' @param data_frame data frame to report.
#' @param missing_values vector with the values (apart from the regular NA and "") used as missing. Default is NULL.
#' @param project_date Date to show in the file name. Defaults to the day the report is run.
#' @param id_var ID variable.
#' @param conditions_list List of conditions used as argument of verify_completeness.
#' @param add_data If TRUE, raw data is added to the report.
#' @param max_integer_distinct ody_verify_conformance argument.
#' @param output_dir Location to save the report.
#'
#' @return A quality report HTML.
#' @export
ody_render_quality_report <- function(
    data_frame, project_name = "project", missing_values = "",
    project_date = {lubridate::today() |> stringr::str_remove_all("-")},
    id_var = "row_number", conditions_list = "no", add_data = FALSE,
    max_integer_distinct = 10, output_dir = getwd()) {
  parameters <- list(
    project_name = project_name,
    data = data_frame,
    missing_values = missing_values,
    conditions_list = conditions_list,
    id_var = id_var,
    max_integer_distinct = max_integer_distinct
  )

  report_name <- stringr::str_c(
    project_name, "_quality_", project_date, ".html"
  )

  if (add_data) {

    rlang::check_installed("DT")
    template <- system.file(
      "quality_reports", "quality_report_template_with_data.Rmd",
      package = "odytools"
    )
  } else {
    template <- system.file(
      "quality_reports", "quality_report_template.Rmd",
      package = "odytools"
    )
  }

  rmarkdown::render(
    template,
    params = parameters,
    output_dir = output_dir,
    output_file = report_name
  )
}
