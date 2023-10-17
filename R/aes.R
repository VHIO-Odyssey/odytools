# Helper function to count the number of patients at each maximum grade.
# It assumes some col names (pac_id, grade_num, grade_fct)
count_ae_max_grade <- function(ae_data, term) {

  ae_data_any <- ae_data |>
    dplyr::mutate(
      {{term}} := "Any Adverse Event"
    )

  dplyr::bind_rows(ae_data_any, ae_data) |>
    dplyr::group_by(.data$pac_id, {{term}}) |>
    dplyr::filter(.data$grade_num == max(.data$grade_num, na.rm = TRUE)) |>
    dplyr::select("pac_id", {{term}}, "grade_num", "grade_fct") |>
    unique() |>
    dplyr::ungroup() |>
    dplyr::count({{term}}, .data$grade_fct, .data$grade_num) |>
    dplyr::group_by({{term}}, .data$grade_fct) |>
    dplyr::summarise(n = sum(.data$n)) |>
    tidyr::pivot_wider(
      names_from = "grade_fct",
      values_from = "n",
      names_sort = TRUE
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.integer), ~tidyr::replace_na(., 0)),
      `Any Grade` = dplyr::c_across(dplyr::where(is.integer)) |> sum(),
      .after = 1
    ) |>
    dplyr::ungroup() |>
    dplyr::select(!dplyr::any_of("NA")) |>
    dplyr::arrange(dplyr::desc(.data$`Any Grade`))

}

# Helper intermediate function that takes care of making the right table
# depending on the presence of an upper_term.
make_ae_tbl <- function(
    ae_data,
    pac_id,
    grade_num,
    grade_fct,
    term,
    upper_term = NULL) {

  # rename variables to the assumed names by count_ae_max_grade
  ae_data <- ae_data |>
    dplyr::rename(
      pac_id = {{pac_id}},
      grade_num = {{grade_num}},
      grade_fct = {{grade_fct}}
    )

  term <- rlang::enquo(term)
  upper_term <- rlang::enquo(upper_term)

  # Term table, if no upper_term this the returned output
  term_tbl <- count_ae_max_grade(ae_data, !!term)

  if (rlang::quo_is_null(upper_term)) return(term_tbl)

  # If the function arrives here, it means there is an upper_term

  # Relation table of the terms with the upper terms
  terms_relation <- ae_data |>
    dplyr::select({{upper_term}}, {{term}}) |>
    unique()

  # According to terms_relation, we join each term with its upper terms
  term_tbl_with_upper <- dplyr::left_join(
    # Any adverse Event row is removed since upper_term_tbl also contains it.
    term_tbl |> dplyr::filter({{term}} != "Any Adverse Event"),
    terms_relation
  ) |> suppressMessages()

  # Upper temrs table
  upper_term_tbl <- count_ae_max_grade(ae_data, !!upper_term)

  # Row binding of he trems and upper terms table
  binded_table <- dplyr::bind_rows(upper_term_tbl, term_tbl_with_upper) |>
    dplyr::relocate({{term}}, .after = 1)


  # The final table is ordered so most common upper_terms (in number of
  # patients) go first.
  purrr::map_dfr(
    upper_term_tbl |> dplyr::pull(!!upper_term),
    ~binded_table |> dplyr::filter(!!upper_term == .)
  )

}

#' Make an Adverse Events Table
#'
#' @param ae_data Data frame of Adverse Events
#' @param pac_id Patients identrifier column in ae_data
#' @param grade_num Numeric column indicating the grade of each AE
#' @param grade_fct Factor column indicating how to group the AEs in the output table.
#' @param term Charachter column with the AE name.
#' @param upper_term Optional, a higher level name.
#' @param group_fct Optional, additional grouping factor.
#' @param add_overall If there is a group_fct, add_overall = TRUE adds an overall count to the by group_fct list.
#'
#' @return A tibble if group_fct = NULL. Otherwise, a list of tibbles
#' @export
ody_make_ae_tbl <- function(
  ae_data,
  pac_id,
  grade_num,
  grade_fct,
  term,
  upper_term = NULL,
  group_fct = NULL,
  add_overall = FALSE) {

  grade_fct <- rlang::enquo(grade_fct)

  is_factor <- ae_data |>
    dplyr::pull(!!grade_fct) |>
    is.factor()

  if (!is_factor) stop(rlang::as_name(grade_fct), " must be a factor.")

  group_fct <- rlang::enquo(group_fct)

  full_table <- make_ae_tbl(
    ae_data,
    {{pac_id}}, {{grade_num}}, {{grade_fct}},
    {{term}}, {{upper_term}}
  )

  if (rlang::quo_is_null(group_fct)) {
    return(full_table)
  }

  is_factor <- ae_data |>
    dplyr::pull(!!group_fct) |>
    is.factor()

  if (!is_factor) stop(rlang::as_name(group_fct), " must be a factor.")

  group_levels <- ae_data |>
    dplyr::pull(!!group_fct) |>
    levels()

  by_var_tables <- purrr::map(
    group_levels,
    ~make_ae_tbl(
      ae_data |> dplyr::filter(!!group_fct == .),
      {{pac_id}}, {{grade_num}}, {{grade_fct}},
      {{term}}, {{upper_term}}
    )
  )

  if (add_overall) {

    all_tables <- c(list(full_table), by_var_tables)
    names(all_tables) <- c(
      "overall",
      stringr::str_c(rlang::as_name(group_fct),"=", group_levels)
    )
    all_tables

  } else {

    names(by_var_tables) <-stringr::str_c(
      rlang::as_name(group_fct),"=", group_levels
    )
    by_var_tables

  }
}


# Helper function to add a percentage to each value of each numeric column.
add_pct <- function(data_frame, n, dec = 1) {

  data_frame |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~stringr::str_c(., " (", round(100 * . / n, dec), "%)")
      )
    )

}


#' Make a gt table of Adverse Events
#'
#' @param ae_tbl Adverse Events table. The output of ody_make_ae_tbl.
#' @param total_n Optional, if presents, a percentage is added to the count
#' @param spanner_names Optional, if ae_tbl is a list, the names of the spanners on the table.
#' @param dec Number of decimals of the percentages.
#'
#' @return A gt table
#' @export
ody_make_ae_gt <- function(
    ae_tbl,
    total_n = NULL,
    spanner_names = NULL,
    dec = 1) {

  # If ae_tbl is a list, it must be joined
  if (!is.data.frame(ae_tbl)) {

    n_terms <- sum(purrr::map_lgl(ae_tbl[[1]], is.character))

    if (is.null(spanner_names)) {

      spanner_names <- names(ae_tbl)

    }

    if (!is.null(total_n)) {

      ae_tbl <- purrr::map2(ae_tbl, total_n, add_pct, dec)

    }

    ae_tbl_join <- purrr::map2(
      1:length(ae_tbl), spanner_names,
      function(x, y) {

        names(ae_tbl[[x]])[-(1:n_terms)] <- stringr::str_c(
          y, ";", names(ae_tbl[[x]])[-(1:n_terms)]
        )

        ae_tbl[[x]]

      }
    ) |>
      purrr::reduce(dplyr::full_join) |>
      suppressMessages()


    if (!is.null(total_n)) {

      ae_tbl_join <- ae_tbl_join |>
        dplyr::mutate(
          dplyr::across(
            dplyr::contains(";"),
            ~tidyr::replace_na(., "0 (0%)"))
        )

    } else {

      ae_tbl_join <- ae_tbl_join |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.integer), ~tidyr::replace_na(., 0))
        )

    }

  } else {
    n_terms <- sum(purrr::map_lgl(ae_tbl, is.character))

    ae_tbl_join <- ae_tbl

    if (!is.null(total_n)) {
      ae_tbl_join <- add_pct(ae_tbl_join, total_n, dec)
    }

  }


  if (n_terms == 1) {

    term_name <- names(ae_tbl_join)[1]

    gt_tbl <- ae_tbl_join |>
      gt::gt() |>
      gt::cols_label("{term_name}" := "") |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = list(
          gt::cells_column_labels(),
          gt::cells_body(rows = .data[[term_name]] =="Any Adverse Event")
        )
      ) |>
      gt::opt_row_striping()

  } else if (n_terms == 2){

    upper_term_name <- names(ae_tbl_join)[1]
    term_name <- names(ae_tbl_join)[2]

    gt_tbl <- ae_tbl_join |>
      dplyr::mutate(
        table_term = dplyr::case_when(
          is.na(.data[[term_name]]) ~ .data[[upper_term_name]],
          .default = .data[[term_name]]
        ), .before = 1
      ) |>
      gt::gt() |>
      gt::cols_label(table_term = "") |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill("gray90")
        ),
        locations = gt::cells_body(rows = is.na(.data[[term_name]]))
      ) |>
      gt::tab_style(
        style = gt::cell_text(indent = gt::pct(5)),
        locations = gt::cells_body(rows = !is.na(.data[[term_name]]))
      ) |>
      gt::cols_hide(dplyr::all_of(c(upper_term_name, term_name))) |>
      gt::opt_row_striping()

  }

  if (!is.data.frame(ae_tbl)) {

    gt_tbl |>
      gt::tab_spanner_delim(
        delim = ";"
      ) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_spanners()
      )

  } else {

    gt_tbl

  }

}

