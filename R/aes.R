count_ae_max_grade <- function(ae_data, term = c("lower_term", "upper_term")) {

  term <- rlang::arg_match(term)

  ae_data_any <- ae_data |>
    dplyr::mutate(
      lower_term = "Any AE",
      upper_term = "Any AE"
    )

  max_grade_tbl <- dplyr::bind_rows(ae_data_any, ae_data) |>
    dplyr::group_by(.data$pac_id, .data[[term]]) |>
    dplyr::filter(.data$grade_num == max(.data$grade_num)) |>
    dplyr::select(
      all_of(c("pac_id", term, "grade_num", "grade_cat"))
    ) |>
    unique() |>
    dplyr::ungroup() |>
    dplyr::count(.data[[term]], .data$grade_cat, .data$grade_num) |>
    dplyr::group_by(.data[[term]], .data$grade_cat) |>
    dplyr::summarise(n = sum(.data$n)) |>
    tidyr::pivot_wider(
      names_from = "grade_cat",
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
    dplyr::select(!any_of("NA")) |>
    dplyr::arrange(desc(.data$`Any Grade`))

  if (term == "lower_term") {

    dplyr::left_join(
      max_grade_tbl,
      ae_data |>
        dplyr::select("lower_term", "upper_term") |>
        unique(),
      by = "lower_term"
    ) |>
      dplyr::relocate("upper_term", .before = 1) |>
      dplyr::mutate(count_term = "lower", .before = 1)

  } else {

    max_grade_tbl |>
      dplyr::mutate(count_term = "upper", .before = 1)

  }

}
