#' Extract KM table from a survfit object
#'
#' @param km_fit KM model
#' @param gt_table logic, returmn the table as a gt?
#' @param n_dec Number of decimals
#'
#' @export
ody_extract_km <- function(km_fit, gt_table = FALSE, n_dec = 1) {

  km_data <- summary(km_fit)$table

  if (is.null(nrow(km_data))) {

    output <- tibble::tibble(
      N = km_data[1],
      Events = km_data[4],
      Median = km_data[7] |> round(n_dec),
      `0.95LCL` = km_data[8] |> round(n_dec),
      `0.95UCL` = km_data[9] |> round(n_dec)
    )
  } else {

    output <- tibble::tibble(
      N = km_data[ ,1],
      Events = km_data[ ,4],
      Median = km_data[ ,7] |> round(n_dec),
      `0.95LCL` = km_data[ ,8] |> round(n_dec),
      `0.95UCL` = km_data[ ,9] |> round(n_dec)
    )

    output <- output |>
      dplyr::mutate(
        Level = rownames(km_data),
        .before = 1
      )
  }

  if (gt_table) {
    gt::gt(output) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      )
  } else {
    output
  }

}
