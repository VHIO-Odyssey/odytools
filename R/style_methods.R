#' Apply "Odytools" styling to an object
#'
#' @description Adds the default style of "Odytools" to some objects.
#'
#' @param x An object usually a graphic or table.
#' @param style An integer indicating the style to apply. Default is 1.
#'
#' @details Suported classes are:
#'   - "gtsummary", 1 style.
#'   - "tbl_ae_focus", 2 styles:
#'     - 1: Adds "Any AE" row.
#'     - 2: Just applies the default "gtsummary" style.
#'
#' @returns The same object with the "Odytools" style applied. If the object is not supported, the object is returned as is.
#'
#' @export
ody_style <- function(x, style = 1) {

  UseMethod("ody_style")

}

#' @export
ody_style.default <- function(x) {

  message("No style method for object of class ", class(x))
  x

}

#' @export
ody_style.gtsummary <- function(x, style = 1) {

  rlang::check_installed("gtsummary")

  if (style == 1) {

  x |>
    gtsummary::bold_labels() |>
    gtsummary::as_gt() |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          weight = gt::px(2),
          color = "#C7C7C7"
        ),
        gt::cell_fill(color = "#F7F7F7")
      ),
      locations = gt::cells_body(rows = .data$row_type == "label")
    )

  } else {

    message("style ", style, " not implemented for gtsummary")
    x

  }

}

#' @export
ody_style.tbl_ae_focus <- function(x, style = 1) {

  rlang::check_installed("gtsummary")

  if (style == 1) {

    rlang::check_installed("gtreg")

    any_ae <- x$inputs$data |>
      dplyr::mutate("{x$inputs$ae}" := "Any AE") |>
      gtreg::tbl_ae_focus(
        include = x$inputs$include,
        id = x$inputs$id,
        id_df = x$inputs$id_df,
        ae = x$inputs$ae,
        strata = x$inputs$strata,
        label = x$inputs$label
      )

    gtsummary::tbl_stack(list(any_ae, x)) |>
      gtsummary::as_gt() |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_borders(
            sides = c("top", "bottom"),
            weight = gt::px(2),
            color = "#C7C7C7"
          ),
          gt::cell_fill(color = "#F7F7F7")
        ),
        locations = gt::cells_body(
          rows = .data$row_type == "label" | .data$label == "Any AE"
        )
      )


  } else if (style == 2) {

    NextMethod(x)

  } else {

    message("style ", style, " not implemented for tbl_ae_focus")
    x

  }

}
