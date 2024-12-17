#' Apply "Odytools" styling to an object
#'
#' @description Modifies the style of tables and graphics.
#'
#' @param x An object, usually a graphic or table.
#' @param style An integer indicating the style to apply. Default is 1.
#'
#' @details Suported classes are:
#'   - "gtsummary", 1 style.
#'   - "tbl_ae_focus", 2 styles:
#'     - 1: Adds "Any AE" row.
#'     - 2: Just applies the default "gtsummary" style.
#'
#'   - "ggsurvfit", 2 styles:
#'     - 1: Standard style. With number at risk, censored and events
#'     - 2: Enriched style with estimates
#'     - 3: Compact style.
#'
#' @returns The same object with the "Odytools" style applied. If the object is not supported, the object is returned as is.
#'
#' @export
ody_style <- function(x, style = 1) {

  UseMethod("ody_style")

}

#' @export
ody_style.default <- function(x, style = 1) {

  warning("No style method for object of class ", class(x))
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

    warning("style ", style, " not implemented for gtsummary")
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

    warning("style ", style, " not implemented for tbl_ae_focus")
    x

  }

}

#' @export
ody_style.ggsurvfit <- function(x, style = 1) {

  rlang::check_installed("ggsurvfit")

  if (style == 1) {

    x +
      ggsurvfit::add_censor_mark() +
      ggsurvfit::add_risktable(
        risktable_stats = c("n.risk", "cum.event", "cum.censor")
      ) +
      ggsurvfit::add_quantile(
        y_value = 0.5, color = "gray50", linewidth = 0.75) +
      ggsurvfit::scale_ggsurvfit() +
      ggplot2::theme(
        legend.position = "top",
        panel.border = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank()

      )

  } else if (style == 2) {

    x +
      ggsurvfit::add_censor_mark() +
      ggsurvfit::add_risktable(
        risktable_stats = c(
          "n.risk",
          "{cum.event} ({cum.censor})",
          "{round(estimate*100)}% ({round(conf.low*100)}, {round(conf.high*100)})"
        ),
        stats_label = c("At Risk", "Events (Cens.)", "Estimate (95% CI)")
      ) +
      ggsurvfit::add_quantile(
        y_value = 0.5, color = "gray50", linewidth = 0.75) +
      ggsurvfit::scale_ggsurvfit(x_scales = list(expand = c(0.04, 0))) +
      ggplot2::theme(
        legend.position = "top",
        panel.border = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank()
      )

    # x +
    #   ggsurvfit::add_censor_mark() +
    #   ggsurvfit::add_risktable(
    #     risktable_stats =
    #       c("{round(estimate*100)}%", "{n.risk} ({cum.event})"),
    #     stats_label = c("Estimate %", "At Risk (Censored)"),
    #     risktable_group = "risktable_stats"
    #   ) +
    #   ggsurvfit::add_risktable_strata_symbol(symbol = "\U25CF") +
    #   ggsurvfit::add_quantile(
    #     y_value = 0.5, color = "gray50", linewidth = 0.75) +
    #   ggsurvfit::scale_ggsurvfit() +
    #   ggplot2::theme(legend.position = "top")

  } else if (style == 3) {

    x +
      ggsurvfit::add_censor_mark() +
      ggsurvfit::add_risktable(
        risktable_stats = c(
          "{n.risk} ({cum.censor})"
        ),
        stats_label = "At Risk (Censored)"
      ) +
      ggsurvfit::add_quantile(
        y_value = 0.5, color = "gray50", linewidth = 0.75) +
      ggsurvfit::scale_ggsurvfit(x_scales = list(expand = c(0.04, 0))) +
      add_risktable_strata_symbol(symbol = "\U25CF", size = 10)
    ggplot2::theme(
      legend.position = "top",
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )

  } else {

    warning("style ", style, " not implemented for ggsurvfit")
    x

  }

}
