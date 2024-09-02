#' @importFrom stats formula
NULL

#' Plot Dots, Boxplot, and Violin Plot
#'
#' This function generates a composite plot that includes a half violin plot, a half dot plot,
#' and a boxplot. It also adds statistical comparisons between groups.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x A string representing the name of the x variable (categorical).
#' @param y A string representing the name of the y variable (numeric).
#' @param no_violin Logical indicating if the half violin plot should be removed.
#' @param compare Logical indicating if a statistical comparison should be added.
#' @param p_adj A string indicating the method to adjust the p-values. See \link[stats]{p.adjust}.
#' @param brackets_pos A numeric value indicating the relative position of the brackets that represent the statistical comparison.
#' @param ... Additional arguments to be passed to \link[ggpubr]{compare_means} (the function
#' used to compare the means) and to \link[ggpubr]{geom_bracket} (the function used to add the brackets).
#' @return A ggplot object that can be further customized.
#' @export
ody_plot_violindotbox <- function(
    data, x, y,
    no_violin = FALSE,
    compare = FALSE,
    p_adj = "fdr",
    brackets_pos = 1.05,
    ...) {

  rlang::check_installed(c("ggpubr", "gghalves"))

  if (!is.factor(data[[x]])) {

    data[[x]] <- factor(data[[x]])

  }

  if (no_violin) {

    p <-
      ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[y]])) +
      ggplot2::geom_boxplot(outliers = FALSE) +
      ggplot2::geom_jitter(alpha = 0.5)

  } else {

    p <-
      ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[y]])) +
      gghalves::geom_half_violin(side = "r") +
      ggplot2::geom_boxplot(width = 0.1, outliers = FALSE, fill = "gray") +
      gghalves::geom_half_point(side = "l", alpha = 0.5)


  }
  if (compare) {

    stats <-
      ggpubr::compare_means(
        formula(glue::glue("{y} ~ {x}")),
        data = data, ...
      )

    y_pos <- max(data |> dplyr::pull(.data[[y]]), na.rm = TRUE) * brackets_pos

    p <-
      p +
      ggpubr::geom_bracket(
        data = stats,
        ggplot2::aes(
          xmin = .data[["group1"]], xmax = .data[["group2"]],
          label = gtsummary::style_pvalue(.data[["p.adj"]])
        ),
        y.position = y_pos, ...#step.increase = 0.07, label.size = 3,  tip.length = 0
      )

  }

  p

}
