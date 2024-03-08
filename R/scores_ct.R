# Función para calcular los scores de cada escala del EORTC QLQ-C30

# QLQ-C30 transformation functons
qlq_c30_scores_transform <- function(rs, range) ((rs - 1) / range) * 100

qlq_c30_scores_transform_1 <- function(rs, range) (1 - (rs - 1) / range) * 100

qlq_c30_scores_scale <- function(qlq_c30, items, range, transformation) {

  if (all(is.na(qlq_c30[items]))) return(NA_real_)

  qlq_c30[items] |>
    as.numeric() |>
    mean(na.rm = TRUE) |>
    transformation(range)
}

qlq_c30_scores <- function(qlq_c30, scoring_tbl) {

  purrr::pmap(
    scoring_tbl,
    function(scale, items, range, transformation) {
      tibble::tibble(
        "{stringr::str_to_upper(scale)}" := qlq_c30_scores_scale(qlq_c30, items, range, transformation)
      )
    }
  ) |>
    purrr::reduce(dplyr::bind_cols)

}

ody_qlq_c30_v3 <- function(qlq_c30) {

  if (ncol(qlq_c30) != 30) stop("qlq_c30 must have 30 columns")

  scoring_tbl <- tibble::tribble(
    ~scale, ~items, ~range, ~transformation,
    "ql2", 29:30, 6, qlq_c30_scores_transform,
    "pf2", 1:5, 3, qlq_c30_scores_transform_1,
    "rf2", 6:7, 3, qlq_c30_scores_transform_1,
    "ef", 21:24, 3, qlq_c30_scores_transform_1,
    "cf", c(20, 25), 3, qlq_c30_scores_transform_1,
    "sf", 26:27, 3, qlq_c30_scores_transform_1,
    "fa", c(10, 12, 18), 3, qlq_c30_scores_transform,
    "nv", 14:15, 3, qlq_c30_scores_transform,
    "pa", c(9, 19), 3, qlq_c30_scores_transform,
    "dy", 8, 3, qlq_c30_scores_transform,
    "sl", 11, 3, qlq_c30_scores_transform,
    "ap", 13, 3, qlq_c30_scores_transform,
    "co", 16, 3, qlq_c30_scores_transform,
    "di", 17, 3, qlq_c30_scores_transform,
    "fi", 28, 3, qlq_c30_scores_transform
  )

  purrr::map_dfr(
    as.data.frame(t(qlq_c30)), ~qlq_c30_scores(., scoring_tbl),
    .progress = "Scoring"
  )

}

