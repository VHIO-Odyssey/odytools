test_that("ody_qlq_c30_v3 calcultaes right scales", {

  # Create a dummy questionare data table
  n <- 50
  set.seed(88751)
  qs <- purrr::map_dfc(
    1:30,
    function(x) {
      if (x <= 28) {
        tibble::tibble(
          "q{x}" := sample(1:4, n, replace = TRUE)
        )
      } else {
        tibble::tibble(
          "q{x}" := sample(1:7, n, replace = TRUE)
        )
      }
    }
  )

  # Independent scales calculation
  result_expected <-
    tibble::tibble(
      QL2 = apply(qs[29:30], 1, function(x) (mean(x) - 1) / 6 * 100),
      PF2 = apply(qs[1:5], 1, function(x) (1 - (mean(x) - 1) / 3) * 100),
      RF2 = apply(qs[6:7], 1, function(x) (1 - (mean(x) - 1) / 3) * 100),
      EF  = apply(qs[21:24], 1, function(x) (1 - (mean(x) - 1) / 3) * 100),
      CF  = apply(qs[c(20, 25)], 1, function(x) (1 - (mean(x) - 1) / 3) * 100),
      SF  = apply(qs[26:27], 1, function(x) (1 - (mean(x) - 1) / 3) * 100),
      FA  = apply(qs[c(10, 12, 18)], 1, function(x) (mean(x) - 1) / 3 * 100),
      NV  = apply(qs[14:15], 1, function(x) (mean(x) - 1) / 3 * 100),
      PA  = apply(qs[c(9, 19)], 1, function(x) (mean(x) - 1) / 3 * 100),
      DY  = apply(qs[8], 1, function(x) (mean(x) - 1) / 3 * 100),
      SL  = apply(qs[11], 1, function(x) (mean(x) - 1) / 3 * 100),
      AP  = apply(qs[13], 1, function(x) (mean(x) - 1) / 3 * 100),
      CO  = apply(qs[16], 1, function(x) (mean(x) - 1) / 3 * 100),
      DI  = apply(qs[17], 1, function(x) (mean(x) - 1) / 3 * 100),
      FI  = apply(qs[28], 1, function(x) (mean(x) - 1) / 3 * 100)
    )

  # Calculate scales with ody_qlq_c30_v3
  result <-
    ody_qlq_c30_v3(qs) |>
    dplyr::select(QL2:FI)
  labelled::var_label(result) <- NULL
  attr(result, "item_mapping") <- NULL

  expect_equal(result, result_expected)

})
