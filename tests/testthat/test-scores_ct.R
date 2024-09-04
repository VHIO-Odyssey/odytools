test_that("ody_qlq_c30_v3 calcultaes right scales", {

  all_max <-
    c(rep(4, 28), rep(7, 2)) |>
    tibble::enframe() |>
    tidyr::pivot_wider()
  all_max_result <-
    ody_qlq_c30_v3(all_max) |>
    dplyr::select(QL2:FI) |>
    unlist() |>
    unname()
  all_max_expected <- c(100, rep(0, 5), rep(100, 9))
  expect_equal(all_max_result, all_max_expected)

  all_min <-
    rep(1, 30) |>
    tibble::enframe() |>
    tidyr::pivot_wider()
  all_min_result <-
    ody_qlq_c30_v3(all_min) |>
    dplyr::select(QL2:FI) |>
    unlist() |>
    unname()
  all_min_expected <- c(0, rep(100, 5), rep(0, 9))
  expect_equal(all_min_result, all_min_expected)

})
