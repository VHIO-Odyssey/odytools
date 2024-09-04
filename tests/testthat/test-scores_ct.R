test_that("ody_qlq_c30_v3 calcultates scales as expected", {

  # Create a dummy questionare data table
  n <- 20
  set.seed(88751)
  qs <-
    purrr::map_dfc(
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

  # Character version must also work
  qs_char <- qs |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Factor version must also work
  qs_factor <- qs |>
    dplyr::mutate(
      dplyr::across(
        q1:q28,
        ~factor(
          .,
          levels = 1:4,
          labels = c("Not at all", "A little", "Quite a bit", "Very much")
        )
      ),
      dplyr::across(q29:q30, ~factor(., levels = 1:7))
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

  result_char <-
    ody_qlq_c30_v3(qs_char) |>
    dplyr::select(QL2:FI)
  labelled::var_label(result_char) <- NULL
  attr(result_char, "item_mapping") <- NULL

  result_factor <-
    ody_qlq_c30_v3(qs_factor) |>
    dplyr::select(QL2:FI)
  labelled::var_label(result_factor) <- NULL
  attr(result_factor, "item_mapping") <- NULL

  # Both independent scales calculation and ody_qlq_c30_v3 should return the same result
  # Input as numeric
  expect_equal(result, result_expected)
  # Input as character
  expect_equal(result_char, result_expected)
  # Input as factor
  expect_equal(result_factor, result_expected)

})

test_that("ody_qlq_c30_v3 calculates deltas as expected", {

  # Create a dummy questionare data table with a group variable
  n <- 20
  set.seed(83938)
  qs <-
    purrr::map_dfc(
      1:31,
      function(x) {
        if (x <= 28) {
          tibble::tibble(
            "q{x}" := sample(1:4, n, replace = TRUE)
          )
        } else if (x <= 30) {
          tibble::tibble(
            "q{x}" := sample(1:7, n, replace = TRUE)
          )
        } else{
          tibble::tibble(
            group = rep(letters[1:4], each = n / 4)
          )
        }
      }
    )

  result_expected <-
    tibble::tibble(
      group = qs$group,
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
    ) |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      dplyr::across(
        QL2:FI,
        ~ . - dplyr::first(.),
        .names = "{.col}_delta"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(tidyselect::ends_with("_delta"))



  result <-
    ody_qlq_c30_v3(qs, group_delta = "group", items_index = 1:30) |>
    dplyr::select(tidyselect::ends_with("_delta"))
  attr(result, "item_mapping") <- NULL

  expect_equal(result, result_expected)

})


test_that("qlq_c30_scores_scale handles missing values as expected", {

  # Expectation when completeness = "all"
  # all items must be present
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, 4, 4), 1:3, 3, qlq_c30_scores_transform, "all")
    )
  )
  expect_true(
    is.na(
      qlq_c30_scores_scale(c(4, 4, NA), 1:3, 3, qlq_c30_scores_transform, "all")
    )
  )
  expect_true(
    is.na(
      qlq_c30_scores_scale(c(4, 4, NA, NA), 1:4, 3, qlq_c30_scores_transform, "all")
    )
  )
  expect_true(
    is.na(
      qlq_c30_scores_scale(c(4, NA, NA), 1:3, 3, qlq_c30_scores_transform, "all")
    )
  )
  expect_true(
    is.na(
      qlq_c30_scores_scale(c(NA, NA, NA), 1:3, 3, qlq_c30_scores_transform, "all")
    )
  )

  # Expectation when completeness = "half"
  # at least half of the items must be present
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, 4, 4), 1:3, 3, qlq_c30_scores_transform, "half")
    )
  )
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, 4, NA), 1:3, 3, qlq_c30_scores_transform, "half")
    )
  )
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, 4, NA, NA), 1:4, 3, qlq_c30_scores_transform, "half")
    )
  )
  expect_true(
    is.na(
      qlq_c30_scores_scale(c(4, NA, NA), 1:3, 3, qlq_c30_scores_transform, "half")
    )
  )
  expect_true(
    is.na(
      qlq_c30_scores_scale(c(NA, NA, NA), 1:3, 3, qlq_c30_scores_transform, "half")
    )
  )

  # Expectation when completeness = "none"
  # Only one item is required to calculate the scale
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, 4, 4), 1:3, 3, qlq_c30_scores_transform, "none")
    )
  )
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, 4, NA), 1:3, 3, qlq_c30_scores_transform, "none")
    )
  )
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, 4, NA, NA), 1:4, 3, qlq_c30_scores_transform, "none")
    )
  )
  expect_true(
    !is.na(
      qlq_c30_scores_scale(c(4, NA, NA), 1:3, 3, qlq_c30_scores_transform, "none")
    )
  )
  expect_true(
    is.na(
      qlq_c30_scores_scale(c(NA, NA, NA), 1:3, 3, qlq_c30_scores_transform, "none")
    )
  )

})

test_that("ody_qlq_c30_v3 handles the input data.frame propperly", {

  # test data frame.
  # cols 1 to 30 have invalid values for the QLQ-C30
  # cols 31 to 60 have valid values for the QLQ-C30
  n <- 5
  set.seed(24401)
  qs <-
    purrr::map_dfc(
      1:60,
      function(x) {
        if (x <= 15) {
          tibble::tibble(
            "q{x}" := sample(letters, n, replace = TRUE)
          )
        } else if (x <= 30) {
          tibble::tibble(
            "q{x}" := sample(1:100, n, replace = TRUE)
          )
        } else if (x <= 58) {
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


  # Expect error if the input is not a data frame
  expect_error(ody_qlq_c30_v3(2), "qlq_c30_df must be a data frame")
  expect_error(ody_qlq_c30_v3(1:5), "qlq_c30_df must be a data frame")
  expect_error(ody_qlq_c30_v3("a"), "qlq_c30_df must be a data frame")

  # Expect error if the data frame has less than 30 columns
  expect_error(
    ody_qlq_c30_v3(qs[1:5]),
    "The provided data frame must have at least 30 columns"
  )

  # If the data frame has more than 30 columns, expect using the last 30 columns
  expect_equal(ody_qlq_c30_v3(qs), ody_qlq_c30_v3(qs, items_index = 31:60))

  # Items index must have 30 elements
  expect_error(
    ody_qlq_c30_v3(qs, items_index = 1:25),
    "The items_index argument must have 30 elements"
  )
  expect_error(
    ody_qlq_c30_v3(qs, items_index = 1:35),
    "The items_index argument must have 30 elements"
  )

  # Items index must be in the range of the data frame
  expect_error(
    ody_qlq_c30_v3(qs, items_index = 61:90),
    "Item indexes out of range"
  )

  # Expect error if the data frame has invalid values
  expect_error(
    ody_qlq_c30_v3(qs[1:30]),
    "Unexpected values in: q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28, q29, q30"
  )

})
