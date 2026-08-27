test_that("nest_rc keeps forms/instances whose only content is a user missing code", {

  # Classic (no events, no repeating) synthetic import with three forms:
  # - capa1_form: every subject only has a user-defined missing code ("99")
  #   for its only field. This used to be wrongly dropped in full by the
  #   form/event-level emptiness check, because that check used base is.na(),
  #   which treats labelled_spss user missing codes as NA.
  # - capa2_form: subject 1 has real data, subject 2 has only a user missing
  #   code, and subject 3 has a genuine (regular) NA. Only subject 3's
  #   instance should be dropped by the row-level emptiness check.
  # - empty_form: every subject has a genuine (regular) NA. This form should
  #   still be dropped entirely (no regression from the fix).
  rc_raw <- tibble::tibble(
    record_id = c("1", "2", "3"),
    capa1_var = labelled::labelled_spss(
      c("99", "99", "99"),
      labels = c(No = "0", Yes = "1"),
      na_values = "99"
    ),
    capa2_var = labelled::labelled_spss(
      c("1", "99", NA_character_),
      labels = c(No = "0", Yes = "1"),
      na_values = "99"
    ),
    empty_var = labelled::labelled_spss(
      c(NA_character_, NA_character_, NA_character_),
      labels = c(No = "0", Yes = "1"),
      na_values = "99"
    )
  )

  metadata <- tibble::tibble(
    field_name = c("capa1_var", "capa2_var", "empty_var"),
    form_name = c("capa1_form", "capa2_form", "empty_form"),
    field_type = c("radio", "radio", "radio")
  )

  attr(rc_raw, "id_var") <- "record_id"
  attr(rc_raw, "metadata") <- metadata
  attr(rc_raw, "repeating") <- NULL

  result <- nest_rc(rc_raw) |>
    suppressMessages()

  # empty_form (real NAs only) is still dropped entirely.
  expect_false("empty_form" %in% result$redcap_form_name)

  # capa1_form (user missing codes only) is no longer dropped, and all three
  # subjects are kept since none of them is a regular NA.
  capa1_data <- result$redcap_form_data[[
    which(result$redcap_form_name == "capa1_form")
  ]]
  expect_equal(nrow(capa1_data), 3)
  expect_setequal(capa1_data$record_id, c("1", "2", "3"))

  # capa2_form keeps the real-data subject and the missing-code subject, but
  # drops the subject with a genuine regular NA.
  capa2_data <- result$redcap_form_data[[
    which(result$redcap_form_name == "capa2_form")
  ]]
  expect_equal(nrow(capa2_data), 2)
  expect_setequal(capa2_data$record_id, c("1", "2"))
})
