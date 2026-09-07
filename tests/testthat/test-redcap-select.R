# Synthetic nested imports (classic and longitudinal) shared by these tests.
# Both have:
# - demographics: a form with data (demo_age, demo_sex).
# - empty_form: a form whose fields (empty_var, empty_check) only have
#   missing values, so it is dropped when nesting (no data yet).
# - demo_note: a descriptive field of demographics, present in the metadata
#   but without a column in the import (phantom variable).

select_test_metadata <- function() {
  tibble::tibble(
    field_name = c(
      "demo_age",
      "demo_sex",
      "demo_note",
      "empty_var",
      "empty_check"
    ),
    form_name = c(
      "demographics",
      "demographics",
      "demographics",
      "empty_form",
      "empty_form"
    ),
    field_type = c("text", "radio", "descriptive", "text", "checkbox")
  )
}

add_select_test_attributes <- function(rc_data) {
  attr(rc_data, "id_var") <- "record_id"
  attr(rc_data, "metadata") <- select_test_metadata()
  attr(rc_data, "checkbox_aux") <- c("empty_check___1", "empty_check___2")
  attr(rc_data, "phantom_variables") <- tibble::tibble(
    field_name = "demo_note",
    field_type = "descriptive",
    form_name = "demographics"
  )
  attr(rc_data, "subjects") <- c("1", "2", "3")

  rc_data
}

make_classic_import <- function() {
  rc_raw <- tibble::tibble(
    record_id = c("1", "2", "3"),
    demo_age = c("25", "30", "35"),
    demo_sex = c("1", "2", "1"),
    empty_var = c(NA_character_, NA_character_, NA_character_),
    empty_check___1 = c(NA_character_, NA_character_, NA_character_),
    empty_check___2 = c(NA_character_, NA_character_, NA_character_)
  )

  attr(rc_raw, "id_var") <- "record_id"
  attr(rc_raw, "metadata") <- select_test_metadata()
  attr(rc_raw, "repeating") <- NULL

  nest_rc(rc_raw) |>
    suppressMessages() |>
    add_select_test_attributes()
}

make_long_import <- function() {
  rc_raw <- tibble::tibble(
    record_id = c("1", "2", "1", "2"),
    redcap_event_name = c("baseline", "baseline", "followup", "followup"),
    redcap_repeat_instrument = rep(NA_character_, 4),
    redcap_repeat_instance = rep(NA_real_, 4),
    demo_age = c("25", "30", "41", "52"),
    demo_sex = c("1", "2", "2", "1"),
    empty_var = rep(NA_character_, 4),
    empty_check___1 = rep(NA_character_, 4),
    empty_check___2 = rep(NA_character_, 4)
  )

  attr(rc_raw, "id_var") <- "record_id"
  attr(rc_raw, "metadata") <- select_test_metadata()
  attr(rc_raw, "repeating") <- tibble::tibble(
    event_name = character(0),
    form_name = character(0)
  )

  rc_data <- nest_rc(rc_raw) |>
    suppressMessages() |>
    add_select_test_attributes()

  # demographics appears in two events; empty_form in a single one.
  attr(rc_data, "forms_events_mapping") <- tibble::tibble(
    form = c("demographics", "demographics", "empty_form"),
    unique_event_name = c("baseline", "followup", "baseline")
  )

  rc_data
}

test_that("a variable of a form without data warns and returns an empty tibble (classic)", {
  rc_data <- make_classic_import()

  expect_warning(
    selection <- ody_rc_select(rc_data, empty_var),
    "has no data in this import yet"
  )

  expect_equal(nrow(selection), 0)
  expect_named(
    selection,
    c(
      "record_id",
      "redcap_form_name",
      "redcap_instance_type",
      "redcap_instance_number",
      "empty_var"
    )
  )
  expect_type(selection$record_id, "character")
  expect_type(selection$empty_var, "character")
})

test_that("a checkbox auxiliary of a form without data returns a logical empty column", {
  rc_data <- make_classic_import()

  expect_warning(
    selection <- ody_rc_select(rc_data, empty_check___1),
    "has no data in this import yet"
  )

  expect_equal(nrow(selection), 0)
  expect_type(selection$empty_check___1, "logical")
})

test_that("a variable of a form without data warns and returns an empty tibble (longitudinal)", {
  rc_data <- make_long_import()

  expect_warning(
    selection <- ody_rc_select(rc_data, empty_var),
    "has no data in this import yet"
  )

  expect_equal(nrow(selection), 0)
  expect_named(
    selection,
    c(
      "record_id",
      "redcap_event_name",
      "redcap_form_name",
      "redcap_instance_type",
      "redcap_instance_number",
      "empty_var"
    )
  )
})

test_that("a phantom variable warns and returns an empty tibble", {
  rc_data <- make_classic_import()

  expect_warning(
    selection <- ody_rc_select(rc_data, demo_note),
    "has no column in the import"
  )

  expect_equal(nrow(selection), 0)
  expect_named(
    selection,
    c(
      "record_id",
      "redcap_form_name",
      "redcap_instance_type",
      "redcap_instance_number",
      "demo_note"
    )
  )
})

test_that("a nonexistent variable still errors", {
  rc_data <- make_classic_import()

  expect_error(
    ody_rc_select(rc_data, not_a_var),
    "does not exist"
  )
})

test_that("the join path adds an all-NA column for a variable of a form without data", {
  rc_data <- make_classic_import()

  expect_warning(
    selection <- ody_rc_select(
      rc_data,
      demo_age,
      empty_var,
      .if_different_forms = "join"
    ),
    "has no data in this import yet"
  )

  expect_equal(nrow(selection), 3)
  expect_setequal(selection$record_id, c("1", "2", "3"))
  expect_setequal(selection$demo_age, c("25", "30", "35"))
  expect_true(all(is.na(selection$empty_var)))
})

test_that("the list path groups empty selections under their metadata form", {
  rc_data <- make_classic_import()

  expect_warning(
    selection <- ody_rc_select(
      rc_data,
      demo_age,
      empty_var,
      .if_different_forms = "list"
    ),
    "has no data in this import yet"
  )

  expect_type(selection, "list")
  expect_setequal(names(selection), c("demographics", "empty_form"))
  expect_equal(nrow(selection$demographics), 3)
  expect_equal(nrow(selection$empty_form), 0)
})

test_that("selecting a form without data by name warns once and returns an empty tibble", {
  rc_data <- make_classic_import()

  expect_warning(
    selection <- ody_rc_select(rc_data, empty_form),
    'Form "empty_form" has no data in this import yet'
  )

  expect_equal(nrow(selection), 0)
  expect_equal(ncol(selection), 0)
})

test_that("normal selections are unchanged", {
  rc_data <- make_classic_import()

  expect_no_warning(selection <- ody_rc_select(rc_data, demo_age))
  expect_equal(nrow(selection), 3)
  expect_setequal(selection$demo_age, c("25", "30", "35"))

  rc_data_long <- make_long_import()

  expect_no_warning(selection_long <- ody_rc_select(rc_data_long, demo_age))
  expect_equal(nrow(selection_long), 4)
  expect_setequal(selection_long$demo_age, c("25", "30", "41", "52"))
})

test_that("simplify_selection2 simplifies empty list entries using their form name", {
  rc_data <- make_long_import()

  selection <- ody_rc_select(rc_data, demo_age, empty_var) |>
    suppressWarnings()

  simplified <- ody_rc_simplify_selection2(selection)

  # empty_form belongs to a single event, so redcap_event_name is dropped
  # from the design metadata; redcap_instance_number is always kept when
  # there is no data.
  expect_named(
    simplified$empty_form,
    c("record_id", "redcap_instance_number", "empty_var")
  )
  expect_equal(nrow(simplified$empty_form), 0)

  # demographics belongs to two events, so nothing but redcap_form_name and
  # redcap_instance_type is dropped.
  expect_named(
    simplified$demographics,
    c(
      "record_id",
      "redcap_event_name",
      "redcap_instance_number",
      "demo_age"
    )
  )
})

test_that("simplify_selection2 with join = TRUE works with empty selections", {
  rc_data <- make_long_import()

  selection <- ody_rc_select(rc_data, demo_age, empty_var) |>
    suppressWarnings()

  simplified <- ody_rc_simplify_selection2(selection, join = TRUE) |>
    suppressMessages()

  expect_equal(nrow(simplified), 4)
  expect_true(all(is.na(simplified$empty_var)))
  expect_setequal(simplified$demo_age, c("25", "30", "41", "52"))
})

test_that("simplify_selection2 conservatively keeps structure in bare empty selections", {
  rc_data <- make_long_import()

  # A single no-data variable returns a bare 0-row tibble whose origin form
  # cannot be determined, so redcap_event_name and redcap_instance_number
  # must be kept.
  selection <- ody_rc_select(rc_data, empty_var) |>
    suppressWarnings()

  simplified <- ody_rc_simplify_selection2(selection)

  expect_named(
    simplified,
    c(
      "record_id",
      "redcap_event_name",
      "redcap_instance_number",
      "empty_var"
    )
  )
  expect_equal(nrow(simplified), 0)
})

test_that("simplify_selection2 works on empty selections of classic projects", {
  rc_data <- make_classic_import()

  selection <- ody_rc_select(rc_data, empty_var) |>
    suppressWarnings()

  simplified <- ody_rc_simplify_selection2(selection)

  expect_named(
    simplified,
    c("record_id", "redcap_instance_number", "empty_var")
  )
  expect_equal(nrow(simplified), 0)
})
