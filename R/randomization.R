#' @title Build Stratified Block Randomization Lists
#' @description
#'   Generates treatment allocation lists using permuted blocks, optionally
#'   stratified by one or more factors.
#'
#'   The function expands all combinations in `rnd_strata_list`, generates a
#'   separate block-randomized sequence for each stratum with
#'   [blockrand::blockrand()], and returns a single tibble combining all
#'   strata-specific randomization schedules.
#'
#' @param n An integer scalar specifying the number of allocations to generate
#'   per stratum.
#' @param rnd_levels A character vector specifying the treatment arm labels
#'   passed to `levels` in [blockrand::blockrand()].
#' @param rnd_name A character scalar specifying the output column name for the
#'   assigned treatment. If `NULL` (default), the column is named
#'   `"treatment"`.
#' @param rnd_strata_list A named list of vectors defining stratification
#'   factors. Each element name becomes a column in the output and each vector
#'   provides the allowed levels for that factor. If `NULL` (default), no
#'   stratification is applied and a single randomization list is generated.
#' @param block_size An integer vector specifying the number of times each
#'   treatment level is represented within a single block. The resulting total
#'   block size is `block_size * length(rnd_levels)`. For example, with two
#'   arms (`A` and `B`), `block_size = 1` produces blocks of size 2 (1 A,
#'   1 B), and `block_size = 2` produces blocks of size 4 (2 A, 2 B). With
#'   three arms (`A`, `B`, `C`), `block_size = 1` produces blocks of size 3
#'   (1 A, 1 B, 1 C). When a vector is supplied (e.g., `block_size = c(1, 2)`),
#'   each block is randomly assigned one of the allowed sizes. This argument
#'   is passed as `block.sizes` to [blockrand::blockrand()].
#'
#' @return A tibble with one row per randomized allocation across all strata.
#'   Includes `block_id` and `block_size`, optional stratification columns from
#'   `rnd_strata_list`, and a treatment-assignment column (`treatment` by
#'   default, or `rnd_name` when provided).
#'
#'   The `block_id` values are character identifiers constructed by
#'   concatenating stratum values (if present) and the per-stratum block id.
#'
#' @details
#'   This function requires the `blockrand` package at runtime and checks for
#'   its availability with [rlang::check_installed()].
#'
#' @examples
#' ody_make_random_list(
#'   n = 12,
#'   rnd_levels = c("A", "B"),
#'   rnd_name = "arm",
#'   rnd_strata_list = list(site = c("S1", "S2"), sex = c("F", "M")),
#'   block_size = c(2, 4)
#' )
#'
#' @export
ody_make_random_list <- function(
  n,
  rnd_levels,
  rnd_name = NULL,
  rnd_strata_list = NULL,
  block_size
) {
  rlang::check_installed("blockrand")

  if (is.null(rnd_strata_list)) {
    strata_df <- tibble::tibble(dummy = 1)
  } else {
    strata_df <- tidyr::expand_grid(!!!rnd_strata_list)
  }

  rnd_list <-
    strata_df |>
    dplyr::mutate(
      randomization = purrr::map(
        1:nrow(strata_df),
        ~ blockrand::blockrand(
          n,
          levels = rnd_levels,
          block.sizes = block_size
        ) |>
          dplyr::select(block_id = block.id, block_size = block.size, treatment)
      )
    ) |>
    tidyr::unnest(cols = .data$randomization) |>
    dplyr::select(-dplyr::any_of("dummy")) |>
    dplyr::relocate("block_id", "block_size", .before = 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      block_id = dplyr::c_across(c(names(rnd_strata_list), "block_id")) |>
        stringr::str_c(collapse = "_")
    ) |>
    dplyr::ungroup()

  if (!is.null(rnd_name)) {
    rnd_list <- rnd_list |>
      dplyr::rename("{rnd_name}" := .data$treatment)
  }

  if (!is.null(rnd_strata_list)) {
    attr(rnd_list, "strata_vars") <- names(rnd_strata_list)

    rnd_list <- rnd_list |>
      dplyr::mutate(
        stratum = stringr::str_c(
          !!!rlang::syms(names(rnd_strata_list)),
          sep = "_"
        ),
        .before = "block_id"
      )
  }

  if (is.null(rnd_name)) {
    rnd_name <- "treatment"
  }

  attr(rnd_list, "rnd_name") <- rnd_name

  rnd_list
}

#' @title Simulate Recruitment from a Stratified Randomization List
#' @description
#'   Simulates patient recruitment by randomly assigning strata to a given
#'   number of subjects and extracting their corresponding treatment allocations
#'   from a pre-built stratified randomization list.
#'
#'   For each simulated recruit, a stratum is drawn uniformly at random (with
#'   replacement) from the unique strata in `rnd_list`. Subjects within each
#'   stratum are then consumed sequentially from the top of the list. The
#'   function reports the resulting treatment balance and the magnitude of
#'   imbalance across arms at the end of recruitment.
#'
#' @param rnd_list A stratified tibble produced by [ody_make_random_list()] with
#'   a non-`NULL` `rnd_strata_list` argument. Must contain a `stratum` column
#'   and carry the `"rnd_name"` attribute identifying the treatment column.
#' @param n_recruitment A single positive integer specifying the total number of
#'   subjects to simulate.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{`simulated_recruitment`}{A tibble with one row per combination of
#'       stratum and treatment arm, with a column `n` counting the number of
#'       subjects allocated to each stratum–arm combination.}
#'     \item{`final_balance`}{A tibble with one row per treatment arm and a
#'       column `n` with the total number of allocations to that arm across all
#'       strata.}
#'     \item{`inbalance`}{A single integer equal to
#'       `max(n) - min(n)` across treatment arms in `final_balance`, measuring
#'       the magnitude of treatment imbalance at the end of recruitment.}
#'   }
#'
#' @seealso [ody_make_random_list()]
#' @export
ody_simulate_recruitment <- function(rnd_list, n_recruitment) {
  if (!is.null(rnd_list$stratum)) {
    strata <-
      rnd_list$stratum |>
      unique()
  } else {
    strata <- "dummy_stratum"

    rnd_list <- rnd_list |>
      dplyr::mutate(stratum = "dummy_stratum")
  }

  recruitment_strata <-
    strata[sample(1:length(strata), n_recruitment, replace = TRUE)] |>
    table()

  simulated_recruitment <-
    purrr::map2(
      names(recruitment_strata),
      recruitment_strata,
      ~ rnd_list |>
        dplyr::filter(.data$stratum == .x) |>
        dplyr::slice(1:.y)
    ) |>
    purrr::list_rbind()

  simulated_recruitment_count <-
    simulated_recruitment |>
    dplyr::count(.data$stratum, .data[[attr(rnd_list, "rnd_name")]])

  if (length(unique(simulated_recruitment_count$stratum)) == 1) {
    simulated_recruitment_count <- simulated_recruitment_count |>
      dplyr::select(-.data$stratum)
  }

  final_balance <-
    dplyr::count(
      simulated_recruitment,
      .data[[attr(rnd_list, "rnd_name")]]
    )

  inbalance <- max(final_balance$n) - min(final_balance$n)

  list(
    simulated_recruitment = simulated_recruitment_count,
    final_balance = final_balance,
    inbalance = inbalance
  )
}
