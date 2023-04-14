#' Binomial GLMM to test PDX responsiveness
#'
#' Runs a binomial Genralized Mixed Model to test wether the response to a treatment depends on the sensitivity. The response variable is assumed to be a count of "positive" findings over a total count of 100.
#'
#' @param data_frame A data frame where:
#'   - The first column contains the PDX id's.
#'   - The second column is a two levels factors for sensitivity.
#'   - The third column is a two levels factor for treatment.
#'   - The fourth column is the response variable (integer from 0 to 100).
#' @param file_name Name of the html file. If NULL, the file is named with the date the analysis was performed and the name of the dependent variable.
#' @param file_dir Directory where the result is stored. The default is the current working directory.
#' @param method Method used to calculate glmr confidence intervals.
#'
#' @return A HTML dashboard.
#' @export
ody_pdx_model_percentage_response <- function(
    data_frame,
    file_name = NULL,
    file_dir = getwd(),
    method = c("profile", "Wald", "boot")
) {

  # Data structure confirmation
  names_df <- names(data_frame)

  rnd_fct <- stringr::str_c(
    "- PDX random factor:",
    stringr::str_c("'", names_df[1], "'"), "with",
    unique(data_frame[[1]]) |> length(),
    "subjects", sep = " "
  )

  sen_fct <- stringr::str_c(
    "- Sensitivity fixed factor:",
    stringr::str_c("'", names_df[2], "'"), "with levels",
    stringr::str_c(
      stringr::str_c("'", unique(data_frame[[2]]), "'"),
      collapse = " and "
    ),
    sep = " "
  )

  trt_fct <- stringr::str_c(
    "- Treatment fixed factor:",
    stringr::str_c("'", names_df[3], "'"), "with levels",
    stringr::str_c(
      stringr::str_c("'", unique(data_frame[[3]]), "'"),
      collapse = " and "
    ),
    sep = " "
  )

  resp <- stringr::str_c(
    "- Response variable:", stringr::str_c("'", names_df[4], "'"), sep = " "
  )

  cat(
    stringr::str_c(
      c("Data Structure:", rnd_fct, sen_fct, trt_fct, resp, "\n"),
      collapse = "\n"
    )
  )

  if (
    length(unique(data_frame[[2]])) != 2 |
    length(unique(data_frame[[3]])) != 2 |
    !all(data_frame[[4]] == as.integer(data_frame[[4]])) |
    any(data_frame[[4]] < 0) | any(data_frame[[4]] > 100)
  ) stop("Unexpected data structure. See ?ody_pdx_model_percentage_response to check the right expected structure.")

  ok <- readline("Please, confirm the data is correct to proceed (y/n): ")

  if (ok == "n") stop("Analysis interrupted")

  #Analysis
  method <- rlang::arg_match(method)

  if (is.null(file_name)) {
    file_name <-stringr::str_c(
      stringr::str_remove_all(lubridate::today(), "-"),
      "_", names_df[4], "_response.html"
    )
  }

  rmarkdown::render(
    system.file("reports", "pdx_response_percentage.Rmd", package = "odytools"),
    output_dir = file_dir,
    output_file = file_name,
    params = list(data_frame = data_frame, method = method)
  )

}
