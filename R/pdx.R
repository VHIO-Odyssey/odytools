
# Old function. First version that only performs binomial GLMMs.
# ody_pdx_model_senitivity is the current life function.
# pdx_response_percentage.Rmd in inst/pdx_reports is also superseded by
# pdx_model_sensitivity.Rmd
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


#' (Generalized) Linear Mixed Model to test PDX sensitivity
#'
#' Runs a (Generalized) Linear Mixed Model to test wether the response to a treatment depends on the sensitivity. By default, if the response is a count of positive findings over 100, the function performs a Binomial Generalized Linear Mixed Model, otherwise it runs a Linear Mixed Model.
#'
#' @param data_frame A data frame where:
#'   - The first column contains the PDX id's.
#'   - The second column is a two levels factors for sensitivity.
#'   - The third column is a two levels factor for treatment.
#'   - The fourth column is the response variable.
#' @param file_name Name of the html file. If NULL, the file is named with the date the analysis was performed, the name of the dependent variable and type of model performed.
#' @param file_dir Directory where the result is stored. The default is the current working directory.
#' @param model_type The type of model the function will run. With the default value, "guess_it", the function "guesses" the model according to the response variable. Other valid arguments are "lmm" or "glmm" to force the function to perform a LMM or a GLMM respectively.
#'
#' @details The way the function decides wich model to use when model_type equals "guess_it" is very simple: If the response variable is a 0 to 100 integer (a number with no decimals), then the model is a GLMM, else, the model is a LMM.
#'
#' @return A HTML report
#' @export
ody_pdx_model_sensitivity <- function(
    data_frame,
    file_name = NULL,
    file_dir = getwd(),
    model_type = c("guess_it", "glmm", "lmm")
) {

  model_type <- rlang::arg_match(model_type)

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
    length(unique(data_frame[[2]])) != 2 ||
    length(unique(data_frame[[3]])) != 2
  ) {
    stop(
      "Unexpected data structure: Factors with more than two levels.
       See ?ody_pdx_model_sensitivity to check the right expected structure."
    )
  }

  if (any(data_frame[[4]] < 0)) {
    stop(
      "Unexpected data structure: Negative values in the response variable.
       See ?ody_pdx_model_sensitivity to check the right expected structure."
    )
  }

  ok <- readline("Please, confirm the data is correct to proceed (y/n): ")

  if (ok == "n") stop("Analysis interrupted")

  levels_sensitivity <-  unique(data_frame[[2]])
  levels_treatment <- unique(data_frame[[3]])


  resistant_level <- readline(
    stringr::str_c(
      "In the '", names_df[2],  "' factor, which is the 'resistant' level?",
      " (", levels_sensitivity[1], "/", levels_sensitivity[2], ") "
    )
  )

  control_level <- readline(
    stringr::str_c(
      "In the '", names_df[3], "' factor, which is the 'control' level?",
      " (", levels_treatment[1], "/", levels_treatment[2], ") "
    )
  )

  #Analysis

  ## Type of analisys if model_type is "guess_it
  if (model_type == "guess_it") {

    no_dec <- all(as.integer(data_frame[[4]]) == data_frame[[4]])
    from_0_to_100 <- all(dplyr::between(data_frame[[4]], 0, 100))

    if (no_dec && from_0_to_100) model_type <- "glmm" else model_type <- "lmm"

  }

  ## File name
  if (is.null(file_name)) {

     sys_time_num <- Sys.time() |> stringr::str_remove_all("[^\\d]")
     date_time <- stringr::str_c(
      stringr::str_sub(sys_time_num, 1, 8),
      stringr::str_sub(sys_time_num, 9, 12), sep = "_")

    file_name <-stringr::str_c(
      date_time, "_", names_df[4], "_", model_type, ".html"
    )

  }

  rmarkdown::render(
    system.file(
      "pdx_reports", "pdx_model_sensitivity.Rmd", package = "odytools"
    ),
    output_dir = file_dir,
    output_file = file_name,
    params = list(
      data_frame = data_frame,
      resistant_level = resistant_level,
      control_level = control_level,
      model_type = model_type
    )
  )

}
