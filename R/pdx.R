#' Binomial GLMM to test PDX responsiveness
#'
#' Runs a binomial Genralized Mixed Model to test wether the response to a treatment depends on the sensitivity. The response variable is assumed to be a count of "positive" findings over a total count of 100.
#'
#' @param data_frame A data frame where:
#'   - The first column contains the PDX id's.
#'   - The second column is a two levels factors for sensitivity.
#'   - The third column is a two levels factor for treatment.
#'   - The fourth column is the response variable (integer from 0 to 100).
#'
#' @return A HTML report in the working directory.
#' @export
ody_pdx_model_percentage_response <- function(data_frame) {

  rmarkdown::render(
    system.file("reports", "pdx_response_percentage.Rmd", package = "odytools"),
    output_dir = getwd(),
    params = list(data_frame = data_frame)
  )

}
