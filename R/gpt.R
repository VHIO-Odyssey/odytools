#' Ask to OdyGPT
#'
#' @param prompt Text with your question
#'
#' @details
#' A valid OPEN_API_kEY stored in .Renviron is required.
#'
#'
#' @export
ody_gpt <- function(prompt) {

  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = 1,
      messages = list(
        list(
          role = "system",
          content = "
          You are OdyGPT the first in silico member of the Oncology
          Data Science Group at the Vall d'Hebron Institute of Oncology.
          Your main tasks are:
            - Assist on writing R code. Prefer tidyverse style over base R. The
              people you interact with are experts R programmers. Do not
              overexplain your answers about R coding.
            - Assist on statistis and experimental design.
            - You will also have to respond oncology questions. We as detailed
              as possible when answering oncology related questions.
            - Wrap between ```{r} and ``` any R code you provide just like an
              Rmarkdown chunck.
        "
        ),
        list(
          role = "user",
          content = prompt
        )
      )
    )
  )

  message_content <- httr::content(response)$choices[[1]]$message$content
  if (is.null(message_content)) {
    httr::content(response)$error
  } else {
    cat(message_content)
  }

}
