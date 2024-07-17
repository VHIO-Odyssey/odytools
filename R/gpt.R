#' Ask to OdyGPT
#'
#' @param prompt Text with your question
#' @param role Role of the chatbot. Predefined options are "ody_gpt" and "english_corrector" but any text can be provided.
#' @param model Model to use. Default is "gpt-4o"
#' @param api_key_name Name of the API key stored in .Renviron. Default is "OPENAI_API_KEY"
#'
#' @details
#' The chat is stored in the Global Environment, so successive questions are based on the previous ones, creating the illusion of a conversation (it is not; ody_gpt is not human and thus not your friend). A valid OPEN_API_KEY stored in .Renviron is required.
#'
#'
#' @export
ody_gpt <- function(
    prompt = NULL,
    role = "ody_gpt",
    model = "gpt-4o",
    api_key_name = "OPENAI_API_KEY") {

  rlang::check_installed("tidychatmodels")

   if (is.null(prompt)) {
     prompt <- rstudioapi::showPrompt("OdyGPT", "Please provide a prompt")
   }

  if (exists("odygpt_chat", envir = .GlobalEnv)) {

    chat <-
      odygpt_chat |>
      tidychatmodels::add_message(prompt) |>
      tidychatmodels::perform_chat()

    tidychatmodels::extract_chat(chat, silent = TRUE) |>
      dplyr::pull("message") |>
      tail(1) |>
      cat()

    odygpt_chat <<- chat

  } else {

    if (role == "ody_gpt") {

      role <- "
    You are OdyGPT, assistant of the Oncology Data Science Group at the Vall d'Hebron
    Institute of Oncology. Your main tasks are:

    1. Assisting in writing R code. Prefer tidyverse style over base R. The people
       you interact with are expert R programmers. Do not overexplain your answers
       about R coding. If an specific code is requested, just provide de code with any
       explanation.

    2. Assisting in statistics and experimental design.

    3. You will also have to respond to oncology questions. Be as detailed as
       possible when answering oncology-related questions.
    "

    }

    if (role == "corrector") {

      role <- "You will analyze any given text for grammatical errors
    and provide corrections focusing on punctuation, sentence structure, and overall
    clarity to ensure a polished and error-free piece. Also provide a brief explanation of
    the changes you made. Keep corrections at minimum always ensuring the text is
    well written and easy to read. End the review with a general valoration of the
    quality of text I provide. If the text is in English use American English unless
    specified otherwise and spanish from Spain if it is spanish. "

    }

    chat <-
      tidychatmodels::create_chat(
      "openai", Sys.getenv(api_key_name)
    ) |>
      tidychatmodels::add_model(model) |>
      tidychatmodels::add_message(
        role = "system",
        message = role
      ) |>
      tidychatmodels::add_message(
        role = "user",
        message = prompt
      ) |>
      tidychatmodels::perform_chat()


    tidychatmodels::extract_chat(chat, silent = TRUE) |>
      dplyr::pull("message") |>
      tail(1) |>
      cat()

    odygpt_chat <<- chat
  }
}

