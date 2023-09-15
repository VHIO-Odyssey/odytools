
#' Start a RedCap project in RStudio
#'
#' @param token Project token. If not provided, a dialog promp will ask for it
#' @param url URL of the RedCap server (VHIO server by default).
#'
#' @export
ody_rc_init <- function(token = NULL,
                        url = "https://redcap.vhio.net/redcap/api/") {

  project_name <- list.files() |>
    stringr::str_subset(".Rproj$") |>
    stringr::str_remove(".Rproj$")

  if (length(project_name) == 0) stop("No RStudio project detected.")

  dir.create(here::here("data", "imports"), recursive = TRUE)
  dir.create(here::here("data", "extra"))
  dir.create(here::here("datasets"))
  dir.create(here::here("docs"))

  # Redcap  Import
  redcap_data <- odytools::ody_rc_import(token, url)

  import_date <- stringr::str_extract(
    attr(redcap_data, "import_date"), "....-..-.. ..:.."
  ) |>
    stringr::str_remove_all("-|:") |>
    stringr::str_replace(" ", "_")

  save(
    redcap_data,
    file = here::here(
      "data", "imports",
      stringr::str_c(project_name, "_import_", import_date, ".RData")
    )
  )

  # file.copy(
  #   here::here("datasets_template.R"),
  #   here::here("datasets", stringr::str_c(project_name, "_datasets.R"))
  # )



}
