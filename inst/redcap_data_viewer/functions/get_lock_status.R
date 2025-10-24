# Function to get the lock status of a record in REDCap using the Locking API module.
# It is used in all the extra tables that include the lock status.

get_lock_status <- function(record, api_key) {
  
  httr2::request(
    "https://redcap.vhio.net/redcap/api/?NOAUTH&type=module&prefix=locking_api&page=status"
  ) |>
    httr2::req_body_form(
      token = api_key,
      record = record,
      returnFormat = "csv"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    read.csv(text = _, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |> 
    dplyr::filter(
      !is.na(lock_status)
    ) |>
    dplyr::select(-username, -timestamp)
  
}