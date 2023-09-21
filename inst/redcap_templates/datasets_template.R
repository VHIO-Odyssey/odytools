
# 1. Code your datasets.
# 2. Label them as datasets with ody_add_to_datasets
# 3. Refresh the project (Addins/Odytools/Refresh Datasets).

example <- tibble::tibble(
  x = 1,
  y = "a"
) |>
  odytools::ody_add_to_datasets("This is an example")


