
source(here::here(
  list.files(here::here(), "_dependencies.R$")
))

cli::rule(
  left = "Odytools Generic Project",
  right = utils::packageVersion("odytools"),
  line = 2
)
odytools:::check_renvlock()
cli::cat_rule()
