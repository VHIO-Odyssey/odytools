
source(here::here(
  list.files(here::here(), "_dependencies.R$")
))

cli::rule(
  left = "Odytools REDCap Project",
  right = utils::packageVersion("odytools"),
  line = 2
)
odytools::ody_rc_current()
cat("\n")
odytools:::check_renvlock()
cli::cat_rule()
