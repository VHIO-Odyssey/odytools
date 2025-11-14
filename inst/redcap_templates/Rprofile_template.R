source(here::here(
  list.files(here::here(), "_dependencies.R$")
))

# Uncomment to set gander chat model
# options(.gander_chat = ellmer::chat_openai(model = "gpt-4o-mini"))

cli::cat_rule(
  left = "Odytools REDCap Project",
  right = utils::packageVersion("odytools"),
  line = 2
)
odytools::ody_rc_current()
cat("\n")
odytools:::check_renvlock()
cli::cat_rule()
