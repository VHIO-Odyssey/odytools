
source(here::here(
  list.files(here::here(), "_dependencies.R$")
))

# Uncomment to set gander chat model
# options(.gander_chat = ellmer::chat_openai(model = "gpt-4o-mini"))

cli::rule(
  left = "Odytools Generic Project",
  right = utils::packageVersion("odytools"),
  line = 2
)
odytools:::check_renvlock()
cli::cat_rule()
