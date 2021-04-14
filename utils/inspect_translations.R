library("jsonlite")
translations <- read_json("./translations/translation.json")

keys <- translations$translation %>% lapply(function(e) e$de)
duplicated_keys_boolean <- as.character(duplicated(keys))
print(duplicated_keys_boolean)
