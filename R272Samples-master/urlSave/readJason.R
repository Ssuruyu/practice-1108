install.packages("rjson")

library("rjson")
json_file <- "result.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))