library(googleAuthR)
library(searchConsoleR)
scr_auth()
service_token <- gar_auth_service(json_file="pecu610-c74a6019e9f7.json")
website <- "http://copenhagenish.me"
start <- Sys.Date() - 3
end <- Sys.Date() - 3 
download_dimensions <- c('date','query')
type <- c('image')

data <- search_analytics(siteURL = website, 
                         startDate = start, 
                         endDate = end, 
                         dimensions = download_dimensions, 
                         searchType = type)
filename <- paste("search_analytics",
                  Sys.Date(),
                  paste(download_dimensions, collapse = "",sep=""),
                  type,".csv",sep="-")
write.csv(data, filename)