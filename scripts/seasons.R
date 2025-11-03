# pull season informaion
url <- "https://lscluster.hockeytech.com/feed/index.php?feed=modulekit&view=seasons&key=446521baf8c38984&client_code=pwhl"
response <- GET(url)
data_json <- content(response, "text")
data_list <- fromJSON(data_json, flatten = TRUE)

# Check structure
str(data_list, max.level = 2)

# Extract seasons
seasons <- as.data.frame(data_list$SiteKit$Seasons)
write.csv(seasons, "data/pwhl_seasons.csv")
