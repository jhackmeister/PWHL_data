library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Roster API URL - Set season and team names
url <- "https://lscluster.hockeytech.com/feed/index.php?feed=modulekit&view=roster&team_id=8&season_id=7&key=446521baf8c38984&client_code=pwhl"
response <- GET(url)
data_json <- content(response, "text")
data_list <- fromJSON(data_json, flatten = TRUE)

# Extract roster data
roster_list <- data_list$SiteKit$Roster

roster_list_clean <- lapply(roster_list, function(x) {
  x$draftinfo <- NULL
  return(x)
})

roster_df_simple <- bind_rows(roster_list_clean)

