library(httr)
library(jsonlite)
library(dplyr)
library(janitor)

# data scrapping via https://github.com/IsabelleLefebvre97/PWHL-Data-Reference?tab=readme-ov-file#hockeytech-base-url
# schedule 
url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&season_id=8&key=446521baf8c38984&client_code=pwhl"

response <- GET(url)
data_json <- content(response, "text")
data_list <- fromJSON(data_json, flatten = TRUE)

# Check structure
str(data_list, max.level = 2)

# Extract schedule
schedule <- as.data.frame(data_list$SiteKit$Schedule)

# Clean column names
schedule <- schedule %>% 
  janitor::clean_names() %>% 
  select(game_id, date_played, home_team_code, home_goal_count, visiting_team_code,
         visiting_goal_count, period, overtime, shootout, attendance, venue_name, notes_text)