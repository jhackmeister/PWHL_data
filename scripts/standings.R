# data scrapping via https://github.com/IsabelleLefebvre97/PWHL-Data-Reference?tab=readme-ov-file#hockeytech-base-url
# schedule 
url <- "https://lscluster.hockeytech.com/feed/index.php?feed=modulekit&view=statviewtype&stat=conference&type=standings&season_id=8&key=446521baf8c38984&client_code=pwhl"

response <- GET(url)
data_json <- content(response, "text")
data_list <- fromJSON(data_json, flatten = TRUE)

# Check structure
str(data_list, max.level = 2)

# Extract
skaters <- data_list$SiteKit$Statviewtype

team_data_ty <- df %>% 
  janitor::clean_names() %>% 
  filter(!name == "PWHL") %>% 
  select(-repeatheader, -placeholder, -division_id,
         -team_name, -teamname, -division_name)
