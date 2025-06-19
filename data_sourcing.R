
library(nflfastR)
library(httr)
library(jsonlite)
library(dplyr)



#library(dplyr)

## Function to save play-by-play data 
nlf_data_fetch <- function(years,path) {
    nflreadr::.clear_cache()
    message('Loading dataset from nflfastR...')
    pbp <- nflfastR::load_pbp(years)
    message('Saving dataset locally as RDS...')
    saveRDS(pbp,file=path)
    message('Saved dataset to: ', path)
}



## Function to read any saved play-by-play data

read_pbp <- function(path) {
    if (!file.exists(path)) {
        stop('The file does not exist at the specified path: ', path)
    }
    message('Reading dataset from: ', path)
    data <- readRDS(path)
    message('Successful read operation!')
    return(data)
}


data_subset <- function(df,colz,team) {
    pbp_subset <- df[, c(colz)]
    pbp_subset <- subset(pbp_subset, grepl(team, pbp_subset$game_id))
    return(pbp_subset)
}








file <- "/Users/ethanparks/Desktop/repos/pbp_2022_2024.rds"
yrz <- 2022:2024
collz1 <- c('play_id','game_id','home_team','away_team','season_type','week','down','quarter_seconds_remaining',
'quarter_end','game_date','posteam','defteam','qtr','time','drive','ydsnet','yards_gained','pass_length','air_yards','yards_after_catch',
'total_home_score','total_away_score','score_differential','score_differential_post','play_type','posteam_score','defteam_score','posteam_score_post','defteam_score_post',
'first_down_rush','first_down_pass','first_down_penalty','third_down_converted','third_down_failed','fourth_down_converted','fourth_down_failed','incomplete_pass','penalty',
'tackled_for_loss','qb_hit','rush_attempt','pass_attempt','sack','touchdown','pass_touchdown','rush_touchdown','return_touchdown','sack','return_team','return_yards',
'penalty_team','penalty_yards','season','start_time','time_of_day','stadium','weather','series','series_result','order_sequence','away_score','home_score','fixed_drive',
'drive_play_count','drive_first_downs','drive_inside20','drive_ended_with_score','drive_quarter_start','drive_quarter_end','drive_yards_penalized','location','total','roof','surface',
'temp','wind','stadium_id','game_stadium','play','out_of_bounds')


collz2 <- c('play_id','game_id','game_date','home_team','away_team','posteam','defteam','season_type','location','stadium_id','game_stadium','quarter_seconds_remaining',
'time','start_time','time_of_day','qtr','qb_hit','total_home_score','total_away_score','score_differential','sack','penalty','penalty_yards','tackled_for_loss','third_down_converted',
'third_down_failed','fourth_down_converted','fourth_down_failed','rush_attempt','pass_attempt','incomplete_pass','ydsnet')


pbp <- read_pbp(path = file)

pbp <- data_subset(df = pbp, colz = collz1, team = "KC")


pbp <- data_subset(df = pbp, colz = collz2, team = "KC")



pbp2 <- pbp %>%
  mutate(qb_hit = ifelse(is.na(qb_hit), 0, qb_hit),
  sack = ifelse(is.na(sack), 0, sack),
  home_score = ifelse(is.na(total_home_score), 0, total_home_score),
  away_score = ifelse(is.na(total_away_score), 0, total_away_score),
  score_diff = ifelse(is.na(score_differential), 0, score_differential)
  ) %>%
  group_by(game_id, defteam) %>%
  arrange(game_id, defteam, play_id) %>%  # play_id ensures chronological order
  mutate(cum_qb_hits = cumsum(qb_hit),
  cum_sacks = cumsum(sack),
  cum_hs = cumsum(home_score),
  cum_as = cumsum(away_score),
  cum_sd = cumsum(score_diff)
  ) %>%
  ungroup()

#pbp <- pbp[pbp$posteam == "KC", ]

pbp <- pbp[pbp$play_id != 1, ]


#print(head(pbp))
#print(colnames(pbp))
#print(unique(pbp$week))
#View(as.data.frame(unique(pbp[, c("stadium_id", "game_stadium")])))


stadium_mapping <- data.frame(
  city = c("New York", "Nashville", "Detroit", "Cincinnati", "Chicago", "Dallas", "Las Vegas",
           "Green Bay", "Denver", "Jacksonville", "Kansas City", "Baltimore", "Philadelphia",
           "Pittsburgh", "Cleveland", "San Francisco", "New Orleans", "Buffalo", "Boston",
           "Tampa", "Indianapolis", "London", "London", "Munich", "Mexico City", "Frankfurt", "São Paulo"),
  game_stadium = c("MetLife Stadium", "Nissan Stadium", "Ford Field", "Paycor Stadium", "Soldier Field", ##
              "AT&T Stadium", "Allegiant Stadium", "Lambeau Field", "Empower Field at Mile High", ##
              "TIAA Bank Stadium", "GEHA Field at Arrowhead Stadium", "M&T Bank Stadium", ##
              "Lincoln Financial Field", "Acrisure Stadium", "FirstEnergy Stadium", "Levi's Stadium", ##
              "Mercedes-Benz Superdome", "New Era Field", "Gillette Stadium", "Raymond James Stadium", ##
              "Lucas Oil Stadium", "Tottenham Stadium", "Wembley Stadium", "Allianz Arena", ##
              "Azteca Stadium", "Deutsche Bank Park", "Arena Corinthians"), 
    latitude = c('40.813778','36.166245','42.341563','39.095245','41.862065','32.746202','36.090415','44.502672','39.743563','30.323192','39.048364','39.278440','39.900706','40.447947',
    '41.504749','37.402606','29.952629','42.775072','42.089961','27.974268','39.758160','51.604572','51.554642','48.217302','19.304293','50.067547','-23.544088'),
    longitude = c('-74.074310','-86.771141','-83.044846','-84.515875','-87.616560','-97.093053','-115.185056','-88.063807','-105.022415','-81.639100','-94.485532','-76.624118','-75.169016','-80.016837',
    '-81.699658','-121.971040','-90.081253','-78.789223','-71.265643','-82.504901','-86.162679','-0.066207','-0.279087','11.624038','-99.152843','8.644358','-46.473001'),
  stringsAsFactors = FALSE
)



get_weather <- function(lat, lon, date) {
  base_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  res <- GET(base_url, query = list(
    latitude = lat,
    longitude = lon,
    start_date = date,
    end_date = date,
    daily = "temperature_2m_max,temperature_2m_min,precipitation_sum",
    timezone = "auto"
  ))
  
  weather <- fromJSON(content(res, "text", encoding = "UTF-8"))
  return(weather$daily)
}



pbp <- merge(x=pbp, y=stadium_mapping,by='game_stadium', all.x=TRUE)

#pbp <- pbp[160:200,]

# print('Making weather API call...')
# weather_list <- lapply(1:nrow(pbp), function(i) {
#   get_weather(pbp$latitude[i], pbp$longitude[i], pbp$game_date[i])
# })
# print('Weather data call finished!')


# games_weather <- cbind(pbp, do.call(rbind, weather_list))

View(as.data.frame(pbp2))

