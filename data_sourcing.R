
library(nflfastR)
library(httr)
library(jsonlite)
library(dplyr)


# --- Path configuration (relative to repo root) ---
data_dir <- "data"
file  <- file.path(data_dir, "pbp_2022_2024.rds")
file2 <- file.path(data_dir, "pbp_2018_2024.rds")
file3 <- file.path(data_dir, "pbp_2009_2024.rds")
path22 <- file.path(data_dir, "pbp_compiled3.csv")


## Function to save play-by-play data 
nfl_data_fetch <- function(years,path) {
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








yrz <- 2009:2024





collz1 <- c('play_id','game_id','home_team','away_team','season_type','week','down','quarter_seconds_remaining',
'quarter_end','game_date','posteam','defteam','qtr','time','drive','ydsnet','yards_gained','pass_length','air_yards','yards_after_catch',
'total_home_score','total_away_score','score_differential','score_differential_post','play_type','posteam_score','defteam_score','posteam_score_post','defteam_score_post',
'first_down_rush','first_down_pass','first_down_penalty','third_down_converted','third_down_failed','fourth_down_converted','fourth_down_failed','incomplete_pass','penalty',
'tackled_for_loss','qb_hit','rush_attempt','pass_attempt','sack','touchdown','pass_touchdown','rush_touchdown','return_touchdown','sack','return_team','return_yards',
'penalty_team','penalty_yards','season','start_time','time_of_day','stadium','weather','series','series_result','order_sequence','away_score','home_score','fixed_drive',
'drive_play_count','drive_first_downs','drive_inside20','drive_ended_with_score','drive_quarter_start','drive_quarter_end','drive_yards_penalized','location','total','roof','surface',
'temp','wind','stadium_id','game_stadium','play','out_of_bounds')


collz2 <- c('play_id','game_id','game_date','home_team','away_team','posteam','defteam','season_type','season','location','stadium_id','game_stadium','quarter_seconds_remaining',
'time','start_time','time_of_day','qtr','qb_hit','total_home_score','total_away_score','score_differential','sack','penalty','penalty_yards','tackled_for_loss','third_down_converted',
'third_down_failed','fourth_down_converted','fourth_down_failed','rush_attempt','pass_attempt','incomplete_pass','ydsnet','yards_gained','roof','surface')


pbp <- read_pbp(path = file3)

pbp <- data_subset(df = pbp, colz = collz1, team = "KC")


pbp <- data_subset(df = pbp, colz = collz2, team = "KC")

pbp <- pbp[pbp$play_id != 1, ]



pbp2 <- pbp %>%
  mutate(qb_hit = ifelse(is.na(qb_hit), 0, qb_hit),
  sack = ifelse(is.na(sack), 0, sack),
  penalty = ifelse(is.na(penalty), 0, penalty),
  penalty_yards = ifelse(is.na(penalty_yards), 0, penalty_yards),
  #home_score = ifelse(is.na(total_home_score), 0, total_home_score),
  #away_score = ifelse(is.na(total_away_score), 0, total_away_score),
  #score_diff = ifelse(is.na(score_differential), 0, score_differential),
  tackled_for_loss = ifelse(is.na(tackled_for_loss), 0, tackled_for_loss),
  thrid_d_conv = ifelse(is.na(third_down_converted), 0, third_down_converted),
  thrid_d_fail = ifelse(is.na(third_down_failed), 0, third_down_failed),
  frth_d_conv = ifelse(is.na(fourth_down_converted), 0, fourth_down_converted),
  frth_d_fail = ifelse(is.na(fourth_down_failed), 0, fourth_down_failed),
  rush_attmpt = ifelse(is.na(rush_attempt), 0, rush_attempt),
  pass_attmpt = ifelse(is.na(pass_attempt), 0, pass_attempt),
  incmpl_pass = ifelse(is.na(incomplete_pass), 0, incomplete_pass),
  ydsgain = ifelse(is.na(yards_gained), 0, yards_gained)
  ) %>%
  group_by(game_id, defteam) %>%
  arrange(game_id, defteam, play_id) %>% 
  mutate(cum_play_count = row_number(),
  cum_qb_hits = cumsum(qb_hit),
  cum_sacks = cumsum(sack),
  cum_penalty = cumsum(penalty),
  cum_penalty_yards = cumsum(penalty_yards),
  #cum_hs = cumsum(home_score),
  #cum_as = cumsum(away_score),
  #cum_sd = cumsum(score_diff),
  cum_tfl = cumsum(tackled_for_loss),
  cum_3dc = cumsum(thrid_d_conv),
  cum_3df = cumsum(thrid_d_fail),
  cum_4dc = cumsum(frth_d_conv),
  cum_4df = cumsum(frth_d_fail),
  cum_rush_attmpt = cumsum(rush_attmpt),
  cum_pass_attmpt = cumsum(pass_attmpt),
  cum_incmpl_pass = cumsum(incmpl_pass),
  cum_ydsgain = cumsum(ydsgain)
  ) %>%
  ungroup()



pbp <- pbp %>%
  mutate(
    chiefs_score = case_when(
      home_team == "KC" ~ total_home_score,
      away_team == "KC" ~ total_away_score,
      TRUE ~ NA_real_
    )
  )


pbp <- pbp %>%
  mutate(
    opp_score = case_when(
      home_team == "KC" ~ total_away_score,
      away_team == "KC" ~ total_home_score,
      TRUE ~ NA_real_
    )
  )


pbp <- pbp %>%
  mutate(score_diff = chiefs_score - opp_score)



pbp2 <- pbp2[pbp2$posteam == 'KC' & !is.na(pbp2$posteam), ]



pbp2 <- pbp2 %>% 
  mutate(quarter_elapsed = 1 - (quarter_seconds_remaining / 900))


pbp2 <- pbp2 %>%
  mutate(
    qtr1 = ifelse(qtr == 1, 1, 0),
    qtr2 = ifelse(qtr == 2, 1, 0),
    qtr3 = ifelse(qtr == 3, 1, 0),
    qtr4 = ifelse(qtr == 4, 1, 0),
    ot = ifelse(qtr > 4, 1, 0)
  )

pbp_cumm <- pbp2[, c('play_id', 'game_id','home_team','away_team','posteam','defteam','season_type',
'location','game_stadium','cum_play_count','cum_qb_hits','cum_sacks','cum_penalty','cum_penalty_yards','cum_tfl','cum_3dc','cum_3df','cum_4dc',
'cum_4df','cum_rush_attmpt','cum_pass_attmpt','cum_incmpl_pass','cum_ydsgain','quarter_elapsed','qtr1','qtr2','qtr3','qtr4','ot')]



stadium_mapping <- data.frame(
  city = c("New York", "Nashville", "Detroit", "Cincinnati", "Chicago", "Dallas", "Las Vegas",
           "Green Bay", "Denver", "Jacksonville", "Kansas City", "Baltimore", "Philadelphia",
           "Pittsburgh", "Cleveland", "San Francisco", "New Orleans", "Buffalo", "Boston",
           "Tampa", "Indianapolis", "London", "London", "Munich", "Mexico City", "Frankfurt", "São Paulo","Charlotte",
           "Atlanta","Houston","Los Angeles","Phoenix","Minneapolis","Kansas City","Seattle","Washington DC",
           "Miami","Pittsburgh","Los Angeles","Oakland","Cincinnati","Oakland","Denver","Carson",
           "Cleveland","St. Louis","Jacksonville","Atlanta","Denver","Jacksonville","Nashville",
           "Oakland","San Diego","Seattle","Buffalo","Houston","Miami","Minneapolis","Phoenix"),
  game_stadium = c("MetLife Stadium", "Nissan Stadium", "Ford Field", "Paycor Stadium", "Soldier Field",
              "AT&T Stadium", "Allegiant Stadium", "Lambeau Field", "Empower Field at Mile High",
              "TIAA Bank Stadium", "GEHA Field at Arrowhead Stadium", "M&T Bank Stadium",
              "Lincoln Financial Field", "Acrisure Stadium", "FirstEnergy Stadium", "Levi's Stadium",
              "Mercedes-Benz Superdome", "New Era Field", "Gillette Stadium", "Raymond James Stadium",
              "Lucas Oil Stadium", "Tottenham Stadium", "Wembley Stadium", "Allianz Arena",
              "Azteca Stadium", "Deutsche Bank Park", "Arena Corinthians","Bank of America Stadium",
              "Mercedes-Benz Stadium","NRG Stadium","SoFi Stadium","State Farm Stadium","U.S. Bank Stadium","Arrowhead Stadium",
              "CenturyLink Field","FedExField",
              "Hard Rock Stadium","Heinz Field","Los Angeles Memorial Coliseum","Oakland-Alameda County Coliseum",
              "Paul Brown Stadium","Ring Central Coliseum","Sports Authority Field at Mile High","StubHub Center",
              "Cleveland Browns Stadium","Edward Jones Dome","EverBank Field","Georgia Dome","Invesco Field at Mile High","Jacksonville Municipal Stadium","LP Field",
              "O.co Coliseum","Qualcomm Stadium","Qwest Field","Ralph Wilson Stadium","Reliant Stadium","Sun Life Stadium","TCF Bank Stadium","University of Phoenix Stadium"),
    latitude = c('40.813778','36.166245','42.341563','39.095245','41.862065','32.746202','36.090415','44.502672','39.743563','30.323192','39.048364','39.278440','39.900706','40.447947',
    '41.504749','37.402606','29.952629','42.775072','42.089961','27.974268','39.758160','51.604572','51.554642','48.217302','19.304293','50.067547','-23.544088',
    '35.225253','33.754531','29.685818','33.952588','33.527185','44.972338','39.051848','47.595433','38.906812',
    "25.957019","40.446759","34.013478","37.752702","39.095450","37.752702","39.743958","33.864022",
    "41.505319","38.631677","30.324831","33.756263","39.744058","30.322966","36.167343",
              "37.752504","32.783030","47.595152","42.774177","29.685154","25.960510","44.975956","33.527133"),
    longitude = c('-74.074310','-86.771141','-83.044846','-84.515875','-87.616560','-97.093053','-115.185056','-88.063807','-105.022415','-81.639100',
    '-94.485532','-76.624118','-75.169016','-80.016837',
    '-81.699658','-121.971040','-90.081253','-78.789223','-71.265643','-82.504901','-86.162679','-0.066207','-0.279087','11.624038','-99.152843','8.644358','-46.473001',
    '-80.851007','-84.399672','-95.412544','-118.342093','-112.689920','-93.258181','-94.488386','-122.333298','-76.866782',
    "-80.241684","-80.017483","-118.290161","-122.198803","-84.518077","-122.198803","-105.022570","-118.262442",
    "-81.697775","-90.188760","-81.635730","-84.402701","-105.022202","-81.635075","-86.770036",
              "-122.199049","-117.122776","-122.333323","-78.788957","-95.412337","-80.240085","-93.226269","-112.264330"),
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

pbp_non_cumm <- pbp[,c('game_id','play_id','posteam','quarter_seconds_remaining','time','qtr','chiefs_score','opp_score','score_diff')]
pbp_non_cumm <- pbp_non_cumm[pbp_non_cumm$posteam == 'KC' & !is.na(pbp_non_cumm$posteam), ]


pbp_gi <- pbp[, c('season','game_stadium','game_id','game_date','home_team','away_team','season_type','location','city',
  'latitude','longitude','chiefs_score','opp_score','score_diff','roof','surface')]

pbp_gi$tot_scr <- pbp_gi$chiefs_score + pbp_gi$opp_score


final_scores <- pbp_gi %>%
  group_by(game_id) %>%
  slice_max(order_by = tot_scr, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(chiefs_win = ifelse(chiefs_score > opp_score, 1, 0)) %>%
  select(game_id, chiefs_score, opp_score, tot_scr, chiefs_win)


game_info <- pbp_gi %>%
  select(season, game_id, game_date, home_team, away_team, season_type,
         location, city, latitude, longitude, game_stadium, roof, surface) %>%
  distinct()





print('Making weather API call...')
weather_list <- lapply(1:nrow(game_info), function(i) {
 get_weather(game_info$latitude[i], game_info$longitude[i], game_info$game_date[i])
})
print('Weather data call finished!')

weather_df <- bind_rows(weather_list)
weather_df$latitude   <- game_info$latitude
weather_df$longitude  <- game_info$longitude
weather_df$game_date  <- game_info$game_date



game_info_final <- game_info %>%
 left_join(weather_df, by = c("latitude","longitude","game_date"))

print('First GIF join complete!')



game_info_final <- game_info_final %>%
 left_join(final_scores, by = "game_id")



#T1 (game): game_id, T3 (cumm): game_id, play_id, T2 (non_cumm): game_id, play_id
final_df <- game_info_final %>%
 left_join(pbp_non_cumm, by = "game_id") %>%
 left_join(pbp_cumm, by = c("game_id", "play_id"))



View(as.data.frame(final_df))



write.csv(final_df, file = path22, row.names = TRUE)


