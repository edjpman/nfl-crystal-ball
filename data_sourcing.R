
library(nflfastR)
library(httr)
library(jsonlite)
library(dplyr)


# --- Configuration ---
TEAM <- "KC"
START_YEAR <- 2009
END_YEAR <- 2024
OUTPUT_PATH <- "./data/pbp_compiled.csv"
RDS_CACHE_PATH <- "./data/pbp_raw.rds"

stadium_mapping <- read.csv("./data/stadium_mapping.csv", stringsAsFactors = FALSE)
yrz <- START_YEAR:END_YEAR


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


if (!file.exists(RDS_CACHE_PATH)) {
    nfl_data_fetch(yrz, RDS_CACHE_PATH)
}


pbp <- read_pbp(path = RDS_CACHE_PATH)

pbp <- data_subset(df = pbp, colz = collz1, team = TEAM)

pbp <- data_subset(df = pbp, colz = collz2, team = TEAM)

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
    team_score = case_when(
      home_team == TEAM ~ total_home_score,
      away_team == TEAM ~ total_away_score,
      TRUE ~ NA_real_
    )
  )


pbp <- pbp %>%
  mutate(
    opp_score = case_when(
      home_team == TEAM ~ total_away_score,
      away_team == TEAM ~ total_home_score,
      TRUE ~ NA_real_
    )
  )


pbp <- pbp %>%
  mutate(score_diff = team_score - opp_score)



pbp2 <- pbp2[pbp2$posteam == TEAM & !is.na(pbp2$posteam), ]



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



get_weather <- function(lat, lon, date) {
  base_url <- "https://archive-api.open-meteo.com/v1/archive"

  tryCatch({
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
  }, error = function(e) {
    message(sprintf(
      "Weather fetch failed for date %s at lat=%s, lon=%s: %s",
      date, lat, lon, e$message
    ))
    return(data.frame(
      time = NA_character_,
      temperature_2m_max = NA_real_,
      temperature_2m_min = NA_real_,
      precipitation_sum = NA_real_,
      stringsAsFactors = FALSE
    ))
  })
}



pbp <- merge(x=pbp, y=stadium_mapping,by='game_stadium', all.x=TRUE)

pbp_non_cumm <- pbp[,c('game_id','play_id','posteam','quarter_seconds_remaining','time','qtr','team_score','opp_score','score_diff')]
pbp_non_cumm <- pbp_non_cumm[pbp_non_cumm$posteam == TEAM & !is.na(pbp_non_cumm$posteam), ]


pbp_gi <- pbp[, c('season','game_stadium','game_id','game_date','home_team','away_team','season_type','location','city',
  'latitude','longitude','team_score','opp_score','score_diff','roof','surface')]

pbp_gi$tot_scr <- pbp_gi$team_score + pbp_gi$opp_score


final_scores <- pbp_gi %>%
  group_by(game_id) %>%
  slice_max(order_by = tot_scr, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(chiefs_win = ifelse(team_score > opp_score, 1, 0)) %>%
  select(game_id, team_score, opp_score, tot_scr, chiefs_win)


game_info <- pbp_gi %>%
  select(season, game_id, game_date, home_team, away_team, season_type,
         location, city, latitude, longitude, game_stadium, roof, surface) %>%
  distinct()





print('Making weather API call...')
weather_list <- lapply(1:nrow(game_info), function(i) {
  message(sprintf("Fetching weather %d/%d...", i, nrow(game_info)))
  get_weather(game_info$latitude[i], game_info$longitude[i], game_info$game_date[i])
})
print('Weather data call finished!')

failed_weather <- which(sapply(weather_list, is.null))
if(length(failed_weather) > 0) {
  message(sprintf("%d games missing weather data", length(failed_weather)))
}

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



#View(as.data.frame(final_df))
print(head(final_df))


write.csv(final_df, file = OUTPUT_PATH, row.names = TRUE)

