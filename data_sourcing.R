
library(nflfastR)
library(dplyr) 

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


data_subset <- function(df,colz) {
    pbp_subset <- df[, c(colz)]
    return(pbp_subset)
}



file <- "filepath"
yrz <- 2022:2024
collz <- c('play_id','game_id','home_team','away_team','season_type','week','down','quarter_seconds_remaining',
'quarter_end','game_date','posteam','defteam','qtr','time','drive','ydsnet','yards_gained','pass_length','air_yards','yards_after_catch',
'total_home_score','total_away_score','score_differential','score_differential_post','play_type','posteam_score','defteam_score','posteam_score_post','defteam_score_post',
'first_down_rush','first_down_pass','first_down_penalty','third_down_converted','third_down_failed','fourth_down_converted','fourth_down_failed','incomplete_pass','penalty',
'tackled_for_loss','qb_hit','rush_attempt','pass_attempt','sack','touchdown','pass_touchdown','rush_touchdown','return_touchdown','sack','return_team','return_yards',
'penalty_team','penalty_yards','season','start_time','time_of_day','stadium','weather','series','series_result','order_sequence','away_score','home_score','fixed_drive',
'drive_play_count','drive_first_downs','drive_inside20','drive_ended_with_score','drive_quarter_start','drive_quarter_end','drive_yards_penalized','location','total','roof','surface',
'temp','wind','stadium_id','game_stadium','play','out_of_bounds')


pbp <- read_pbp(path = file)

pbp <- data_subset(df = pbp, colz = collz)

#print(head(pbp))
print(colnames(pbp))