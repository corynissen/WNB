
# returns the games with points by Player/Team
get_games_w_pts <- function(df){
  tmp <- df %>%
    select(Game, Player, Team) %>%
    distinct() %>%
    group_by(Player, Team) %>%
    summarize(`Games W/ Pts` = length(Game))
  return(tmp)
}

# for a given week, return the player points summary
get_points <- function(df){
  tmp <- df %>%
    group_by(Player) %>%
    summarize(`Points (2s)` = sum(Score[Score == 2]),
              `Points (3s)` = sum(Score[Score == 3]),
              `Total Points` = sum(Score)) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  names(tmp) <- tmp[1,]
  tmp <- tmp[-1,]
  tmp$Value <- rownames(tmp)
  return(tmp)
}

# for a given week, return the player buckets summary
get_buckets <- function(df){
  tmp <- df %>%
    group_by(Player) %>%
    summarize(`Buckets (2s)` = length(Score[Score == 2]),
              `Buckets (3s)` = length(Score[Score == 3]),
              `Total Buckets` = length(Score)) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  names(tmp) <- tmp[1,]
  tmp <- tmp[-1,]
  tmp$Value <- rownames(tmp)
  return(tmp)
}

# return the points summary for all games / players by team
get_pts_df <- function(df, team_num){
  tm_pts <- df %>%  
    filter(Team == team_num) %>%
    group_by(Game) %>%
    group_map(~ get_points(.x)) 
  names(tm_pts) <- unique(df$Game)
  date_col <- unlist(lapply(tm_pts, nrow))
  date_col <- rep(names(date_col), as.numeric(date_col))
  
  tm_pts <- do.call(bind_rows, tm_pts)
  tm_pts$Date <- date_col
  tm_pts <- tm_pts %>% 
    select(Date, Value, everything()) %>%
    replace(is.na(.), 0)
  # Overall rows...
  games_w_pts_tm <- games_w_pts[games_w_pts$Team == team_num, ]
  tm_pts <- rbind(tm_pts,
                  c("Total", "Games W/ Pts",
                    games_w_pts_tm$`Games W/ Pts`[
                      match(names(tm_pts)[3:ncol(tm_pts)], games_w_pts_tm$Player)]),
                  stringsAsFactors = FALSE)
  
  tm_ov_tot <- tm_pts %>%
    filter(Value == "Total Points") %>%
    select(-Date, -Value) %>%
    mutate_if(is.character, as.numeric)
  
  tm_pts <- rbind(tm_pts,
                  c("Total", "Total Pts", apply(tm_ov_tot, 2, sum)),
                  stringsAsFactors = FALSE)
  
  tm_pts <- rbind(tm_pts,
                  c("Total", "Avg Pts",
                    round(as.numeric(tm_pts[nrow(tm_pts), (3:ncol(tm_pts))]) / 
                            as.numeric(tm_pts[(nrow(tm_pts) - 1), (3:ncol(tm_pts))]), 1)),
                  stringsAsFactors = FALSE)
  return(tm_pts)
}

# return the buckets summary for all games / players by team
get_bkts_df <- function(df, team_num){
  tm_bkts <- df %>%  
    filter(Team == team_num) %>%
    group_by(Game) %>%
    group_map(~ get_buckets(.x)) 
  names(tm_bkts) <- unique(df$Game)
  date_col <- unlist(lapply(tm_bkts, nrow))
  date_col <- rep(names(date_col), as.numeric(date_col))
  
  tm_bkts <- do.call(bind_rows, tm_bkts)
  tm_bkts$Date <- date_col
  tm_bkts <- tm_bkts %>% 
    select(Date, Value, everything()) %>%
    replace(is.na(.), 0)
  # Overall rows...
  games_w_pts_tm <- games_w_pts[games_w_pts$Team == team_num, ]
  tm_bkts <- rbind(tm_bkts,
                   c("Total", "Games W/ Pts",
                     games_w_pts_tm$`Games W/ Pts`[match(names(tm_bkts)[3:ncol(tm_bkts)],                           
                                                         games_w_pts_tm$Player)]),
                   stringsAsFactors = FALSE)
  
  tm_ov_tot <- tm_bkts %>%
    filter(Value == "Total Buckets") %>%
    select(-Date, -Value) %>%
    mutate_if(is.character, as.numeric)
  
  tm_ov_2 <- tm_bkts %>%
    filter(Value == "Buckets (2s)") %>%
    select(-Date, -Value) %>%
    mutate_if(is.character, as.numeric)
  
  tm_ov_3 <- tm_bkts %>%
    filter(Value == "Buckets (3s)") %>%
    select(-Date, -Value) %>%
    mutate_if(is.character, as.numeric)
  
  tm_bkts <- rbind(tm_bkts,
                   c("Total", "Total Bkts", apply(tm_ov_tot, 2, sum)),
                   stringsAsFactors = FALSE)
  
  tm_bkts <- rbind(tm_bkts,
                   c("Total", "Total 2s", apply(tm_ov_2, 2, sum)),
                   stringsAsFactors = FALSE)
  
  tm_bkts <- rbind(tm_bkts,
                   c("Total", "Total 3s", apply(tm_ov_3, 2, sum)),
                   stringsAsFactors = FALSE)
  
  tm_bkts <- rbind(tm_bkts,
                   c("Total", "Avg 2s",
                     round(as.numeric(tm_bkts[(nrow(tm_bkts) - 1), (3:ncol(tm_bkts))]) /
                             as.numeric(tm_bkts[(nrow(tm_bkts) - 3), (3:ncol(tm_bkts))]), 1)),
                   stringsAsFactors = FALSE)
  
  tm_bkts <- rbind(tm_bkts,
                   c("Total", "Avg 3s",
                     round(as.numeric(tm_bkts[(nrow(tm_bkts) - 1), (3:ncol(tm_bkts))]) /
                             as.numeric(tm_bkts[(nrow(tm_bkts) - 4), (3:ncol(tm_bkts))]), 1)),
                   stringsAsFactors = FALSE)
  return(tm_bkts)
  
}