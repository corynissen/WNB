
# get the points for for a given team
getpf <- function(team){
  sum(tmsc$teama_score[tmsc$teama == team], na.rm = TRUE) +
    sum(tmsc$teamb_score[tmsc$teamb == team], na.rm = TRUE)
}

# get the points against for a given team
getpa <- function(team){
  sum(tmsc$teamb_score[tmsc$teama == team], na.rm = TRUE) +
    sum(tmsc$teama_score[tmsc$teamb == team], na.rm = TRUE)
}

# given the team score df, return the team records table
get_team_rec <- function(tmsc){
  tm_rec <- tmsc %>%
    filter(!is.na(teama_score)) %>%
    mutate(matchup = paste0(teama, " vs. ", teamb),
           winner = ifelse(teama_score > teamb_score, teama, teamb),
           loser = ifelse(teama_score > teamb_score, teamb, teama))
  tm_rec <- data.frame(Team = 1:4,
                       Wins = sapply(1:4, function(x)sum(tm_rec$winner == x)),
                       Losses = sapply(1:4, function(x)sum(tm_rec$loser == x)),
                       stringsAsFactors = FALSE)
  tm_rec$Record <- paste0(tm_rec$Wins, " - ", tm_rec$Losses)
  tm_rec$Avg_PF = round(unlist(lapply(1:4, getpf)) / length(unique(tmsc$date[!is.na(tmsc$teama_score)])), 1)
  tm_rec$Avg_PA = round(unlist(lapply(1:4, getpa)) / 
                          length(unique(tmsc$date[!is.na(tmsc$teama_score)])), 1)
  return(tm_rec)
}

# given the team score df and team, return a table of the schedule / results
get_team_results <- function(tmsc, team_num){
  tmp <- tmsc %>%
    filter(teama == team_num | teamb == team_num) %>%
    mutate(scoresheet_link = ifelse(scoresheet != "",
                                    cell_spec("Scoresheet Link", "html", link = scoresheet),
                                    ""),
           teama_score = ifelse(is.na(teama_score), "", teama_score),
           teamb_score = ifelse(is.na(teamb_score), "", teamb_score)) %>%
    select(Date = date, Time = time, TeamA = teama, TeamB = teamb,
           TeamA_Score = teama_score, TeamB_Score = teamb_score,
           Scoresheet_Link = scoresheet_link, Notes = note)
}