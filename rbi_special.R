games <- c(4,5,6,7)
#caesars odds
games_odds <- c(600,250,190,200)
implied_probability <- function(odds){
  if (is.na(odds)){
    return(NA)}
  if (odds < -100){
    return(-odds/(-odds+100))}
  if (odds >= 100){
    return(100/(100+odds))}
  else{
    return(NA)}}
library(tidyverse)
games_implied_probabilities <- unlist(map(games_odds, implied_probability))
games_probabilities <- games_implied_probabilities/(sum(games_implied_probabilities))
#going to count how many times Ohtani got x amount of rbis in the 2026 regular season
games <- 139
rbis <- c(0,1,2,3,4,5)
times <- c(NA,36,10,4,2,1)
times[1] <- games-sum(times, na.rm = T)
rbi_probabilities <- times/games
rbi_simulation <- function(){
  