series_length <- c(4,5,6,7)
series_length_odds <- c(600,250,190,200)
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
series_length_implied_probabilities <- unlist(map(series_length_odds, implied_probability))
series_length_probabilities <- series_length_implied_probabilities/(sum(series_length_implied_probabilities))
#going to count how many times player got _ rbis in 2026 regular season
games <- 139
rbis <- c(0,1,2,3,4,5,6)
sum(rbis)

times <- c(NA,36,10,4,2,1,0)
times[1] <- games-sum(times, na.rm = T)
rbi_probabilities <- times/games
series_rbis <- list()
simulation_n <- 0
repeat{
  n_games <- sample(x = series_length, size = 1, prob = series_length_probabilities)
  game_1_rbis <- sample(x = rbis, size = 1, prob = rbi_probabilities)
  games_rbis <- list(game_1_rbis)
  game <- 1
  repeat{
    game <- game+1
    if (game > n_games){
      break}
    games_rbis <- c(games_rbis,sample(x = rbis, size = 1, prob = rbi_probabilities))}
  simulation_n <- simulation_n+1
  if (simulation_n > 10000){
    break}
  series_rbis <- c(series_rbis,sum(unlist(games_rbis)))}
series_rbis_simulations <- data.frame(simulation_n = 1:10000,rbis = unlist(series_rbis))
