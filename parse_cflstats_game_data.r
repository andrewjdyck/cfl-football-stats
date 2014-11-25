

# CFLSTATS.ca
library("sqldf")
library("XML")
library("jsonlite")
library("plyr")


# function to parse individual game data
parse_game_json <- function(game_num) {
  game_url <- paste(url_base, "/game/", game_num, ".json?type=TeamStats", sep="")
  game_json <- fromJSON(readLines(game_url, warn=FALSE))
  return(game_json)
}

# a function for parsing a season of data
parse_season_data <- function(season) {
  url_base <- "http://www.cflstats.ca"
  schedule_doc <- htmlParse(paste(url_base, "/schedule/", season, sep=""))
  game_links <- grep("/game/", 
                     unlist(xpathApply(schedule_doc, "//a", xmlGetAttr, "href")), 
                     value=TRUE)  
  
  # setup loop
  i <- 2
  home <- list(season)
  away <- list(season)
  
  # loop through season games
  for (game in game_links) {
    game_num <- strsplit(game, '/')[[1]][3]
    game_json <- parse_game_json(game_num)
    home[[i]] <- data.frame(game_json$HomeStats, stringsAsFactors=FALSE)
    away[[i]] <- data.frame(game_json$AwayStats, stringsAsFactors=FALSE)
    i <- i + 1
  } # end for loop
  
  # prep output of function
  home_out <- ldply(home[2:length(home)])
  away_out <- ldply(away[2:length(away)])
  
  return(list(home=home_out, away=away_out))
} # end season function


season <- 2013

output_2013 <- parse_season_data(2013)
write.csv(output_2013[[1]], "./cfl/home_2013.csv")
write.csv(output_2013[[2]], "./cfl/away_2013.csv")


for (season in 2009:2013) {
  
}

