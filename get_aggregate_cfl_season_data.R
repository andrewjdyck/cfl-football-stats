
# This script is used to pull aggregate season data from CFLStats.ca

# load libraries
library("jsonlite")
library("plyr")

# A function to pull the aggregate season stats for a CFL team
get_season_stats_by_team <- function(team_abrv) {
  url_base <- 'http://www.cflstats.ca/team/'
  team_url <- paste(url_base, team_abrv, '.json', sep='')
  team_json <- fromJSON(readLines(team_url, warn=FALSE))
  column_names <- c("Team", "Year", "Wins", "Losses", "Ties", "PointsFor", "PointsAgainst")
  return( team_json$Stats[, column_names] ) 
}

# A list of all teams in the CFL
teams <- c('BC', "SSK", "CGY", "EDM", "WPG", "HAM", 'TOR', 'MTL', 'ORB')
season_data <- ldply(lapply(teams, get_season_stats_by_team))

# output the aggregate season data for analysis
write.csv(
  season_data,
  './data/cfl_agg_season_data.csv',
  row.names=FALSE
)
