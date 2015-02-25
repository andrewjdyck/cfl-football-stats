
library('jsonlite')

get_team_season_points <- function(team) {
  url <- paste('http://www.cflstats.ca/team', team, '2014.json', sep='/')                                        
  df <- fromJSON(readLines(url, warn=FALSE))$Games
  home <- split(df[, c('WeekNumber', 'HomeTeam', 'HomeScore')], df$HomeTeam)[[team]]
  away <- split(df[, c('WeekNumber', 'AwayTeam', 'AwayScore')], df$AwayTeam)[[team]]
  names(home) <- c('WeekNumber', 'Team', 'Score')
  names(away) <- c('WeekNumber', 'Team', 'Score')
  out <- rbind(home, away)
  return(out[order(out$WeekNumber),])
}

teams <- c('BC', "SSK", "CGY", "EDM", "WPG", "HAM", 'TOR', 'MTL', 'ORB')
ll <- lapply(teams, get_team_season_points)
df <- do.call('rbind', ll)

names(df) <- c('date', 'key', 'value')
write.csv(df, '~/Documents/cfl-football-stats/streamgraph/data.csv', row.names=FALSE)

as.Date(paste(cgy$WeekNumber, '01', '2014', sep=''), '%d%m%Y')
