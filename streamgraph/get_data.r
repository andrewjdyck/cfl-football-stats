
library('jsonlite')
team <- 'TOR'
get_team_season_points <- function(team) {
  url <- paste('http://www.cflstats.ca/team', team, '2014.json', sep='/')                                        
  url_return <- fromJSON(readLines(url, warn=FALSE))$Games
  home <- split(url_return[, c('WeekNumber', 'HomeScore')], url_return$HomeTeam)[[team]]
  away <- split(url_return[, c('WeekNumber', 'AwayScore')], url_return$AwayTeam)[[team]]
  names(home) <- c('WeekNumber', 'Score')
  names(away) <- c('WeekNumber', 'Score')
  temp <- rbind(home, away)
  temp <- temp[temp$WeekNumber<=20,]
  temp <- temp[order(temp$WeekNumber),]
  weeks <- data.frame(WeekNumber=1:20)
  weeks$v <- temp$WeekNumber[pmatch(weeks$WeekNumber, temp$WeekNumber)]
  out <- merge(weeks, temp, all.x=TRUE)
  out[is.na(out$Score), 'Score'] <- 0
  out$v <- NULL
  out$team <- team
  return(out)
}

teams <- c('BC', "SSK", "CGY", "EDM", "WPG", "HAM", 'TOR', 'MTL', 'ORB')
df <- do.call('rbind', lapply(teams, get_team_season_points))
names(df) <- c('date', 'value', 'key')

df$date <- as.character(df$date)
df$date[nchar(df$date)<2] <- paste('0', df$date[nchar(df$date)<2], sep='')
df$date <- as.Date(paste(df$date, '012014', sep=''), '%d%m%Y')
df <- df[,c('key', 'value', 'date')]
write.csv(test, '~/Documents/cfl-football-stats/streamgraph/data3.csv', 
          row.names=FALSE,
          quote=FALSE)

