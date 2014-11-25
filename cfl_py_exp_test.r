

# CFL Pythagorean expectation

# load libraries
library("jsonlite")
library("plyr")


url <- "http://www.cflstats.ca/team/BC.json"
url_base <- "http://www.cflstats.ca/team/"

team <- "BC"
teams <- c('BC', "SSK", "CGY", "EDM", "WPG", "HAM", 'TOR', 'MTL')

teams_data <- list(1)
for (i in 2:(length(teams)+1)) {
  t <- i-1
  team_url <- paste(url_base, teams[t], ".json", sep="")
  team_json <- fromJSON(readLines(team_url, warn=FALSE))
  team_stats <- team_json$Stats[, c("Team", "Year", "Wins", "Losses", "Ties", "PointsFor", "PointsAgainst")]
  teams_data[[i]] <- team_stats  
}
season_data <- ldply(teams_data[2:length(teams_data)])

# in baseball, sigma = std(runs scored)/average(runs scored)
sd_pts <- sd(season_data$PointsFor)
mean_pts <- mean(season_data$PointsFor)
sigma <- sd_pts/mean_pts
py_exponent <- 2/(sqrt(pi)*sigma)

# PyExpectation Test
season_data <- season_data[which(season_data$Year != 2014),]
season_data$GP <- season_data$Wins + season_data$Losses + season_data$Ties
season_data$WinPct <- season_data$Wins/season_data$GP
season_data$PyExp <- season_data$PointsFor^2.37/(season_data$PointsFor^2.37 + season_data$PointsAgainst^2.37)
season_data$PyExpNew <- season_data$PointsFor^py_exponent/(season_data$PointsFor^py_exponent + season_data$PointsAgainst^py_exponent)
head(season_data)

test_results <- season_data[, c("Team", "Year", "WinPct", "PyExp", "PyExpNew")]
test_results$py_error <- (test_results$PyExp-test_results$WinPct)^2
test_results$pynew_error <- (test_results$PyExpNew-test_results$WinPct)^2
error <- data.frame(
  PyExp=sum(test_results$py_error),
  PyExpNew=sum(test_results$pynew_error)
)

