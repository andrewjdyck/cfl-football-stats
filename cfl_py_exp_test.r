


# CFL Pythagorean expectation

# This CSV file is sourced from CFLStats.ca.
# The CSV can be re-loaded from the API with the script get_aggregate_cfl_season_data.R
season_data <- read.csv('./data/cfl_agg_season_data.csv', as.is=TRUE)

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

