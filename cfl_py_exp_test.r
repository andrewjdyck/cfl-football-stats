


# CFL Pythagorean expectation

# This CSV file is sourced from CFLStats.ca.
# The CSV can be re-loaded from the API with the script get_aggregate_cfl_season_data.R
raw_data <- read.csv('./data/cfl_agg_season_data.csv', as.is=TRUE)

# in baseball, sigma = std(runs scored)/average(runs scored)
# Calculate the optimal exponent for Pythagorean Expectation in the CFL based on data
sd_pts <- sd(raw_data$PointsFor)
mean_pts <- mean(raw_data$PointsFor)
sigma <- sd_pts/mean_pts
py_exponent <- 2/(sqrt(pi)*sigma)

# PyExpectation Test
# Test the calculated pythagorean exponent to that used in NFL football
nfl_py_exponent <- 2.37
season_data <- raw_data[which(raw_data$Year != 2014),]
season_data$GP <- season_data$Wins + season_data$Losses + season_data$Ties
season_data$WinPct <- season_data$Wins/season_data$GP
season_data$PyExp <- season_data$PointsFor^nfl_py_exponent/(season_data$PointsFor^nfl_py_exponent + season_data$PointsAgainst^nfl_py_exponent)
season_data$PyExpNew <- season_data$PointsFor^py_exponent/(season_data$PointsFor^py_exponent + season_data$PointsAgainst^py_exponent)

# A look at the results of the test
test_results <- season_data[, c("Team", "Year", "WinPct", "PyExp", "PyExpNew")]
test_results$py_error <- (test_results$PyExp-test_results$WinPct)^2
test_results$pynew_error <- (test_results$PyExpNew-test_results$WinPct)^2
error <- data.frame(
  PyExp=sum(test_results$py_error),
  PyExpNew=sum(test_results$pynew_error)
)

