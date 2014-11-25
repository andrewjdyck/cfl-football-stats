# Advanced Stats for Canadian Football #

This repository uses the R language and the CFLStats.ca API to look at the Canadian Football League (CFL). 

## Repository contents ##
### R Scripts ####

The repo contains the following:

- get_aggregate_cfl_season_data.R
  - Contains a function for pulling data from the CFLStats.ca API. The output dataset consists of the number of wins and losses for each team by season along with points for and against. This data is sufficient to estimate the [Pythagorean Expectation](http://en.wikipedia.org/wiki/Pythagorean_expectation) for CFL football.
- cfl_py_exp.R
  - This file computes the optimal exponent for use in Pythagorean Expectation based on the observed data. The win performance based on this algorithm is then compared to what is expected if the exponent from NFL football is used.

### Data ###

- cfl_agg_season_data.csv
  - A CSV pulled from the CFLStats.ca API using the R script get_aggregate_cfl_season_data.R
  - Contains the following fields:
    - c("Team", "Year", "Wins", "Losses", "Ties", "PointsFor", "PointsAgainst")

## More Info? ##

If you'd like more information about this repository or if you're interested in contributing, contact me <info@andrewdyck.com>