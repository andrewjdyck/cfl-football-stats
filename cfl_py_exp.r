

library("XML")
library("sqldf")
library("RMySQL")
mydb = dbConnect(MySQL(), user='cfldata', password='cfldata1', dbname='cfl_data', host='dbs.andrewdyck.com')

# get the data straight from cfl.ca 
# Years go from 2005 to 2013
year <- 2013
url <- paste("http://cfl.ca/schedule/year/", year, "/time_zone/0", sep="")
doc <- htmlParse(url)
tab <- readHTMLTable(doc)
data <- tab[[2]]
names(data)[c(2,3,4,5)] <- c("AwayTeam", "AwayPts", "HomeTeam", "HomePts")
web_data <- data[,c("Date", "AwayTeam", "AwayPts", "HomeTeam", "HomePts","Time","Attendance")]
web_data$Date <- as.Date(paste(web_data$Date, 2012, sep=" "), "%a %b %d %Y")
web_data <- web_data[is.na(web_data$HomeTeam)==FALSE, ]

web_data$HomePts <- as.numeric(web_data$HomePts)
web_data$AwayPts <- as.numeric(web_data$AwayPts)
web_data$HomeTeam <- as.character(web_data$HomeTeam)
web_data$AwayTeam <- as.character(web_data$AwayTeam)
web_data$GameId <- c(1:length(web_data$Date))

# teams <- unique(web_data$HomeTeam)
# for (i in 1:length(teams)) {
#   
#   
# }


home <- web_data
home$IsHome <- 1
#home$AwayPtsAgainst <- home$HomePts
#home$AwayPtsFor <- home$AwayPts
names(home)[c(3, 4, 5)] <- c("PtsAgainst", "Team", "PtsFor")
home$AwayTeam <- NULL
away <- web_data
away$IsHome <- 0
names(away)[c(2, 3, 5)] <- c("Team", "PtsFor", "PtsAgainst")
away$HomeTeam <- NULL

d <- rbind(home, away)
d$Win <- 0
d$Win[d$PtsFor>d$PtsAgainst]<- 1

teams <- unique(d$Team)
d$CumPtsFor <- NA
d$CumPtsAgainst <- NA
d$CumWins <- NA
d$GP <- NA
d <- d[order(d$Team, d$GameId),]
for (i in 1:length(teams)) {
  d$CumPtsFor[d$Team==teams[i]] <- cumsum(d$PtsFor[d$Team==teams[i]])
  d$CumPtsAgainst[d$Team==teams[i]] <- cumsum(d$PtsAgainst[d$Team==teams[i]])
  d$CumWins[d$Team==teams[i]] <- cumsum(d$Win[d$Team==teams[i]])
  d$GP[d$Team==teams[i]] <- 1:length(d$Team[d$Team==teams[i]])
}
d$PyExp <- NA
d$PyWins <- NA
d$PyExp <- d$CumPtsFor^2.73/(d$CumPtsFor^2.73+d$CumPtsAgainst^2.73)
d$PyWins <- round(d$PyExp*d$GP, 1)
d$Luck <- d$CumWins-d$PyWins
d$IsAway <- 1-d$IsHome
t <- sqldf("SELECT a.*, b.PyWins as OppPyWins, b.PyExp as OppPyExp FROM d a INNER JOIN d b ON a.GameId = b.GameId AND a.IsHome = b.IsAway")
t <- t[order(t$Team, t$GameId),]
t$PyExpLag <- NA
t$LuckLag <- NA
t$OppPyExpLag <- NA
for (i in 1:length(teams)) {
  team <- teams[i]
  l <- length(t$Team[t$Team==team])
  ll <- l-1
  t$PyExpLag[t$Team==team][2:l] <- t$PyExp[t$Team==team][1:ll]
  t$OppPyExpLag[t$Team==team][2:l] <- t$OppPyExp[t$Team==team][1:ll]
  t$LuckLag[t$Team==team][2:l] <- t$Luck[t$Team==team][1:ll]
}

model <- glm(Win ~ IsHome + PyExp + Luck + OppPyWins, data=t, family="binomial")
model2 <- glm(Win ~ IsHome + PyExpLag + OppPyExpLag, data=t, family="binomial")
model3 <- glm(Win ~ IsHome + PyExpLag + LuckLag + OppPyExpLag -1, data=t, family="binomial")
t$fit <- 1/(1+exp(model3$coefficients[1]*t$IsHome+model3$coefficients[2]*t$PyExpLag+model3$coefficients[3]*t$LuckLag+model3$coefficients[4]*t$OppPyExpLag))
t$ModelWin <- 0
t$ModelWin[t$fit > 0.5] <- 1
t[t$Team=="BC", c("Team", "IsHome", "PyExp", "PyExpLag", "OppPyExp", "OppPyExpLag", "LuckLag", "Luck", "fit", "ModelWin", "Win")]

correct <- length(t$ModelWin[t$ModelWin==t$Win & is.na(t$fit)==FALSE])
games <- length(t$ModelWin[is.na(t$fit)==FALSE])
correctPct <- correct/games

one <- t[t$IsHome==1, c("Team", "PtsFor", "PtsAgainst", "PyExpLag", "fit", "GameId")]
two <- t[t$IsHome==0, c("Team", "PyExpLag", "fit", "GameId")]
names(one) <- c("HomeTeam", "HomePts", "AwayPts", "HomePyExpLag", "HomeWinProb", "GameId")
names(two) <- c("AwayTeam", "AwayPyExpLag", "AwayWinProb", "GameId")

tt <- sqldf("SELECT a.*, b.* FROM one a INNER JOIN two b ON a.GameId = b.GameId")
tt$Winner <- NA
tt$Winner[tt$HomePts>tt$AwayPts] <- tt$HomeTeam[tt$HomePts>tt$AwayPts]
tt$Winner[tt$HomePts<tt$AwayPts] <- tt$AwayTeam[tt$HomePts<tt$AwayPts]
tt$PredWinner <- NA
tt$PredWinner[is.na(tt$HomeWinProb)==FALSE & tt$HomeWinProb > tt$AwayWinProb] <- tt$HomeTeam[is.na(tt$HomeWinProb)==FALSE & tt$HomeWinProb > tt$AwayWinProb]
tt$PredWinner[is.na(tt$HomeWinProb)==FALSE & tt$HomeWinProb < tt$AwayWinProb] <- tt$AwayTeam[is.na(tt$HomeWinProb)==FALSE & tt$HomeWinProb < tt$AwayWinProb]
tt[, c("HomeTeam", "AwayTeam", "HomeWinProb", "AwayWinProb", "PredWinner", "Winner")]
length(tt$PredWinner[tt$PredWinner==tt$Winner])/length(tt$PredWinner)


data <- read.table("~/Dropbox/business/football/data.csv", sep=",", header=TRUE, as.is=TRUE)

data$AwayTeam <- gsub(" ", "", data$AwayTeam)
data$HomeTeam <- gsub(" ", "", data$HomeTeam)


# This will be in a loop of some sort
createData <- function(Week) {
d1 <- data[data$Week<=Week,
  c("Date", "KickOffTime", "Attendance", 
    "Week", "AwayTeam", "HomeTeam", "AwayPts", "HomePts")]
d2 <- data[data$Week<=Week,
  c("Date", "KickOffTime", "Attendance", 
    "Week", "HomeTeam", "AwayTeam", "HomePts", "AwayPts")]
d1$HomeAway <- "Away"
d2$HomeAway <- "Home"
names(d1)[c(5,6,7,8)]<-c("Team", "Opponent", "PointsFor", "PointsAgainst")
names(d2)[c(5,6,7,8)]<-c("Team", "Opponent", "PointsFor", "PointsAgainst")

dd <- rbind(d1,d2)
dd$Win <- 0
dd$Win[dd$PointsFor>dd$PointsAgainst]<- 1
dd$Loss <- 1 - dd$Win

agg <-  aggregate(cbind(PointsFor, PointsAgainst, Win, Loss) ~ Team, data=dd, FUN=sum)
agg$Week <- Week

agg$PyExp <- agg$PointsFor^2.73/(agg$PointsFor^2.73+agg$PointsAgainst^2.73)

return(agg)
}



results <- data.frame()
results$Team <- as.character()
results$PointsFor <- as.numeric()
results$PointsAgainst <- numeric()
results$Wins <- numeric()
results$Loss <- numeric()
results$Week <- numeric()
results$PyExp <- numeric()

for (i in 1:11) {

  A <- createData(i)
  N <- length(results$Team)+1
  N1 <- N + length(A$Team)-1
  
  results[N:N1,]<-A
}

results$GP <- results$Wins+results$Loss
results$PyWins <- round(results$PyExp*results$GP,1)
results$luck <- results$Wins-results$PyWins

xrange <- range(results$Week)
yrange <- range(results$luck)
teams <- unique(results$Team)
plot(xrange, yrange, type="n", xlab="Week",
     ylab="Luck" )
colors <- rainbow(8) 
linetype <- c(1:8) 
plotchar <- seq(18,18+8,1)
for (i in 1:8) {
  t <- teams[i]
  r <- results[results$Team==t,]
  lines(r$Week, r$luck, type="b", lwd=1.5,
    lty=linetype[i], col=colors[i], pch=plotchar[i])
}

legend(xrange[1], yrange[2], teams, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Teams")
