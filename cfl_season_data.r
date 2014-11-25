
library("XML")
season <- 2013
schedule_url <- paste("http://cfl.ca/schedule/year/", season, "/time_zone/0", sep="")
schedule_doc <- htmlParse(schedule_url)
schedule_tabs <- readHTMLTable(schedule_doc, stringsAsFactors=FALSE)
statsgame_links <- grep("statsGame", 
                        unlist(xpathApply(schedule_doc, "//a", xmlGetAttr, "href")), 
                        value=TRUE)

season_games <- schedule_tabs[[2]]

href <- statsgame_links[1]

getGameStats <- function(href) {
  url <- paste("http://cfl.ca/", href, sep="")
  doc <- htmlParse(url)
  return(readHTMLTable(doc, stringsAsFactors=FALSE))
}

single_game <- getGameStats(href)
getGameId <- function(href) {
  split <- strsplit(href, "/")
  return(split[[1]][length(split[[1]])])
}
game_id <- getGameId(href)


home_cols <- seq(4, 20, 2)
away_cols <- seq(3, 20, 2)


get_team_stats <- function (column_id) {
  t <- single_game[[column_id]][2:length(single_game[[column_id]])]
  if (column_id == 11 | column_id == 12) {
    t$FG_att <- unlist(lapply(strsplit(t$FG, "/ "), function(x) x[2]))
    t$FG <- unlist(lapply(strsplit(t$FG, "/ "), function(x) x[1]))
    t$XP_att <- unlist(lapply(strsplit(t$XP, "/ "), function(x) x[2]))
    t$XP <- unlist(lapply(strsplit(t$XP, "/ "), function(x) x[1]))
    r <- t
  } else {
    if (length(t[,1]) == 1) {
      r <- t
    } else {
    r <- data.frame(t(colSums(data.frame(sapply(names(t), function(x) as.numeric(t[,x]))))))
    }
    names(r) <- sapply(names(t), function(x) paste(
      strsplit(names(single_game)[column_id], " ")[[1]][2], 
      "_", 
      x, 
      sep="")
    )
  }
  return(r)
}
home_stats <- data.frame(t(unlist(lapply(home_cols, get_team_stats))), stringsAsFactors=FALSE)
away_stats <- data.frame(t(unlist(lapply(away_cols, get_team_stats))), stringsAsFactors=FALSE)
home_stats$game_id <- game_id
away_stats$game_id <- game_id


scores <- single_game[[2]][,c(1,2,3,4,5,6,7)]
names(scores) <- c("team", "Q1", "Q2", "Q3", "Q4", "OT", "Final")
scores$date <- as.Date(names(single_game[[2]][1]), "%a, %b %d, %Y")
scores$game_id <- game_id
away_scores <- scores[1,]
home_scores <- scores[2,]




games <- season_games[, c(1,2,3,4,5,6,10)]
names(games)[c(3,5)] <- c("AwayPts", "HomePts")
games <- games[which(is.na(games$HomePts)==FALSE),]
games$Date <- as.Date(paste(games$Date, season), "%a %b %d %Y")


#url2 <- "http://liveplay.cflcentral.com/LPFiles/5_2013_cfllive_scoreboard.html"
#doc2 <- htmlParse(url2)
#tab2 <- readHTMLTable(doc2)

#url <- "http://stats.cfldb.ca/league/cfl/schedule/2013/06/14/ssk-at-edm/"
#doc <- htmlParse(url)
#tab <- readHTMLTable(doc)




