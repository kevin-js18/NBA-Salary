path <- file.path("C:/Users/aet/Desktop/School/R projects/NBA Salary/nba_salary.txt")
nba_url <- "http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=1&LeagueID=00&Season=2016-17"

nba_salaries <- read.table(path, sep = ",", skip = 1, stringsAsFactors = FALSE)
all_nba_data <- fromJSON(nba_url)

nba_df <- data.frame(all_nba_data$resultSets$rowSet, stringsAsFactors = FALSE)
nba_df <- nba_df[ , -2]
nba_df <- nba_df[ , -(3:12)]

nba_salaries$V2 <- sapply(strsplit(nba_salaries$V2, "\\\\"), "[", 1)

nba_salaries <- nba_salaries[ , -(5:11)]

names(nba_salaries) <- c("Rank", "Player", "Team", "16-17 Salary")
names(nba_df) <- c("ID", "Name")

nba_salaries$`16-17 Salary` <- sub("^\\$([0-9]+)", "\\1",nba_salaries$`16-17 Salary`)
nba_salaries$`16-17 Salary` <- sapply(nba_salaries$`16-17 Salary`, function(x) { as.numeric(x) / 1000000 })

player_ids <- c()
PIE_rating <- c()

for (i in 1:length(nba_salaries$Player)) {
  if (length(grep(nba_salaries$Player[i], nba_df$Name)) == 0) {
    player_ids[i] <- NA
  }
  else {
    player_ids[i] <- nba_df[grep(nba_salaries$Player[i], nba_df$Name), 1]
  }
}

for (i in 1:length(player_ids)) {
  if (is.na(player_ids[i])) {
    PIE_rating[i] <- NA
  }
  else {
    player_url <- paste("http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=", player_ids[i],"&SeasonType=Regular+Season", sep = "")
    player_data <- fromJSON(player_url)
    if (length(player_data$resultSets$rowSet[[2]]) == 0) {
      PIE_rating[i] <- NA
    }
    else {
      player_df <- data.frame(player_data$resultSets$rowSet, stringsAsFactors = FALSE)
      PIE_rating[i] <- player_df$X7.1
    }
  }
}

PIE_rating <- as.numeric(PIE_rating)

final_df <- data.frame(nba_salaries, PIE_rating)

non_na_index <- which(!is.na(PIE_rating))

plot(final_df$PIE_rating[non_na_index], final_df$X16.17.Salary[non_na_index], xlab = "Player Impact Estimate", ylab = "Salary (in millions)", main = "Player Salary vs. Player Impact Estimate in the NBA")
mtext("2016 - 2017 Regular Season, as of 11/05/2016") 

ggplot(final_df, aes(y = X16.17.Salary, x = factor(Team))) +
  geom_boxplot() +
  labs(x = "Team", y = "Salary (in millions)") +
  ggtitle("2016-2017 Salary ranges for NBA teams")
