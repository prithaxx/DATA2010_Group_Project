matches <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")
worldcup <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")

# Shorterning the dataset to what the code needs
library(tidyverse)
library(gridExtra)

winners <- matches_rename |>
  select(year, home_team, away_team, home_score, away_score, outcome, stage) |>
  mutate(winning_team = ifelse(outcome == "H", home_team, away_team)) |>
  mutate(winning_score = ifelse(outcome == "H", home_score, away_score))

split_data <- split(winners, f = winners$stage)
group <- split_data$Group
knockout <- split_data$Knockout

country = unique(knockout$winning_team)
group = subset(group, winning_team %in% country)

# FUNCTION TO FIND OUT THE TOTAL NUMBER OF GOALS SCORED BY EACH TEAM
score <- function(df){
  teams_won <- unique(df$winning_team)
  teams_score <- vector(length = length(teams_won))
  
  count = 0
  for(i in 1:length(teams_won)){
    for(j in 1:nrow(df)){
      if(teams_won[i] == df$winning_team[j]){
        count = count + 1
      }
    }
    teams_score[i] = count
    count = 0
  }
  
  total_goals <- data.frame(teams_won, teams_score)
  return(total_goals)
}

# FUNCTION TO COMPARE IF COUNTRIES PLAY BETTER IN GROUP OR KNOCKOUTS
comparison <- function(df, header){
  total_goals <- score(df)
  
  total_goals |>
    mutate(teams_won = reorder(teams_won, teams_score)) |>
    ggplot(aes(teams_won, teams_score)) +
    geom_bar(stat = "identity") +
    labs(x = "Total goals scored", y = "Teams", title = header) +
    coord_flip()
}

grid.arrange(comparison(group, "Group Games"), comparison(knockout, "Knockout Games"))


# JOIN THE DATASETS 
group_score <- score(group)
knockout_score <- score(knockout)

final_score <- group_score |>
  inner_join(knockout_score, by = "teams_won")

# CORRELATION BETWEEN KNOCKOUT GOALS AND GROUP GOALS
final_score |>
  summarise(corr = cor(teams_score.x, teams_score.y))
  
