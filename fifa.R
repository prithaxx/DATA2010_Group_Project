matches <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")
worldcup <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")

# GROUP VS KNOCKOUT
# Shorterning the dataset to what the code needs
library(tidyverse)
library(gridExtra)


matches_rename = matches

for(i in seq(1, length(matches_rename$stage)))
{
  if(matches_rename[i,]$stage %in% c("Group 1", "Group 2", "Group 3",
                                     "Group 4", "Group 5","Group 6", 
                                     "Group D ", " Group A",
                                     "Group A", "Group B", "Group C",
                                     "Group D", "Group E", "Group F",
                                     "Group G", "Group H", "Group I"))
  {
    matches_rename[i,]$stage = "Group"
  }
  
  if(matches_rename[i,]$stage %in% c("Round of 16", "Quarterfinals",
                                     "Semifinals", "Third place",
                                     "Final", "Final Round"))
  {
    matches_rename[i,]$stage = "Knockout"
  }
}

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
comparison <- function(df){
  total_goals <- score(df)
  
  total_goals |>
    mutate(teams_won = reorder(teams_won, teams_score))
}

total_group_goals <- comparison(group)
total_knockout_goals <- comparison(knockout)

total_fifa_goals <- total_group_goals |>
  inner_join(total_knockout_goals, by = "teams_won") |>
  rename("group_goals" = "teams_score.x") |>
  rename("knockout_goals" = "teams_score.y")

dataset <- tidyr::gather(total_fifa_goals, key = "score_type", value = "score", 
                    group_goals, knockout_goals)

ggplot(dataset, aes(y = teams_won, x = score, fill = score_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = score), size = 3, position = position_dodge(width = 0.9), vjust = 0.5) +
  labs(title = "Goals by Country", x = "Goals", y = "Country", fill = "")


# JOIN THE DATASETS 
group_score <- score(group)
knockout_score <- score(knockout)

final_score <- group_score |>
  inner_join(knockout_score, by = "teams_won")

final_score <- final_score |>
  rename("group_score" = "teams_score.x") |>
  rename("knockout_score" = "teams_score.y")

# AVERAGE GOALS SCORED BY THESE COUNTRIES 
final_score <- final_score |>
  mutate(avg_score = (group_score + knockout_score)/2)

# CORRELATION BETWEEN KNOCKOUT GOALS AND GROUP GOALS
final_score |>
  summarise(corr = cor(group_score, knockout_score))


# HOME VS AWAY
# Group matches: home vs away goals for every country (all years)
Home_Goals_Group = group %>% group_by(home_team) %>% 
  group_by(year, .add = TRUE) %>%
  summarise(home_goals = sum(home_score), H_goals_against = sum(away_score),
            H_total_matches = n(), year) %>% 
  rename(country = home_team)

Away_Goals_Group = group %>% group_by(away_team) %>% 
  group_by(year, .add = TRUE) %>%
  summarise(away_goals = sum(away_score), A_goals_against = sum(home_score),
            A_total_matches = n()) %>% 
  rename(country = away_team)

Group_Goals <- Home_Goals_Group |>
  inner_join(Away_Goals_Group, by = c("country", "year"))

# Knockout matches: home vs away goals for every country (all years)
Home_Goals_Knockout = knockout %>% group_by(home_team) %>% 
  group_by(year, .add = TRUE) %>%
  summarise(home_goals = sum(home_score), H_goals_against = sum(away_score),
            H_total_matches = n()) %>% 
  rename(country = home_team)

Away_Goals_Knockout = knockout %>% group_by(away_team) %>% 
  group_by(year, .add = TRUE) %>%
  summarise(away_goals = sum(away_score), A_goals_against = sum(home_score),
            A_total_matches = n()) %>% 
  rename(country = away_team)

Knockout_Goals = Home_Goals_Knockout |>
  inner_join(Away_Goals_Knockout, by = c("country", "year"))



# Calculating additional stats:
Group_Goals = Group_Goals |> 
  mutate(total_goals = home_goals + away_goals,
         total_goals_against = H_goals_against + A_goals_against,
         diff = total_goals - total_goals_against,
         total_matches = H_total_matches + A_total_matches,
         goals_per_game = total_goals/total_matches,
         goals_against_per_game = total_goals_against/total_matches)

Knockout_Goals = Knockout_Goals |> 
  mutate(total_goals = home_goals + away_goals,
         total_goals_against = H_goals_against + A_goals_against,
         diff = total_goals - total_goals_against,
         total_matches = H_total_matches + A_total_matches,
         goals_per_game = total_goals/total_matches,
         goals_against_per_game = total_goals_against/total_matches)

# Normalization of Goals per World Cup:
norm_goals_group = Group_Goals %>% 
  group_by(year) %>% summarize(mean_gpg = mean(goals_per_game),
                               sd_gpg = sd(goals_per_game),
                               mean_gapg = mean(goals_against_per_game),
                               sd_gapg = sd(goals_against_per_game))

Group_Goals = Group_Goals %>% 
  left_join(norm_goals_group, by = "year")

norm_goals_knock = Knockout_Goals %>% 
  group_by(year) %>% summarize(mean_gpg = mean(goals_per_game),
                               sd_gpg = sd(goals_per_game),
                               mean_gapg = mean(goals_against_per_game),
                               sd_gapg = sd(goals_against_per_game))


Knockout_Goals = Knockout_Goals %>% 
  left_join(norm_goals_knock, by = "year")

Knockout_Goals <- na.omit(Knockout_Goals)

# SCORING SYSTEM
# group
Norm_scores_group = Group_Goals %>% 
  dplyr::select(c(country, year, goals_per_game, goals_against_per_game,
                  mean_gpg, sd_gpg, mean_gapg, sd_gapg)) %>% 
  mutate(norm_goals_per_game = (goals_per_game - mean_gpg)/sd_gpg,
         norm_goals_against_per_game = 
           (goals_against_per_game - mean_gapg)/sd_gapg,
         score = norm_goals_per_game - norm_goals_against_per_game) %>% 
  dplyr::select(c(country, year, score)) %>% arrange(desc(score))

Norm_scores_group <- Norm_scores_group |>
  group_by(country) |>
  summarise(score = mean(score))

head(Norm_scores_group, 10)

# knockout
Norm_scores_knock = Knockout_Goals %>% 
  dplyr::select(c(country, year, goals_per_game, goals_against_per_game,
                  mean_gpg, sd_gpg, mean_gapg, sd_gapg)) %>% 
  mutate(norm_goals_per_game = (goals_per_game - mean_gpg)/sd_gpg,
         norm_goals_against_per_game = 
           (goals_against_per_game - mean_gapg)/sd_gapg,
         score = norm_goals_per_game - norm_goals_against_per_game) %>% 
  dplyr::select(c(country, year, score)) %>% arrange(desc(score))

Norm_scores_knock <- Norm_scores_knock |>
  group_by(country) |>
  summarise(score = mean(score))


head(Norm_scores_knock, 10)
Norm_scores_knock <- na.omit(Norm_scores_knock)

# Filtering out the countries from group which qualify in the knockouts
teams = unique(Norm_scores_knock$country)
Norm_scores_group = subset(Norm_scores_group, country %in% teams)


# Joining the two normalized dataset 
Norm_fifa_scores <- Norm_scores_group |>
  full_join(Norm_scores_knock, by = "country") |>
  rename("group_score" = "score.x") |>
  rename("knockout_score" = "score.y")

# Finding correlation between group score and knockout score
cor(Norm_fifa_scores$group_score, Norm_fifa_scores$knockout_score)

# BARPLOT
df <- tidyr::gather(Norm_fifa_scores, key = "score_type", value = "score", 
                    group_score, knockout_score)

ggplot(df, aes(y = country, x = score, fill = score_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.3f", score)), size = 3, position = position_dodge(width = 0.9), vjust = 0.5) +
  labs(title = "Scores by Country", x = "Score", y = "Country", fill = "")


# Matched pair test between the scores of group and knockout games
test <- t.test(Norm_fifa_scores$group_score, Norm_fifa_scores$knockout_score, 
               paired = TRUE)
test


# Checking if the OVERALL SCORING SYSTEM can predict the world cup winners
# (first, second, third and fourth place) from the worldcup dataset
count <- 0
total <- 0

country <- c()
year <- c()
position <- c()

pred_first_four <- data.frame(country, year, position)

for(i in 1:nrow(Norm_scores_fifa)){
  
  for(j in 1:nrow(worldcup)){
    
    if(Norm_scores_fifa$score[i] >= 1){
      
      if(Norm_scores_fifa$year[i] == worldcup$year[j]){
        
        if(Norm_scores_fifa$country[i] == worldcup$winner[j] ||
           Norm_scores_fifa$country[i] == worldcup$second[j] ||
           Norm_scores_fifa$country[i] == worldcup$third[j] ||
           Norm_scores_fifa$country[i] == worldcup$fourth[j]){
          
          country <- append(country, Norm_scores_fifa$country[i])
          year <- append(year, Norm_scores_fifa$year[i])
          position <- append(position, worldcu)
        }
      }
      
    }
  }
  total = total + 1
}