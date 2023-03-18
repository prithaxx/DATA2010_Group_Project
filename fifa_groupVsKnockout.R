match <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")
worldcup <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")

matches_rename = match

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
