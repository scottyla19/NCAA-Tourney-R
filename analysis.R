library(tidyverse)
conf <- read.csv("TeamConferences.csv")
teams  <- read.csv("Teams.csv")

# join team, coach, and conf data into one dataframe
allTeams <- read.csv("TeamCoaches.csv") %>% merge( teams[ , c("TeamID", "TeamName")], by="TeamID") %>%
        merge(conf, by = c("Season", "TeamID"))
results <- read.csv("NCAATourneyCompactResults.csv") 
seeds <- read.csv("NCAATourneySeeds.csv")

# convert and remove string characters from seed values
seeds$Seed <- sapply(seeds$Seed,function(x) as.integer(gsub("[A-Z]","",as.character(x))))

seedResults <- merge(results, rename(seeds, WTeamID = TeamID, WSeed = Seed), by = c("WTeamID", "Season")) %>%
               merge( rename(seeds, LTeamID = TeamID, LSeed = Seed), by = c("LTeamID", "Season")) %>%
               na.omit()
# wins by team
byTeam <- results%>% group_by(WTeamID)
teamWins <- byTeam %>% summarise(
  totalWins = n())

# wins by seeds
seedWins <- seedResults%>% group_by(WSeed) %>% summarise(totalWins = n())
seedLoss <- seedResults%>% group_by(LSeed) %>% summarise(totalLosses = n())

bySeed <- merge(seedWins, seedLoss, by.x = "WSeed", by.y = "LSeed")
# EWins is yearly expected wins per team by their seed. --> totalWins / 4 * number of seasons   (4 due to 4 different seeds every year)
bySeed <- mutate(bySeed, WPct = totalWins/(totalWins +totalLosses), EWins = totalWins/(4*n_distinct(seedResults$Season))) %>%
          round(digits = 3)
