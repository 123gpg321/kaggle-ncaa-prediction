# Loading Packages
library(data.table); library(dplyr); library(reshape); library(stringdist)
# Reading in the data
TourneySeeds <- fread("../input/TourneySeeds.csv")
SampleSubmission <- fread("../input/SampleSubmission.csv")
Seasons <- fread("../input/Seasons.csv")
Teams <- fread("../input/Teams.csv")
TourneySlots <- fread("../input/TourneySlots.csv")
# TourneyDetailedResults <- fread("../input/TourneyDetailedResults.csv")
# TourneyCompactResults <- fread("../input/TourneyCompactResults.csv")
TourneyDetailedResults <- fread("../input/RegularSeasonDetailedResults.csv")
TourneyCompactResults <- fread("../input/RegularSeasonCompactResults.csv")
KenPom <- fread("../input/kenpom.csv")
FiveThirtyEight <- fread("../input/fivethirtyeight.csv")
TeamSpelling <- fread("../input/TeamSpellings.csv")

# Extracting seeds for each team
TourneySeeds <- TourneySeeds %>%
    mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)


# Fix this with spelling!
fuzzyMatch = cbind(Teams$Team_Name,KenPom$Team[amatch(Teams$Team_Name,KenPom$Team,maxDist=70)])
colnames(fuzzyMatch) = c("Teams_name","KenPom_name")

# Prepare prediction target
games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))

# Add available data to each target game
temp <- left_join(games.to.predict, TourneySeeds, by=c("season"="Season", "team1"="Team"))
games.to.predict <- left_join(temp, TourneySeeds, by=c("season"="Season", "team2"="Team"))
colnames(games.to.predict)[c(1,5:6)] <- c("Id", "team1seed", "team2seed")
games.to.predict <- games.to.predict %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))
games.to.predict = games.to.predict %>% dplyr::rename(Season=season,team1_seed=team1seed,team2_seed=team2seed)



# Joining (compact) Results with Team Seeds
temp <- left_join(as.data.frame(TourneyCompactResults), TourneySeeds, by=c("Season", "Wteam"="Team"))
compact.results <- left_join(temp, TourneySeeds, by=c("Season", "Lteam"="Team"))
compact.results = compact.results %>%
                  mutate(SeedNum.x=as.numeric(SeedNum.x),
                         SeedNum.y=as.numeric(SeedNum.y))
head(compact.results)

# Every win for one team is a loss for the other team...

set1 <- compact.results %>% select(SeedNum.x, SeedNum.y) %>% mutate(result=1)
set2 <- compact.results %>% select(SeedNum.y, SeedNum.x) %>% mutate(result=0)
colnames(set1) <- c("team1seed", "team2seed", "team1win")
colnames(set2) <- c("team1seed", "team2seed", "team1win")
full.set <- rbind(set1, set2)
full.set <- full.set %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))

winner_Tdata = select(TourneyDetailedResults, Season, Wteam, Wscore, Wfgm:Wpf)
loser_Tdata = select(TourneyDetailedResults, Season, Lteam, Lscore, Lfgm:Lpf)
tdata = rbind(winner_Tdata, setNames(loser_Tdata, names(winner_Tdata)))
colnames(tdata) = c("season",
                        "team",
                        "score",
                        "fgm",
                        "fga",
                        "fgm3",
                        "fga3",
                        "ftm",
                        "fta",
                        "or",
                        "dr",
                        "ast",
                        "to",
                        "stl",
                        "blk",
                        "Wpf")

team_data_by_season = tdata %>% group_by(season, team) %>% summarise_each(funs(mean))
team_data = tdata %>% select(-season) %>%group_by(team) %>% summarise_each(funs(mean))

temp = left_join(data.frame(team_data_by_season),data.frame(Teams),by=c("team"="Team_Id"))
remp = left_join(temp,fuzzyMatch,by=c("Team_Name"="Teams_name"),copy=TRUE)
team_data_by_season = left_join(remp,data.frame(KenPom),by=c("KenPom_name"="Team", "season"="Year"))



team1_data = data.frame(team_data)
colnames(team1_data) <- paste("team1", colnames(team1_data), sep = "_")
team2_data = data.frame(team_data)
colnames(team2_data) <- paste("team2", colnames(team2_data), sep = "_")

team1_data_by_season = data.frame(team_data_by_season)
colnames(team1_data_by_season) <- paste("team1", colnames(team1_data_by_season), sep = "_")
team2_data_by_season = data.frame(team_data_by_season)
colnames(team2_data_by_season) <- paste("team2", colnames(team2_data_by_season), sep = "_")



games.as.wins = compact.results %>%
                mutate(Score_diff = Wscore - Lscore) %>%
                select(Season,
                       team1=Wteam,
                       team2=Lteam,
                       team1_seed = SeedNum.x,
                       team2_seed = SeedNum.y,
                       Score_diff)
games.as.losses = compact.results %>%
                  mutate(Score_diff = Lscore - Wscore) %>%
                  select(Season,
                         team1=Lteam,
                         team2=Wteam,
                         team1_seed = SeedNum.y,
                         team2_seed = SeedNum.x,
                         Score_diff)
games = rbind(games.as.wins, games.as.losses)
temp <- left_join(games, team1_data_by_season, by=c("Season"="team1_season", "team1"="team1_team"))
all.data = left_join(temp, team2_data_by_season, by=c("Season"="team2_season", "team2"="team2_team"))
#temp <- left_join(games, team1_data, by=c("team1"="team1_team"))
#all.data = left_join(temp, team2_data, by=c("team2"="team2_team"))
all.data = all.data %>% na.omit()


temp <- left_join(games.to.predict, team1_data_by_season, by=c("Season"="team1_season", "team1"="team1_team"))
games.to.predict = left_join(temp, team2_data_by_season, by=c("Season"="team2_season", "team2"="team2_team"))
#temp <- left_join(games.to.predict, team1_data, by=c("team1"="team1_team"))
#games.to.predict = left_join(temp, team2_data, by=c("team2"="team2_team"))


all.data = all.data %>% select(-c(team1_KenPom_name,team2_KenPom_name, team1_Team_Name, team2_Team_Name))

test = glm(team1win~ ., data=all.data, family = "binomial")

m.score_diff <- lm(Score_diff~ ., data=all.data)
all.data$Predicted_Score_diff = predict(m.score_diff)
all.data = all.data %>% mutate(team1win = ifelse(Score_diff > 0,1,0))
win_chance = ecdf(all.data$Predicted_Score_diff)
# Building a Simple Linear Model Based on the Difference in Team Seeds

#m.seed.diff <- lm(team1win~ I(team2seed-team1seed), data=full.set)
m.seed.diff = glm(team1win~ Predicted_Score_diff + I(team2_seed-team1_seed), data=all.data, family = "binomial")

#summary(m.seed.diff)
# Making Predictions using the Team Seeds Model

games.to.predict$Predicted_Score_diff = predict(m.score_diff,games.to.predict)
games.to.predict$Pred = predict(m.seed.diff, games.to.predict, type='response')
#games.to.predict$Pred <- win_chance(predict(glm.fit, games.to.predict))

#games.to.predict$Pred <- win_chance(predict(m.score_diff, games.to.predict))
write.csv(games.to.predict %>% select(Id, Pred), 'seed_submission.csv', row.names=FALSE)



# this is how I could get the KenPom Data in.

#i = 5; KenPom$Team_Name[i]; Teams$Team_Name[agrep(KenPom$Team_Name[i],Teams$Team_Name)]
