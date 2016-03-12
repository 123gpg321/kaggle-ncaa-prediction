# Loading Packages
library(data.table); library(dplyr); library(reshape)
# Reading in the data
TourneySeeds <- fread("../input/TourneySeeds.csv")
SampleSubmission <- fread("../input/SampleSubmission.csv")
Seasons <- fread("../input/Seasons.csv")
Teams <- fread("../input/Teams.csv")
TourneySlots <- fread("../input/TourneySlots.csv")
TourneyDetailedResults <- fread("../input/TourneyDetailedResults.csv")
TourneyCompactResults <- fread("../input/TourneyCompactResults.csv")
KenPom <- fread("../input/kenpom.csv")
colnames(KenPom)[2]="Team_Name"
# A Quick Look at the Data
head(TourneySeeds)
head(TourneySlots)
head(SampleSubmission)
head(Seasons)
head(Teams)
head(TourneyDetailedResults)
head(TourneyCompactResults)
# Extracting seeds for each team
TourneySeeds <- TourneySeeds %>%
    mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)

head(TourneySeeds)


games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))
head(games.to.predict)
# Joining Games with Team Seeds

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
team1_data = data.frame(team_data)
colnames(team1_data) <- paste("team1", colnames(team1_data), sep = "_")
team2_data = data.frame(team_data)
colnames(team2_data) <- paste("team2", colnames(team_data), sep = "_")


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
#temp <- left_join(games, team_data_by_season, by=c("Season"="Season", "Wteam"="Wteam"))
#all.data = left_join(games, team_data_by_season, by=c("Season"="Season", "Lteam"="Wteam"))
temp <- left_join(games, team1_data, by=c("team1"="team1_team"))
all.data = left_join(temp, team2_data, by=c("team2"="team2_team"))
all.data = all.data %>% na.omit()

temp <- left_join(games.to.predict, team1_data, by=c("team1"="team1_team"))
games.to.predict = left_join(temp, team2_data, by=c("team2"="team2_team"))

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

predict(m.seed.diff2,games.to.predict)[which(full.set$team1win==1)]
full.set$prediction = predict(m.seed.diff2,type="response")
#ggplot(full.set, aes(prediction, fill = team1win==1)) + geom_density(alpha = 0.2)
ggplot(full.set, aes(prediction, fill = team1win==1)) + geom_density(alpha = 0.2) + guides(fill=guide_legend(title='Team 1 wins'))


all.data = data.frame(TourneyDetailedResults) %>%
                  left_join(data.frame(TourneySeeds),by=c("Season"="Season", "Wteam"="Team")) %>%
                  dplyr::rename(Wseed = SeedNum) %>%
                  left_join(data.frame(TourneySeeds),by=c("Season"="Season", "Lteam"="Team")) %>%
                  dplyr::rename(Lseed = SeedNum) %>%
                  mutate(Wseed = as.numeric(Wseed),
                  Lseed = as.numeric(Lseed),
                  Score_diff=Wscore - Lscore,
                  Rankdiff = Wseed - Lseed) %>%
                  select(-c(Wloc,Lscore,Wscore))


KenPom$link = KenPom$Team_Name
KenPom$Ken_index = 1:dim(KenPom)[1]
Teams$link = Teams$Team_Name
Teams$Team_index = 1:dim(Teams)[1]
team.full = merge(Teams, KenPom, by="link")
> i = 5; KenPom$Team_Name[i]; Teams$Team_Name[agrep(KenPom$Team_Name[i],Teams$Team_Name)]
> i = 5; KenPom$Team_Name[i]; Teams$Team_Name[agrep(KenPom$Team_Name[i],Teams$Team_Name)]
