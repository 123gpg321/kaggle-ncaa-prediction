rm(list=ls())
# Loading Packages
library(data.table);
library(dplyr);
library(reshape);
library(stringdist)

rename = dplyr::rename
################################################################################
DataFiles = fread('meta_data.txt')
################################################################################
# Download data if missing
apply(DataFiles,1,function(x){
  file_location=paste('data/',x[1],'.csv',sep='')
  if(!file.exists(file_location)){
    download.file(x[2],file_location)
  }
  })

################################################################################
# Reading in the raw data
TourneySeeds <- fread("data/TourneySeeds.csv")
SampleSubmission <- fread("data/SampleSubmission.csv")
Seasons <- fread("data/Seasons.csv")
Teams <- fread("data/Teams.csv")
TourneySlots <- fread("data/TourneySlots.csv")
TourneyDetailedResults <- fread("data/TourneyDetailedResults.csv")
#TourneyCompactResults <- fread("data/TourneyCompactResults.csv")
RegularSeasonDetailedResults <- fread("data/RegularSeasonDetailedResults.csv")
#RegularSeasonCompactResults <- fread("data/RegularSeasonCompactResults.csv")
KenPom <- fread("data/KenPom.csv")
TeamSpelling <- fread("data/TeamSpellings.csv")
################################################################################
# Preproces data
# Rename for merge
TeamSpelling <- rbind(TeamSpelling,rename(Teams,name_spelling=Team_Name,team_id=Team_Id))
# Extracting seeds for each team
TourneySeeds <- TourneySeeds %>%
    mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)

# fix external data
colnames(TeamSpelling)[1] = "Team"
KenPom$Team = tolower(KenPom$Team)
Teams$Team_Name = tolower(Teams$Team_Name)

# mamp spelling
KenPom = left_join(KenPom, TeamSpelling, by=c('Team'='Team'))

################################################################################
# Construct summary data

winner_Tdata = select(RegularSeasonDetailedResults, Season, Wteam, Wscore, Wfgm:Wpf)
loser_Tdata = select(RegularSeasonDetailedResults, Season, Lteam, Lscore, Lfgm:Lpf)
Team_Game_History = rbind(winner_Tdata, setNames(loser_Tdata, names(winner_Tdata)))
colnames(Team_Game_History) = c("season",
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

team_data_by_season = Team_Game_History %>% group_by(season, team) %>% summarise_each(funs(mean))
#team_data = Team_Game_History %>%group_by(team) %>% summarise_each(funs(mean))

# Prepare data for joining

#team1_data = data.frame(team_data)
#colnames(team1_data) <- paste("team1", colnames(team1_data), sep = "_")
#team2_data = data.frame(team_data)
#colnames(team2_data) <- paste("team2", colnames(team2_data), sep = "_")

team1_data_by_season = data.frame(team_data_by_season)
colnames(team1_data_by_season) <- paste("team1", colnames(team1_data_by_season), sep = "_")
team2_data_by_season = data.frame(team_data_by_season)
colnames(team2_data_by_season) <- paste("team2", colnames(team2_data_by_season), sep = "_")

team1_kenpom_data_by_season = data.frame(KenPom)
colnames(team1_kenpom_data_by_season) <- paste("team1", colnames(team1_kenpom_data_by_season), sep = "_")
team2_kenpom_data_by_season = data.frame(KenPom)
colnames(team2_kenpom_data_by_season) <- paste("team2", colnames(team2_kenpom_data_by_season), sep = "_")


################################################################################
# Get going with actual machine learning:
# 1. set train,test,predict
# 2. add available data
# 3. build models
# 4. Make Predictions
################################################################################
# Prepare prediction target
games.to.train <- RegularSeasonDetailedResults %>%
                  mutate(season=Season, team1=Wteam, team2=Lteam, Score_diff=Wscore-Lscore, team1win=1) %>%
                  select(season, team1, team2, Score_diff, team1win)
games.to.test <- TourneyDetailedResults %>%
                  mutate(season=Season, team1=Wteam, team2=Lteam, Score_diff=Wscore-Lscore, team1win=1) %>%
                  select(season, team1, team2, Score_diff, team1win)
games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))

flippedGames = function(game){
  flipped <- game %>%
  rename(team1=team2,team2=team1) %>%
  mutate(Score_diff=-Score_diff, team1win=0)
}

games.to.train = rbind(games.to.train,flippedGames(games.to.train))

# Add available data to each target game
addDataToGames = function(games) {

  games <- data.frame(games) %>%
          # add March madness seed to teams in games
          left_join(TourneySeeds, by=c("season"="Season", "team1"="Team")) %>%
          rename(team1seed = SeedNum) %>%
          left_join(TourneySeeds, by=c("season"="Season", "team2"="Team")) %>%
          rename(team2seed = SeedNum) %>%
          mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed)) %>%
          # add seasonal data
          left_join(team1_data_by_season,by=c("season" = "team1_season", "team1"="team1_team")) %>%
          left_join(team2_data_by_season,by=c("season" = "team2_season", "team2"="team2_team")) %>%
          # add external data
          left_join(team1_kenpom_data_by_season,by=c("season" = "team1_Year", "team1"="team1_team_id")) %>%
          left_join(team2_kenpom_data_by_season,by=c("season" = "team2_Year", "team2"="team2_team_id"))
  return(games)

}

games.to.train = addDataToGames(games.to.train)
games.to.test = addDataToGames(games.to.test)
games.to.predict = addDataToGames(games.to.predict)

games.to.train = games.to.train %>% na.omit()
games.to.test = games.to.test %>% na.omit()

getMissing = function(){
  a=Teams$Team_Name[which(Teams$Team_Id %in% unique(games.to.test$team2[which(is.na(games.to.test$team2_Team))]))]
  b=Teams$Team_Name[which(Teams$Team_Id %in% unique(games.to.test$team1[which(is.na(games.to.test$team1_Team))]))]
  c=Teams$Team_Name[which(Teams$Team_Id %in% unique(games.to.train$team2[which(is.na(games.to.train$team2_Team))]))]
  d=Teams$Team_Name[which(Teams$Team_Id %in% unique(games.to.train$team1[which(is.na(games.to.train$team1_Team))]))]
  e=Teams$Team_Name[which(Teams$Team_Id %in% unique(games.to.predict$team2[which(is.na(games.to.predict$team2_Team))]))]
  f=Teams$Team_Name[which(Teams$Team_Id %in% unique(games.to.predict$team1[which(is.na(games.to.predict$team1_Team))]))]
  missing = unique(c(a,b,c,d,e,f))
  print(missing)
  return(missing)
}

m.score_diff <- lm(Score_diff~ .,
                   data=select(games.to.train,
                               -c(team1win,
                                  team1_Team,
                                  team2_Team,
                                  team1,
                                  team2)))

games.to.train$Predicted_Score_diff = predict(m.score_diff)
games.to.test$Predicted_Score_diff = predict(m.score_diff,games.to.test)
games.to.predict$Predicted_Score_diff = predict(m.score_diff,games.to.predict)

 win_chance = ecdf(games.to.train$Score_diff)

games.to.train$Pred = win_chance(games.to.train$Predicted_Score_diff)
games.to.test$Pred = win_chance(games.to.test$Predicted_Score_diff)
games.to.predict$Pred = win_chance(games.to.predict$Predicted_Score_diff)

getLogLoss = function(games){
  y = games$team1win
  pred = games$Pred
  logLoss = -mean(y*log(pred) + (1-y)*log(1-pred))
  return(logLoss)
}

getLogLoss(games.to.train)

getLogLoss(games.to.test)

write.csv(games.to.predict %>% select(Id=SampleSubmission.Id, Pred), 'seed_submission.csv', row.names=FALSE)
