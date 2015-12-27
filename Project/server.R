#read in the data

teams <- 'data/teams.csv'
tourneyResultsThru2015 <- 'data/tourney_compact_results_thru_2015.csv'
tourneySeeds2015 <- 'data/tourney_seeds_2015.csv'
tourneySeeds <- 'data/tourney_seeds.csv'
teams <- read.csv(teams)
tourneyResultsThru2015 <- read.csv(tourneyResultsThru2015,header=T)
tourneySeeds2015 <- read.csv(tourneySeeds2015,header=T)
tourneySeeds <- read.csv(tourneySeeds,header=T)
tourneySeeds <- rbind(tourneySeeds,tourneySeeds2015)
remove(tourneySeeds2015)

library(dplyr);
tourneySeeds <- tourneySeeds[tourneySeeds$season>2002,]
tourneyResultsThru2015 <- tourneyResultsThru2015[tourneyResultsThru2015$season>2002,]
tourneyResultsThru2015 <- select(tourneyResultsThru2015,-wloc,-numot)
tourneySeeds <- tourneySeeds %>% mutate(season=as.factor(season),
                                        seed=as.factor(seed),
                                        team=as.factor(team))
teams$team <- as.character(teams$team_id)

# get the daynum of the last win in the tournament for each team by year from tourneyResultsThru2015
byTeam <- tourneyResultsThru2015 %>% group_by(season,team=wteam)
results <- byTeam %>% summarise(lastWin=max(daynum)); remove(byTeam)
results$season <- as.factor(results$season);results$team <- as.factor(results$team)
tourneySeeds$rank <- as.factor(substr(tourneySeeds$seed,2,3));tourneySeeds$region <- as.factor(substr(tourneySeeds$seed,1,1))

# leftjoin on tourneyVars, if na then that means they didn't have any wins and were knocked out in R1
tourneySeeds <- left_join(tourneySeeds,teams,by=c("team"))
tourneySeeds <- left_join(tourneySeeds,results,by=c("season","team")); remove(results)
tourneySeeds <- tourneySeeds %>% select(season,team,rank,team_name,lastWin)

tourneyWins <- tourneySeeds$lastWin
tourneyWins <- ifelse(is.na(tourneyWins),0,
                      ifelse(tourneyWins<136,0,
                             ifelse(tourneyWins<138,1,
                                    ifelse(tourneyWins<140,2,
                                           ifelse(tourneyWins<145,3,
                                                  ifelse(tourneyWins<147,4,
                                                         ifelse(tourneyWins<154,5,6)))))))

tourneySeeds$lastWin <- tourneyWins; remove(tourneyWins)
tourneySeeds$rank <- as.numeric(tourneySeeds$rank)

possibleMatches <- left_join(x=tourneySeeds,y=tourneySeeds,by=c("season"))
possibleMatches$team.x <- as.integer(possibleMatches$team.x)
possibleMatches$team.y <- as.integer(possibleMatches$team.y)
possibleMatches <- possibleMatches[possibleMatches$team.x<possibleMatches$team.y,]

possibleMatches$pred <- ifelse(possibleMatches$rank.x<=possibleMatches$rank.y,
                               possibleMatches$team.x,
                               possibleMatches$team.y)
head(possibleMatches)
possibleMatches$team.x<-as.character(possibleMatches$team.x)
possibleMatches$team.y<-as.character(possibleMatches$team.y)

results <- tourneyResultsThru2015 %>% mutate(season=as.factor(season),
                                             win=wteam,
                                             team.x=as.character(ifelse(wteam<lteam,wteam,lteam)),
                                             team.y=as.character(ifelse(wteam>lteam,wteam,lteam))) %>%
  select(season,team.x,team.y,win)
remove(tourneyResultsThru2015); remove(tourneySeeds);remove(teams)

results$team.x<-as.character(results$team.x)
results$team.y<-as.character(results$team.y)

results <- inner_join(possibleMatches,results,by=c("season","team.x","team.y"))
results <- results %>% mutate(perf=ifelse(pred==win,1,-1))
results$pred<-as.character(results$pred)
results$win<-as.character(results$win)

results$pred_team <- ifelse(grepl(results$pred,results$team.x),
                            as.character(results$team_name.x),
                            as.character(results$team_name.y))
results$winning_team <- ifelse(grepl(results$win,results$team.x),
                               as.character(results$team_name.x),
                               as.character(results$team_name.y))

mycols <- c("team_name.x","team_name.y","pred_team","winning_team")
myOutcome <- results[results$season==2015,mycols]
colnames(myOutcome)<-c("teamA","teamB","pred_team","winning_team")

shinyServer(function(input, output) {
  output$inputValue <- renderPrint({
    input$teamA
    input$teamB})
  output$pred_team <- renderPrint({
    ifelse(length(myOutcome$teamA==input$teamA & myOutcome$teamB==input$teamB)>0,
           myOutcome[myOutcome$teamA==input$teamA & myOutcome$teamB==input$teamB,
                     3],
           "Sorry, this is not a valid matchup.  Please try again.")
  })
  output$winning_team <- renderPrint({
    ifelse(length(myOutcome$teamA==input$teamA & myOutcome$teamB==input$teamB)>0,
           myOutcome[myOutcome$teamA==input$teamA & myOutcome$teamB==input$teamB,
                     4],
           "Sorry, this is not a valid matchup.  Please try again.")
  })
  }
)
