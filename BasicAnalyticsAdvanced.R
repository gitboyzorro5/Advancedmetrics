#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

epl_match_report <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier="1st")

bundes_match_report <- fb_match_results(country = "GER", gender = "M", season_end_year = 2024, tier="1st")
########################################################################################################################################
sqldf("SELECT Referee,COUNT(*),AVG(TY) FROM LIGUEONE_spread GROUP BY Referee ORDER BY COUNT(*) desc")
#########################################################################################################################################
View(final_doublefixture_e0)
########################################################################################################################################
library('xlsx')
E0_spreaducl <- readxl::read_excel('../Rsoccer/E0_spread.xlsx')
E0_spreaducl <- E0_spreaducl[,c(-1)]

E0_advstats <- readxl::read_excel('EPL_SPREAD.xlsx')
E0_advstats <- E0_advstats[,-1]
EPL_FINALSPREAD <- dplyr::left_join(E0_spreaducl,EPL_spread)
EPL_FINALSPREAD <- EPL_FINALSPREAD %>% dplyr::relocate(51,.after = 37)
write.xlsx(EPL_FINALSPREAD,'EPL_FINALSPREAD.xlsx')

########################################################################################################################################


###MLS TEST
mls_match_results <- fb_match_results(country = "USA", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(mls_match_results,"mls_match_results.xlsx")
mls_urls <- fb_match_urls(country = "USA", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(mls_urls,"mls_urls.xlsx")

mls_match_sumary <- fb_match_summary(match_url = mls_urls)
write.xlsx(mls_match_sumary,"mls_match_summary.xlsx")

mls_match_shots <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "summary" , team_or_player = "team")

write.xlsx(mls_match_shots,"mls_shots.xlsx")

mls_match_corners <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "passing_types" , team_or_player = "team")

write.xlsx(mls_match_corners,"mls_corners.xlsx")

mls_match_fouls <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "misc" , team_or_player = "team")

write.xlsx(mls_match_fouls,"mls_fouls.xlsx")
###################################################################################################
allteams2010presentcsv <- read.csv('../FDAS/allteams2010-present.csv')
View(allteams2010presentcsv)
allteams2010presentcsv <- allteams2010presentcsv[,c(-14,-15)]
allteams2010presentcsv$HTCS <- paste(allteams2010presentcsv$HTHG,allteams2010presentcsv$HTAG,sep = "-")
allteams2010presentcsv$CS <- paste(allteams2010presentcsv$FTHG,allteams2010presentcsv$FTAG,sep = "-")
write.csv(allteams2010presentcsv,'allteams2010-present.csv',row.names = FALSE)
#####################################################################################################

#SC2 and SC3 fixtures
HomeTeam_sc3 <- rep(sc3_teams, each = length(sc3_teams))
AwayTeam_sc3 <- rep(sc3_teams, length(sc3_teams))
SC3_fixtures <- cbind(HomeTeam_sc3,AwayTeam_sc3)
SC3_fixtures <- as.data.frame(SC3_fixtures)
SC3_fixtures <- SC3_fixtures[!SC3_fixtures$HomeTeam_sc3 == SC3_fixtures$AwayTeam_sc3,]
rownames(SC3_fixtures) <- NULL
SC3_fixtures$Div <- "SC3"
SC3_fixtures <- SC3_fixtures[,c(3,1,2)]
colnames(SC3_fixtures)[3] <- "AwayTeam"
write.csv(SC3_fixtures,'SCOTTLEAGUETWOFIXURES.csv')


#######################################################################################################
library('xlsx')
library('sqldf')
require('RH2')

OTHERLEAGUES_analytics <- readxl::read_excel('OTHERLEAGUES20232024.xlsx')
OTHERLEAGUES_analytics <- OTHERLEAGUES_analytics[,-1]
OTHERLEAGUES_analytics <- as.data.frame(OTHERLEAGUES_analytics)

Refereefoulsother <- sqldf("SELECT Referee,Div,COUNT(*),AVG(TF),SUM(Bookings)/SUM(TF) FROM OTHERLEAGUES_analytics GROUP BY Referee ORDER BY COUNT(*) DESC")


write.xlsx(Refereefoulsother,'Refereefoulsother.xlsx', sheetName = "fouls", append = TRUE)
#################################################################################################################################
#################################################################################################################################
#picks last seasons match summaries
ligueone_match_urls <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(ligueone_match_urls,'ligueone_match_urls20232024.xlsx')
ligueone_summary <- fb_match_summary(match_url = ligueone_match_urls)

sort(unique(ligueone_summary$Home_Team))
f1_teams

ligueone_summary$Home_Team <- mgsub(ligueone_summary$Home_Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))
ligueone_summary$Away_Team <- mgsub(ligueone_summary$Away_Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))
ligueone_summary$Team <- mgsub(ligueone_summary$Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))

ligueone_summary$matchid <- paste(ligueone_summary$Home_Team,ligueone_summary$Away_Team,sep = "-")

LIGUEONE_spread <- subset(allteams20232024,Div =="F1")
LIGUEONE_spread$matchid <- paste(LIGUEONE_spread$HomeTeam,LIGUEONE_spread$AwayTeam,sep = "-")

options(java.parameters = "-Xmx4g")
write.xlsx(ligueone_summary,'ligueone_summary20232024.xlsx')

library('sqldf')
require('RH2')

Home_first_GoalTime <- c()
Home_first_GoalTime <- sqldf("SELECT ligueone_summary.matchid,MIN(ligueone_summary.Event_Time) AS Home_first_GoalTime FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Goal' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Home_first_GoalTime)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

Away_first_GoalTime <- c()
Away_first_GoalTime <- sqldf("SELECT ligueone_summary.matchid,MIN(ligueone_summary.Event_Time) AS Away_first_GoalTime FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Goal' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Away_first_GoalTime)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

LIGUEONE_spread$match_First_GoalTime <- ifelse(LIGUEONE_spread$Home_first_GoalTime == '0' | LIGUEONE_spread$Away_first_GoalTime == '0',pmax(LIGUEONE_spread$Home_first_GoalTime,LIGUEONE_spread$Away_first_GoalTime),pmin(LIGUEONE_spread$Home_first_GoalTime,LIGUEONE_spread$Away_first_GoalTime))
View(LIGUEONE_spread)
ligueone_first_matchgoal <- LIGUEONE_spread[,c(37,38,39,40)]
write.xlsx(ligueone_first_matchgoal,'ligueone_first_matchgoal.xlsx')

################################################################################################################
################################################################################################################
bigfive_first_matchgoal <- rbind(epl_first_matchgoal,bundes_first_matchgoal,seriea_first_matchgoal,laliga_first_matchgoal,ligueone_first_matchgoal)
BIGFIVE_analytics <- readxl::read_excel('BIGFIVE20232024.xlsx')
BIGFIVE_analytics <- BIGFIVE_analytics[,-1]
##################################################################################################################
sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_GoalTime BETWEEN '1' and '45' GROUP BY DIV ORDER BY COUNT(*) DESC")
