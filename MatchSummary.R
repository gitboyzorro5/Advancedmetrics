#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')


epl_match_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="1st")

epl_summary <- fb_match_summary(match_url = epl_match_urls)

library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")
write.xlsx(epl_summary,"epl_sumamry.xlsx")
write.xlsx(epl_match_urls,'epl_match_urls.xlsx')

#reading and writing
epl_summary <- readxl::read_excel('epl_sumamry.xlsx')
epl_summary <- epl_summary[,c(-1)]


sort(unique(epl_summary$Home_Team))

epl_summary$Home_Team <- mgsub(epl_summary$Home_Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))
epl_summary$Away_Team <- mgsub(epl_summary$Away_Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))
epl_summary$Team <- mgsub(epl_summary$Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))

epl_summary$matchid <- paste(epl_summary$Home_Team,epl_summary$Away_Team,sep = "-")


EPL_spread <- subset(allteams20232024,Div =="E0")
EPL_spread$matchid <- paste(EPL_spread$HomeTeam,EPL_spread$AwayTeam,sep = "-")

library('sqldf')
require('RH2')

Home_xG <- c()
Home_xG <- sqldf("SELECT epl_summary.matchid,epl_summary.Home_xG FROM epl_summary INNER JOIN EPL_spread ON epl_summary.matchid = EPL_spread.matchid GROUP BY epl_summary.matchid")
EPL_spread <- dplyr::left_join(EPL_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT epl_summary.matchid,epl_summary.Away_xG FROM epl_summary INNER JOIN EPL_spread ON epl_summary.matchid = EPL_spread.matchid GROUP BY epl_summary.matchid")
EPL_spread <- dplyr::left_join(EPL_spread,Away_xG)


Total_Goal_mins <- c()
Total_Goal_mins <- sqldf("SELECT epl_summary.matchid,SUM(epl_summary.Event_Time) FROM epl_summary INNER JOIN EPL_spread ON epl_summary.matchid = EPL_spread.matchid WHERE epl_summary.Event_Type = 'Goal' GROUP BY epl_summary.matchid")
EPL_spread <- dplyr::left_join(EPL_spread,Total_Goal_mins)

colnames(EPL_spread)





View(EPL_spread)




