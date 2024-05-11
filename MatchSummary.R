#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

#first time reading
#epl_match_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="1st"
#epl_summary <- fb_match_summary(match_url = epl_match_urls)


#write.xlsx(epl_summary,"epl_summary.xlsx")
#write.xlsx(epl_match_urls,'epl_match_urls.xlsx')
#second time reading algorithm
#first read current rows in epl_match_urls.xlsx
#read total rows in fbref epl_match_urls from fb_match_urls function
#find the difference between the two and tail the number to be used to download for new epl_summary
current_epl_match_urls_nrows <- nrow(readxl::read_excel('epl_match_urls.xlsx'))
epl_match_urls_nrows <- length(fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="1st"))
new_epl_match_urls <- tail(fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="1st"),epl_match_urls_nrows - current_epl_match_urls_nrows)

new_epl_summary <- fb_match_summary(match_url = new_epl_match_urls)

new_epl_summary$Home_Team <- mgsub(new_epl_summary$Home_Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))
new_epl_summary$Away_Team <- mgsub(new_epl_summary$Away_Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))
new_epl_summary$Team <- mgsub(new_epl_summary$Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))

new_epl_summary$matchid <- paste(new_epl_summary$Home_Team,new_epl_summary$Away_Team,sep = "-")

epl_summary <- rbind(epl_summary,new_epl_summary)
unlink('epl_summary.xlsx')
write.xlsx(epl_summary,"epl_summary.xlsx")

current_epl_match_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2024, tier="1st")
unlink('epl_match_urls.xlsx')
write.xlsx(current_epl_match_urls,'epl_match_urls.xlsx')
View(current_epl_match_urls)

#reading and writing
epl_summary <- readxl::read_excel('epl_summary.xlsx')
epl_summary <- epl_summary[,c(-1)]

epl_summary$Home_Team <- mgsub(epl_summary$Home_Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))
epl_summary$Away_Team <- mgsub(epl_summary$Away_Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))
epl_summary$Team <- mgsub(epl_summary$Team,c("Brighton & Hove Albion","Luton Town","Manchester City","Manchester United","Newcastle United","Nottingham Forest","Sheffield Utd","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"),c("Brighton","Luton","Man City","Man United","Newcastle","Nottm Forest","Sheffield United","Tottenham","West Ham","Wolves"))

epl_summary$matchid <- paste(epl_summary$Home_Team,epl_summary$Away_Team,sep = "-")


EPL_spread <- subset(allteams20232024,Div =="E0")
EPL_spread$matchid <- paste(EPL_spread$HomeTeam,EPL_spread$AwayTeam,sep = "-")

E0_referees <- read.csv('../FDAS/E0.csv')
E0_referees <- E0_referees[,c(4,5,12)]
E0_referees$matchid <- paste(E0_referees$HomeTeam,E0_referees$AwayTeam,sep = "-")

EPL_spread <- dplyr::left_join(EPL_spread,E0_referees)


library('sqldf')
require('RH2')


Home_xG <- c()
Home_xG <- sqldf("SELECT epl_summary.matchid,epl_summary.Home_xG FROM epl_summary INNER JOIN EPL_spread ON epl_summary.matchid = EPL_spread.matchid GROUP BY epl_summary.matchid")
EPL_spread <- dplyr::left_join(EPL_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT epl_summary.matchid,epl_summary.Away_xG FROM epl_summary INNER JOIN EPL_spread ON epl_summary.matchid = EPL_spread.matchid GROUP BY epl_summary.matchid")
EPL_spread <- dplyr::left_join(EPL_spread,Away_xG)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS FH_HYC FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Event_Half = '1' AND epl_summary.Home_Away = 'Home' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,FH_HYC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS FH_AYC FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Event_Half = '1' AND epl_summary.Home_Away = 'Away' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,FH_AYC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS FH_HRC FROM epl_summary WHERE epl_summary.Event_Type = 'Red Card' AND epl_summary.Event_Half = '1' AND epl_summary.Home_Away = 'Home' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,FH_HRC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS FH_ARC FROM epl_summary WHERE epl_summary.Event_Type = 'Red Card' AND epl_summary.Event_Half = '1' AND epl_summary.Home_Away = 'Away' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,FH_ARC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS SH_HYC FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Event_Half = '2' AND epl_summary.Home_Away = 'Home' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,SH_HYC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS SH_AYC FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Event_Half = '2' AND epl_summary.Home_Away = 'Away' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,SH_AYC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS SH_HRC FROM epl_summary WHERE epl_summary.Event_Type = 'Red Card' AND epl_summary.Event_Half = '2' AND epl_summary.Home_Away = 'Home' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,SH_HRC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS SH_ARC FROM epl_summary WHERE epl_summary.Event_Type = 'Red Card' AND epl_summary.Event_Half = '2' AND epl_summary.Home_Away = 'Away' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,SH_ARC)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

#firsthalf
EPL_spread$FH_HomeBookings <- EPL_spread$FH_HYC *10 + EPL_spread$FH_HRC *25

EPL_spread$FH_AwayBookings <- EPL_spread$FH_AYC *10 + EPL_spread$FH_ARC *25

EPL_spread$FH_TotalBookings <- EPL_spread$FH_HomeBookings + EPL_spread$FH_AwayBookings

#second half
EPL_spread$SH_HomeBookings <- EPL_spread$SH_HYC *10 + EPL_spread$SH_HRC *25

EPL_spread$SH_AwayBookings <- EPL_spread$SH_AYC *10 + EPL_spread$SH_ARC *25

EPL_spread$SH_TotalBookings <- EPL_spread$SH_HomeBookings + EPL_spread$SH_AwayBookings


EPL_spread$MultiBookings <- EPL_spread$FH_TotalBookings * EPL_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT epl_summary.matchid,SUM(Event_time) AS Home_YCmins FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Home_Away = 'Home' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,Home_YCmins)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT epl_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM epl_summary WHERE epl_summary.Event_Type = 'Red Card' AND epl_summary.Home_Away = 'Home' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,Home_RCmins)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT epl_summary.matchid,SUM(Event_time) AS Away_YCmins FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Home_Away = 'Away' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,Away_YCmins)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT epl_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM epl_summary WHERE epl_summary.Event_Type = 'Red Card' AND epl_summary.Home_Away = 'Away' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,Away_RCmins)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

EPL_spread$Home_TotalCardmins <- EPL_spread$Home_YCmins + EPL_spread$Home_RCmins
EPL_spread$Away_TotalCardmins <- EPL_spread$Away_YCmins + EPL_spread$Away_RCmins
EPL_spread$match_TotalCardmins <- EPL_spread$Home_TotalCardmins + EPL_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT epl_summary.matchid,MIN(epl_summary.Event_Time) AS Home_first_YCTime FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Home_Away = 'Home' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,Home_first_YCTime)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT epl_summary.matchid,MIN(epl_summary.Event_Time) AS Away_first_YCTime FROM epl_summary WHERE epl_summary.Event_Type = 'Yellow Card' AND epl_summary.Home_Away = 'Away' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,Away_first_YCTime)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)

EPL_spread$match_First_YCTime <- pmin(EPL_spread$Home_first_YCTime,EPL_spread$Away_first_YCTime)


write.xlsx(EPL_spread,'EPL_SPREAD.xlsx')
colnames(EPL_spread)# 39 to 65 columns




