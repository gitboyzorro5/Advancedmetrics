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

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT epl_summary.matchid,COUNT(*) AS Penalty FROM epl_summary WHERE epl_summary.Event_Type = 'Penalty' GROUP BY epl_summary.matchid ")
EPL_spread <- dplyr::left_join(EPL_spread,Penalty)
EPL_spread <- EPL_spread %>% replace(is.na(.),0)
#calculate match performance
EPL_spread$MatchPerfomance <- EPL_spread$TG *15 + EPL_spread$TY *5 + EPL_spread$TR *15 + EPL_spread$TC *3 + EPL_spread$Penalty *10


unlink('EPL_SPREAD.xlsx')
write.xlsx(EPL_spread,'EPL_SPREAD.xlsx')
View(EPL_spread)
###################################################################################################

####################################################################################################
#Bundesliga

bundes_match_urls <- fb_match_urls(country = "GER", gender = "M", season_end_year = 2024, tier="1st")
bundes_summary <- fb_match_summary(match_url = bundes_match_urls)

write.xlsx(bundes_summary,"bundes_summary.xlsx")
write.xlsx(bundes_match_urls,'bundes_match_urls.xlsx')
#second time reading algorithm
#first read current rows in bundes_match_urls.xlsx
#read total rows in fbref bundes_match_urls from fb_match_urls function
#find the difference between the two and tail the number to be used to download for new bundes_summary
current_bundes_match_urls_nrows <- nrow(readxl::read_excel('bundes_match_urls.xlsx'))
bundes_match_urls_nrows <- length(fb_match_urls(country = "GER", gender = "M", season_end_year = 2024, tier="1st"))
new_bundes_match_urls <- tail(fb_match_urls(country = "GER", gender = "M", season_end_year = 2024, tier="1st"),bundes_match_urls_nrows - current_bundes_match_urls_nrows)

new_bundes_summary <- fb_match_summary(match_url = new_bundes_match_urls)

new_bundes_summary$Home_Team <- mgsub(new_bundes_summary$Home_Team,c("Bayer Leverkusen","Darmstadt 98","Eintracht Frankfurt","Mainz 05","Mönchengladbach","Köln"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln"))
new_bundes_summary$Away_Team <- mgsub(new_bundes_summary$Away_Team,c("Bayer Leverkusen","Darmstadt 98","Eintracht Frankfurt","Mainz 05","Mönchengladbach","Köln"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln"))
new_bundes_summary$Team <- mgsub(new_bundes_summary$Team,c("Bayer Leverkusen","Darmstadt 98","Eintracht Frankfurt","Mainz 05","Mönchengladbach","Köln"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln"))

new_bundes_summary$matchid <- paste(new_bundes_summary$Home_Team,new_bundes_summary$Away_Team,sep = "-")

bundes_summary <- rbind(bundes_summary,new_bundes_summary)
unlink('bundes_summary.xlsx')
write.xlsx(bundes_summary,"bundes_summary.xlsx")

current_bundes_match_urls <- fb_match_urls(country = "GER", gender = "M", season_end_year = 2024, tier="1st")
unlink('bundes_match_urls.xlsx')
write.xlsx(current_bundes_match_urls,'bundes_match_urls.xlsx')
View(current_bundes_match_urls)

#reading and writing
bundes_summary <- readxl::read_excel('bundes_summary.xlsx')
bundes_summary <- bundes_summary[,c(-1)]

sort(unique(bundes_summary$Home_Team))
bundes_summary$Home_Team <- mgsub(bundes_summary$Home_Team,c("Bayer Leverkusen","Darmstadt 98","Eintracht Frankfurt","Mainz 05","Mönchengladbach","Köln"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln"))
bundes_summary$Away_Team <- mgsub(bundes_summary$Away_Team,c("Bayer Leverkusen","Darmstadt 98","Eintracht Frankfurt","Mainz 05","Mönchengladbach","Köln"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln"))
bundes_summary$Team <- mgsub(bundes_summary$Team,c("Bayer Leverkusen","Darmstadt 98","Eintracht Frankfurt","Mainz 05","Mönchengladbach","Köln"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln"))

bundes_summary$matchid <- paste(bundes_summary$Home_Team,bundes_summary$Away_Team,sep = "-")


BUNDES_spread <- subset(allteams20232024,Div =="D1")
BUNDES_spread$matchid <- paste(BUNDES_spread$HomeTeam,BUNDES_spread$AwayTeam,sep = "-")

D1_referees <- fb_match_results(country = "GER", gender = "M", season_end_year = 2024, tier="1st")
D1_referees <- D1_referees[,c(10,13,18)]
View(D1_referees)
#rename column names
names(D1_referees)[1] <- paste("HomeTeam")
names(D1_referees)[2] <- paste("AwayTeam")
D1_referees$HomeTeam <- mgsub(D1_referees$HomeTeam,c("Bayer Leverkusen","Darmstadt 98","Eint Frankfurt","Mainz 05","M'Gladbach","Köln","Gladbach"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln","Mgladbach"))
D1_referees$AwayTeam <- mgsub(D1_referees$AwayTeam,c("Bayer Leverkusen","Darmstadt 98","Eint Frankfurt","Mainz 05","M'Gladbach","Köln","Gladbach"),c("Leverkusen","Darmstadt","Ein Frankfurt","Mainz","Mgladbach","FC Koln","Mgladbach"))
D1_referees$matchid <- paste(D1_referees$HomeTeam,D1_referees$AwayTeam,sep = "-")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,D1_referees)


library('sqldf')
require('RH2')


Home_xG <- c()
Home_xG <- sqldf("SELECT bundes_summary.matchid,bundes_summary.Home_xG FROM bundes_summary INNER JOIN BUNDES_spread ON bundes_summary.matchid = BUNDES_spread.matchid GROUP BY bundes_summary.matchid")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT bundes_summary.matchid,bundes_summary.Away_xG FROM bundes_summary INNER JOIN BUNDES_spread ON bundes_summary.matchid = BUNDES_spread.matchid GROUP BY bundes_summary.matchid")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Away_xG)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS FH_HYC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Event_Half = '1' AND bundes_summary.Home_Away = 'Home' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,FH_HYC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS FH_AYC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Event_Half = '1' AND bundes_summary.Home_Away = 'Away' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,FH_AYC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS FH_HRC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Red Card' AND bundes_summary.Event_Half = '1' AND bundes_summary.Home_Away = 'Home' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,FH_HRC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS FH_ARC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Red Card' AND bundes_summary.Event_Half = '1' AND bundes_summary.Home_Away = 'Away' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,FH_ARC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS SH_HYC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Event_Half = '2' AND bundes_summary.Home_Away = 'Home' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,SH_HYC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS SH_AYC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Event_Half = '2' AND bundes_summary.Home_Away = 'Away' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,SH_AYC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS SH_HRC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Red Card' AND bundes_summary.Event_Half = '2' AND bundes_summary.Home_Away = 'Home' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,SH_HRC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS SH_ARC FROM bundes_summary WHERE bundes_summary.Event_Type = 'Red Card' AND bundes_summary.Event_Half = '2' AND bundes_summary.Home_Away = 'Away' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,SH_ARC)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

#firsthalf
BUNDES_spread$FH_HomeBookings <- BUNDES_spread$FH_HYC *10 + BUNDES_spread$FH_HRC *25

BUNDES_spread$FH_AwayBookings <- BUNDES_spread$FH_AYC *10 + BUNDES_spread$FH_ARC *25

BUNDES_spread$FH_TotalBookings <- BUNDES_spread$FH_HomeBookings + BUNDES_spread$FH_AwayBookings

#second half
BUNDES_spread$SH_HomeBookings <- BUNDES_spread$SH_HYC *10 + BUNDES_spread$SH_HRC *25

BUNDES_spread$SH_AwayBookings <- BUNDES_spread$SH_AYC *10 + BUNDES_spread$SH_ARC *25

BUNDES_spread$SH_TotalBookings <- BUNDES_spread$SH_HomeBookings + BUNDES_spread$SH_AwayBookings


BUNDES_spread$MultiBookings <- BUNDES_spread$FH_TotalBookings * BUNDES_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT bundes_summary.matchid,SUM(Event_time) AS Home_YCmins FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Home_Away = 'Home' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Home_YCmins)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT bundes_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM bundes_summary WHERE bundes_summary.Event_Type = 'Red Card' AND bundes_summary.Home_Away = 'Home' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Home_RCmins)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT bundes_summary.matchid,SUM(Event_time) AS Away_YCmins FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Home_Away = 'Away' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Away_YCmins)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT bundes_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM bundes_summary WHERE bundes_summary.Event_Type = 'Red Card' AND bundes_summary.Home_Away = 'Away' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Away_RCmins)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

BUNDES_spread$Home_TotalCardmins <- BUNDES_spread$Home_YCmins + BUNDES_spread$Home_RCmins
BUNDES_spread$Away_TotalCardmins <- BUNDES_spread$Away_YCmins + BUNDES_spread$Away_RCmins
BUNDES_spread$match_TotalCardmins <- BUNDES_spread$Home_TotalCardmins + BUNDES_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT bundes_summary.matchid,MIN(bundes_summary.Event_Time) AS Home_first_YCTime FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Home_Away = 'Home' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Home_first_YCTime)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT bundes_summary.matchid,MIN(bundes_summary.Event_Time) AS Away_first_YCTime FROM bundes_summary WHERE bundes_summary.Event_Type = 'Yellow Card' AND bundes_summary.Home_Away = 'Away' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Away_first_YCTime)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)

BUNDES_spread$match_First_YCTime <- pmin(BUNDES_spread$Home_first_YCTime,BUNDES_spread$Away_first_YCTime)

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT bundes_summary.matchid,COUNT(*) AS Penalty FROM bundes_summary WHERE bundes_summary.Event_Type = 'Penalty' GROUP BY bundes_summary.matchid ")
BUNDES_spread <- dplyr::left_join(BUNDES_spread,Penalty)
BUNDES_spread <- BUNDES_spread %>% replace(is.na(.),0)
#calculate match performance
BUNDES_spread$MatchPerfomance <- BUNDES_spread$TG *15 + BUNDES_spread$TY *5 + BUNDES_spread$TR *15 + BUNDES_spread$TC *3 + BUNDES_spread$Penalty *10
View(BUNDES_spread)

unlink('BUNDES_SPREAD.xlsx')
write.xlsx(BUNDES_spread,'BUNDES_SPREAD.xlsx')

######################################################################################################################

######################################################################################################################
#seriea
#seriea_match_urls <- fb_match_urls(country = "ITA", gender = "M", season_end_year = 2024, tier="1st")
#seriea_summary <- fb_match_summary(match_url = seriea_match_urls)
#write.xlsx(seriea_summary,"seriea_summary.xlsx")
#write.xlsx(seriea_match_urls,'seriea_match_urls.xlsx')
#second time reading algorithm
#first read current rows in seriea_match_urls.xlsx
#read total rows in fbref seriea_match_urls from fb_match_urls function
#find the difference between the two and tail the number to be used to download for new seriea_summary
current_seriea_match_urls_nrows <- nrow(readxl::read_excel('seriea_match_urls.xlsx'))
seriea_match_urls_nrows <- length(fb_match_urls(country = "ITA", gender = "M", season_end_year = 2024, tier="1st"))
new_seriea_match_urls <- tail(fb_match_urls(country = "ITA", gender = "M", season_end_year = 2024, tier="1st"),seriea_match_urls_nrows - current_seriea_match_urls_nrows)

new_seriea_summary <- fb_match_summary(match_url = new_seriea_match_urls)

new_seriea_summary$Home_Team <- mgsub(new_seriea_summary$Home_Team,c("Hellas Verona","Internazionale"),c("Verona","Inter"))
new_seriea_summary$Away_Team <- mgsub(new_seriea_summary$Away_Team,c("Hellas Verona","Internazionale"),c("Verona","Inter"))
new_seriea_summary$Team <- mgsub(new_seriea_summary$Team,c("Hellas Verona","Internazionale"),c("Verona","Inter"))

new_seriea_summary$matchid <- paste(new_seriea_summary$Home_Team,new_seriea_summary$Away_Team,sep = "-")

seriea_summary <- rbind(seriea_summary,new_seriea_summary)
unlink('seriea_summary.xlsx')
write.xlsx(seriea_summary,"seriea_summary.xlsx")

current_seriea_match_urls <- fb_match_urls(country = "ITA", gender = "M", season_end_year = 2024, tier="1st")
unlink('seriea_match_urls.xlsx')
write.xlsx(current_seriea_match_urls,'seriea_match_urls.xlsx')


#reading and writing
seriea_summary <- readxl::read_excel('seriea_summary.xlsx')
seriea_summary <- seriea_summary[,c(-1)]

seriea_summary$Home_Team <- mgsub(seriea_summary$Home_Team,c("Hellas Verona","Internazionale"),c("Verona","Inter"))
seriea_summary$Away_Team <- mgsub(seriea_summary$Away_Team,c("Hellas Verona","Internazionale"),c("Verona","Inter"))
seriea_summary$Team <- mgsub(seriea_summary$Team,c("Hellas Verona","Internazionale"),c("Verona","Inter"))

seriea_summary$matchid <- paste(seriea_summary$Home_Team,seriea_summary$Away_Team,sep = "-")


SERIEA_spread <- subset(allteams20232024,Div =="I1")
SERIEA_spread$matchid <- paste(SERIEA_spread$HomeTeam,SERIEA_spread$AwayTeam,sep = "-")

I1_referees <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2024, tier="1st")
I1_referees <- I1_referees[,c(10,13,18)]
#rename column names
names(I1_referees)[1] <- paste("HomeTeam")
names(I1_referees)[2] <- paste("AwayTeam")
I1_referees$HomeTeam <- mgsub(I1_referees$HomeTeam,c("Hellas Verona","Internazionale"),c("Verona","Inter"))
I1_referees$AwayTeam <- mgsub(I1_referees$AwayTeam,c("Hellas Verona","Internazionale"),c("Verona","Inter"))
I1_referees$matchid <- paste(I1_referees$HomeTeam,I1_referees$AwayTeam,sep = "-")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,I1_referees)


library('sqldf')
require('RH2')


Home_xG <- c()
Home_xG <- sqldf("SELECT seriea_summary.matchid,seriea_summary.Home_xG FROM seriea_summary INNER JOIN SERIEA_spread ON seriea_summary.matchid = SERIEA_spread.matchid GROUP BY seriea_summary.matchid")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT seriea_summary.matchid,seriea_summary.Away_xG FROM seriea_summary INNER JOIN SERIEA_spread ON seriea_summary.matchid = SERIEA_spread.matchid GROUP BY seriea_summary.matchid")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Away_xG)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS FH_HYC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Event_Half = '1' AND seriea_summary.Home_Away = 'Home' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,FH_HYC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS FH_AYC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Event_Half = '1' AND seriea_summary.Home_Away = 'Away' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,FH_AYC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS FH_HRC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Red Card' AND seriea_summary.Event_Half = '1' AND seriea_summary.Home_Away = 'Home' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,FH_HRC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS FH_ARC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Red Card' AND seriea_summary.Event_Half = '1' AND seriea_summary.Home_Away = 'Away' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,FH_ARC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS SH_HYC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Event_Half = '2' AND seriea_summary.Home_Away = 'Home' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,SH_HYC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS SH_AYC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Event_Half = '2' AND seriea_summary.Home_Away = 'Away' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,SH_AYC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS SH_HRC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Red Card' AND seriea_summary.Event_Half = '2' AND seriea_summary.Home_Away = 'Home' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,SH_HRC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS SH_ARC FROM seriea_summary WHERE seriea_summary.Event_Type = 'Red Card' AND seriea_summary.Event_Half = '2' AND seriea_summary.Home_Away = 'Away' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,SH_ARC)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

#firsthalf
SERIEA_spread$FH_HomeBookings <- SERIEA_spread$FH_HYC *10 + SERIEA_spread$FH_HRC *25

SERIEA_spread$FH_AwayBookings <- SERIEA_spread$FH_AYC *10 + SERIEA_spread$FH_ARC *25

SERIEA_spread$FH_TotalBookings <- SERIEA_spread$FH_HomeBookings + SERIEA_spread$FH_AwayBookings

#second half
SERIEA_spread$SH_HomeBookings <- SERIEA_spread$SH_HYC *10 + SERIEA_spread$SH_HRC *25

SERIEA_spread$SH_AwayBookings <- SERIEA_spread$SH_AYC *10 + SERIEA_spread$SH_ARC *25

SERIEA_spread$SH_TotalBookings <- SERIEA_spread$SH_HomeBookings + SERIEA_spread$SH_AwayBookings


SERIEA_spread$MultiBookings <- SERIEA_spread$FH_TotalBookings * SERIEA_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT seriea_summary.matchid,SUM(Event_time) AS Home_YCmins FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Home_Away = 'Home' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Home_YCmins)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT seriea_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM seriea_summary WHERE seriea_summary.Event_Type = 'Red Card' AND seriea_summary.Home_Away = 'Home' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Home_RCmins)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT seriea_summary.matchid,SUM(Event_time) AS Away_YCmins FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Home_Away = 'Away' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Away_YCmins)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT seriea_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM seriea_summary WHERE seriea_summary.Event_Type = 'Red Card' AND seriea_summary.Home_Away = 'Away' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Away_RCmins)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

SERIEA_spread$Home_TotalCardmins <- SERIEA_spread$Home_YCmins + SERIEA_spread$Home_RCmins
SERIEA_spread$Away_TotalCardmins <- SERIEA_spread$Away_YCmins + SERIEA_spread$Away_RCmins
SERIEA_spread$match_TotalCardmins <- SERIEA_spread$Home_TotalCardmins + SERIEA_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT seriea_summary.matchid,MIN(seriea_summary.Event_Time) AS Home_first_YCTime FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Home_Away = 'Home' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Home_first_YCTime)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT seriea_summary.matchid,MIN(seriea_summary.Event_Time) AS Away_first_YCTime FROM seriea_summary WHERE seriea_summary.Event_Type = 'Yellow Card' AND seriea_summary.Home_Away = 'Away' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Away_first_YCTime)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)

SERIEA_spread$match_First_YCTime <- pmin(SERIEA_spread$Home_first_YCTime,SERIEA_spread$Away_first_YCTime)

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT seriea_summary.matchid,COUNT(*) AS Penalty FROM seriea_summary WHERE seriea_summary.Event_Type = 'Penalty' GROUP BY seriea_summary.matchid ")
SERIEA_spread <- dplyr::left_join(SERIEA_spread,Penalty)
SERIEA_spread <- SERIEA_spread %>% replace(is.na(.),0)
#calculate match performance
SERIEA_spread$MatchPerfomance <- SERIEA_spread$TG *15 + SERIEA_spread$TY *5 + SERIEA_spread$TR *15 + SERIEA_spread$TC *3 + SERIEA_spread$Penalty *10



unlink('SERIEA_SPREAD.xlsx')
write.xlsx(SERIEA_spread,'SERIEA_SPREAD.xlsx')
###################################################################################################################################################
###################################################################################################################################################
#Laliga
laliga_match_urls <- fb_match_urls(country = "ESP", gender = "M", season_end_year = 2024, tier="1st")
laliga_summary <- fb_match_summary(match_url = laliga_match_urls)


write.xlsx(laliga_summary,"laliga_summary.xlsx")
write.xlsx(laliga_match_urls,'laliga_match_urls.xlsx')
#second time reading algorithm
#first read current rows in laliga_match_urls.xlsx
#read total rows in fbref laliga_match_urls from fb_match_urls function
#find the difference between the two and tail the number to be used to download for new laliga_summary
current_laliga_match_urls_nrows <- nrow(readxl::read_excel('laliga_match_urls.xlsx'))
laliga_match_urls_nrows <- length(fb_match_urls(country = "ESP", gender = "M", season_end_year = 2024, tier="1st"))
new_laliga_match_urls <- tail(fb_match_urls(country = "ESP", gender = "M", season_end_year = 2024, tier="1st"),laliga_match_urls_nrows - current_laliga_match_urls_nrows)

new_laliga_summary <- fb_match_summary(match_url = new_laliga_match_urls)
sort(unique(laliga_summary$Home_Team))
sp1_teams
new_laliga_summary$Home_Team <- mgsub(new_laliga_summary$Home_Team,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))
new_laliga_summary$Away_Team <- mgsub(new_laliga_summary$Away_Team,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))
new_laliga_summary$Team <- mgsub(new_laliga_summary$Team,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))

new_laliga_summary$matchid <- paste(new_laliga_summary$Home_Team,new_laliga_summary$Away_Team,sep = "-")

laliga_summary <- rbind(laliga_summary,new_laliga_summary)
unlink('laliga_summary.xlsx')
write.xlsx(laliga_summary,"laliga_summary.xlsx")

current_laliga_match_urls <- fb_match_urls(country = "ESP", gender = "M", season_end_year = 2024, tier="1st")
unlink('laliga_match_urls.xlsx')
write.xlsx(current_laliga_match_urls,'laliga_match_urls.xlsx')
View(current_laliga_match_urls)

#reading and writing
laliga_summary <- readxl::read_excel('laliga_summary.xlsx')
laliga_summary <- laliga_summary[,c(-1)]

laliga_summary$Home_Team <- mgsub(laliga_summary$Home_Team,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))
laliga_summary$Away_Team <- mgsub(laliga_summary$Away_Team,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))
laliga_summary$Team <- mgsub(laliga_summary$Team,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))

laliga_summary$matchid <- paste(laliga_summary$Home_Team,laliga_summary$Away_Team,sep = "-")


LALIGA_spread <- subset(allteams20232024,Div =="SP1")
LALIGA_spread$matchid <- paste(LALIGA_spread$HomeTeam,LALIGA_spread$AwayTeam,sep = "-")

SP1_referees <- fb_match_results(country = "ESP", gender = "M", season_end_year = 2024, tier="1st")
SP1_referees <- SP1_referees[,c(10,13,18)]
#rename column names
names(SP1_referees)[1] <- paste("HomeTeam")
names(SP1_referees)[2] <- paste("AwayTeam")
SP1_referees$HomeTeam <- mgsub(SP1_referees$HomeTeam,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))
SP1_referees$AwayTeam <- mgsub(SP1_referees$AwayTeam,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Real Betis","Real Sociedad","Rayo Vallecano"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Betis","Sociedad","Vallecano"))
SP1_referees$matchid <- paste(SP1_referees$HomeTeam,SP1_referees$AwayTeam,sep = "-")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,SP1_referees)

library('sqldf')
require('RH2')


Home_xG <- c()
Home_xG <- sqldf("SELECT laliga_summary.matchid,laliga_summary.Home_xG FROM laliga_summary INNER JOIN LALIGA_spread ON laliga_summary.matchid = LALIGA_spread.matchid GROUP BY laliga_summary.matchid")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT laliga_summary.matchid,laliga_summary.Away_xG FROM laliga_summary INNER JOIN LALIGA_spread ON laliga_summary.matchid = LALIGA_spread.matchid GROUP BY laliga_summary.matchid")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Away_xG)
View(LALIGA_spread)
#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS FH_HYC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Event_Half = '1' AND laliga_summary.Home_Away = 'Home' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,FH_HYC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS FH_AYC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Event_Half = '1' AND laliga_summary.Home_Away = 'Away' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,FH_AYC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS FH_HRC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Red Card' AND laliga_summary.Event_Half = '1' AND laliga_summary.Home_Away = 'Home' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,FH_HRC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS FH_ARC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Red Card' AND laliga_summary.Event_Half = '1' AND laliga_summary.Home_Away = 'Away' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,FH_ARC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS SH_HYC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Event_Half = '2' AND laliga_summary.Home_Away = 'Home' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,SH_HYC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS SH_AYC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Event_Half = '2' AND laliga_summary.Home_Away = 'Away' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,SH_AYC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS SH_HRC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Red Card' AND laliga_summary.Event_Half = '2' AND laliga_summary.Home_Away = 'Home' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,SH_HRC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS SH_ARC FROM laliga_summary WHERE laliga_summary.Event_Type = 'Red Card' AND laliga_summary.Event_Half = '2' AND laliga_summary.Home_Away = 'Away' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,SH_ARC)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

#firsthalf
LALIGA_spread$FH_HomeBookings <- LALIGA_spread$FH_HYC *10 + LALIGA_spread$FH_HRC *25

LALIGA_spread$FH_AwayBookings <- LALIGA_spread$FH_AYC *10 + LALIGA_spread$FH_ARC *25

LALIGA_spread$FH_TotalBookings <- LALIGA_spread$FH_HomeBookings + LALIGA_spread$FH_AwayBookings

#second half
LALIGA_spread$SH_HomeBookings <- LALIGA_spread$SH_HYC *10 + LALIGA_spread$SH_HRC *25

LALIGA_spread$SH_AwayBookings <- LALIGA_spread$SH_AYC *10 + LALIGA_spread$SH_ARC *25

LALIGA_spread$SH_TotalBookings <- LALIGA_spread$SH_HomeBookings + LALIGA_spread$SH_AwayBookings


LALIGA_spread$MultiBookings <- LALIGA_spread$FH_TotalBookings * LALIGA_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT laliga_summary.matchid,SUM(Event_time) AS Home_YCmins FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Home_Away = 'Home' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Home_YCmins)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT laliga_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM laliga_summary WHERE laliga_summary.Event_Type = 'Red Card' AND laliga_summary.Home_Away = 'Home' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Home_RCmins)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT laliga_summary.matchid,SUM(Event_time) AS Away_YCmins FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Home_Away = 'Away' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Away_YCmins)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT laliga_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM laliga_summary WHERE laliga_summary.Event_Type = 'Red Card' AND laliga_summary.Home_Away = 'Away' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Away_RCmins)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

LALIGA_spread$Home_TotalCardmins <- LALIGA_spread$Home_YCmins + LALIGA_spread$Home_RCmins
LALIGA_spread$Away_TotalCardmins <- LALIGA_spread$Away_YCmins + LALIGA_spread$Away_RCmins
LALIGA_spread$match_TotalCardmins <- LALIGA_spread$Home_TotalCardmins + LALIGA_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT laliga_summary.matchid,MIN(laliga_summary.Event_Time) AS Home_first_YCTime FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Home_Away = 'Home' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Home_first_YCTime)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT laliga_summary.matchid,MIN(laliga_summary.Event_Time) AS Away_first_YCTime FROM laliga_summary WHERE laliga_summary.Event_Type = 'Yellow Card' AND laliga_summary.Home_Away = 'Away' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Away_first_YCTime)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)

LALIGA_spread$match_First_YCTime <- pmin(LALIGA_spread$Home_first_YCTime,LALIGA_spread$Away_first_YCTime)
#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT laliga_summary.matchid,COUNT(*) AS Penalty FROM laliga_summary WHERE laliga_summary.Event_Type = 'Penalty' GROUP BY laliga_summary.matchid ")
LALIGA_spread <- dplyr::left_join(LALIGA_spread,Penalty)
LALIGA_spread <- LALIGA_spread %>% replace(is.na(.),0)
#calculate match performance
LALIGA_spread$MatchPerfomance <- LALIGA_spread$TG *15 + LALIGA_spread$TY *5 + LALIGA_spread$TR *15 + LALIGA_spread$TC *3 + LALIGA_spread$Penalty *10


unlink('LALIGA_SPREAD.xlsx')
write.xlsx(LALIGA_spread,'LALIGA_SPREAD.xlsx')

################################################################################################################
################################################################################################################
#Ligue one
#ligueone_match_urls <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2024, tier="1st")
#ligueone_summary <- fb_match_summary(match_url = ligueone_match_urls)


#write.xlsx(ligueone_summary,"ligueone_summary.xlsx")
#write.xlsx(ligueone_match_urls,'ligueone_match_urls.xlsx')
#second time reading algorithm
#first read current rows in ligueone_match_urls.xlsx
#read total rows in fbref ligueone_match_urls from fb_match_urls function
#find the difference between the two and tail the number to be used to download for new ligueone_summary
current_ligueone_match_urls_nrows <- nrow(readxl::read_excel('ligueone_match_urls.xlsx'))
ligueone_match_urls_nrows <- length(fb_match_urls(country = "FRA", gender = "M", season_end_year = 2024, tier="1st"))
new_ligueone_match_urls <- tail(fb_match_urls(country = "FRA", gender = "M", season_end_year = 2024, tier="1st"),ligueone_match_urls_nrows - current_ligueone_match_urls_nrows)
sort(unique(ligueone_summary$Home_Team))
f1_teams
new_ligueone_summary <- fb_match_summary(match_url = new_ligueone_match_urls)

new_ligueone_summary$Home_Team <- mgsub(new_ligueone_summary$Home_Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))
new_ligueone_summary$Away_Team <- mgsub(new_ligueone_summary$Away_Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))
new_ligueone_summary$Team <- mgsub(new_ligueone_summary$Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))

new_ligueone_summary$matchid <- paste(new_ligueone_summary$Home_Team,new_ligueone_summary$Away_Team,sep = "-")

ligueone_summary <- rbind(ligueone_summary,new_ligueone_summary)
unlink('ligueone_summary.xlsx')
write.xlsx(ligueone_summary,"ligueone_summary.xlsx")

current_ligueone_match_urls <- fb_match_urls(country = "FRA", gender = "M", season_end_year = 2024, tier="1st")
unlink('ligueone_match_urls.xlsx')
write.xlsx(current_ligueone_match_urls,'ligueone_match_urls.xlsx')
View(current_ligueone_match_urls)

#reading and writing
ligueone_summary <- readxl::read_excel('ligueone_summary.xlsx')
ligueone_summary <- ligueone_summary[,c(-1)]

ligueone_summary$Home_Team <- mgsub(ligueone_summary$Home_Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))
ligueone_summary$Away_Team <- mgsub(ligueone_summary$Away_Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))
ligueone_summary$Team <- mgsub(ligueone_summary$Team,c("Clermont Foot","Paris Saint-Germain"),c("Clermont","Paris SG"))

ligueone_summary$matchid <- paste(ligueone_summary$Home_Team,ligueone_summary$Away_Team,sep = "-")


LIGUEONE_spread <- subset(allteams20232024,Div =="F1")
LIGUEONE_spread$matchid <- paste(LIGUEONE_spread$HomeTeam,LIGUEONE_spread$AwayTeam,sep = "-")

F1_referees <- fb_match_results(country = "FRA", gender = "M", season_end_year = 2024, tier="1st")
F1_referees <- F1_referees[,c(10,13,18)]
#rename column names
names(F1_referees)[1] <- paste("HomeTeam")
names(F1_referees)[2] <- paste("AwayTeam")
F1_referees$HomeTeam <- mgsub(F1_referees$HomeTeam,c("Clermont Foot","Paris S-G"),c("Clermont","Paris SG"))
F1_referees$AwayTeam <- mgsub(F1_referees$AwayTeam,c("Clermont Foot","Paris S-G"),c("Clermont","Paris SG"))
F1_referees$matchid <- paste(F1_referees$HomeTeam,F1_referees$AwayTeam,sep = "-")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,F1_referees)


library('sqldf')
require('RH2')


Home_xG <- c()
Home_xG <- sqldf("SELECT ligueone_summary.matchid,ligueone_summary.Home_xG FROM ligueone_summary INNER JOIN LIGUEONE_spread ON ligueone_summary.matchid = LIGUEONE_spread.matchid GROUP BY ligueone_summary.matchid")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Home_xG)

Away_xG <- c()
Away_xG <- sqldf("SELECT ligueone_summary.matchid,ligueone_summary.Away_xG FROM ligueone_summary INNER JOIN LIGUEONE_spread ON ligueone_summary.matchid = LIGUEONE_spread.matchid GROUP BY ligueone_summary.matchid")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Away_xG)
View(LIGUEONE_spread)
#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS FH_HYC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Event_Half = '1' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,FH_HYC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS FH_AYC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Event_Half = '1' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,FH_AYC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS FH_HRC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Red Card' AND ligueone_summary.Event_Half = '1' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,FH_HRC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS FH_ARC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Red Card' AND ligueone_summary.Event_Half = '1' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,FH_ARC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS SH_HYC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Event_Half = '2' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,SH_HYC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS SH_AYC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Event_Half = '2' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,SH_AYC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS SH_HRC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Red Card' AND ligueone_summary.Event_Half = '2' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,SH_HRC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS SH_ARC FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Red Card' AND ligueone_summary.Event_Half = '2' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,SH_ARC)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

#firsthalf
LIGUEONE_spread$FH_HomeBookings <- LIGUEONE_spread$FH_HYC *10 + LIGUEONE_spread$FH_HRC *25

LIGUEONE_spread$FH_AwayBookings <- LIGUEONE_spread$FH_AYC *10 + LIGUEONE_spread$FH_ARC *25

LIGUEONE_spread$FH_TotalBookings <- LIGUEONE_spread$FH_HomeBookings + LIGUEONE_spread$FH_AwayBookings

#second half
LIGUEONE_spread$SH_HomeBookings <- LIGUEONE_spread$SH_HYC *10 + LIGUEONE_spread$SH_HRC *25

LIGUEONE_spread$SH_AwayBookings <- LIGUEONE_spread$SH_AYC *10 + LIGUEONE_spread$SH_ARC *25

LIGUEONE_spread$SH_TotalBookings <- LIGUEONE_spread$SH_HomeBookings + LIGUEONE_spread$SH_AwayBookings


LIGUEONE_spread$MultiBookings <- LIGUEONE_spread$FH_TotalBookings * LIGUEONE_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT ligueone_summary.matchid,SUM(Event_time) AS Home_YCmins FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Home_YCmins)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT ligueone_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Red Card' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Home_RCmins)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT ligueone_summary.matchid,SUM(Event_time) AS Away_YCmins FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Away_YCmins)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT ligueone_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Red Card' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Away_RCmins)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

LIGUEONE_spread$Home_TotalCardmins <- LIGUEONE_spread$Home_YCmins + LIGUEONE_spread$Home_RCmins
LIGUEONE_spread$Away_TotalCardmins <- LIGUEONE_spread$Away_YCmins + LIGUEONE_spread$Away_RCmins
LIGUEONE_spread$match_TotalCardmins <- LIGUEONE_spread$Home_TotalCardmins + LIGUEONE_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT ligueone_summary.matchid,MIN(ligueone_summary.Event_Time) AS Home_first_YCTime FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Home_Away = 'Home' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Home_first_YCTime)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT ligueone_summary.matchid,MIN(ligueone_summary.Event_Time) AS Away_first_YCTime FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Yellow Card' AND ligueone_summary.Home_Away = 'Away' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Away_first_YCTime)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)

LIGUEONE_spread$match_First_YCTime <- pmin(LIGUEONE_spread$Home_first_YCTime,LIGUEONE_spread$Away_first_YCTime)

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT ligueone_summary.matchid,COUNT(*) AS Penalty FROM ligueone_summary WHERE ligueone_summary.Event_Type = 'Penalty' GROUP BY ligueone_summary.matchid ")
LIGUEONE_spread <- dplyr::left_join(LIGUEONE_spread,Penalty)
LIGUEONE_spread <- LIGUEONE_spread %>% replace(is.na(.),0)
#calculate match performance
LIGUEONE_spread$MatchPerfomance <- LIGUEONE_spread$TG *15 + LIGUEONE_spread$TY *5 + LIGUEONE_spread$TR *15 + LIGUEONE_spread$TC *3 + LIGUEONE_spread$Penalty *10



unlink('LIGUEONE_SPREAD.xlsx')
write.xlsx(LIGUEONE_spread,'LIGUEONE_SPREAD.xlsx')

##################################################################################################################################
##################################################################################################################################
#CREATE FINAL SPREAD
E0_spreaducl <- readxl::read_excel('../Rsoccer/E0_spread.xlsx')
E0_spreaducl <- E0_spreaducl[,c(-1)]

E0_advstats <- readxl::read_excel('EPL_SPREAD.xlsx')
E0_advstats <- E0_advstats[,-1]
EPL_FINALSPREAD <- dplyr::left_join(E0_spreaducl,EPL_spread)
EPL_FINALSPREAD <- EPL_FINALSPREAD %>% dplyr::relocate(51,.after = 37)
unlink('EPL_FINALSPREAD')
write.xlsx(EPL_FINALSPREAD,'EPL_FINALSPREAD.xlsx')
###################################################################################################################################
D1_spreaducl <- readxl::read_excel('../Rsoccer/D1_spread.xlsx')
D1_spreaducl <- D1_spreaducl[,c(-1)]

D1_advstats <- readxl::read_excel('BUNDES_SPREAD.xlsx')
D1_advstats <- D1_advstats[,-1]
BUNDES_FINALSPREAD <- dplyr::left_join(D1_spreaducl,BUNDES_spread)
BUNDES_FINALSPREAD <- BUNDES_FINALSPREAD %>% dplyr::relocate(51,.after = 37)
unlink('BUNDES_FINALSPREAD')
write.xlsx(BUNDES_FINALSPREAD,'BUNDES_FINALSPREAD.xlsx')
#####################################################################################################################################
I1_spreaducl <- readxl::read_excel('../Rsoccer/I1_spread.xlsx')
I1_spreaducl <- I1_spreaducl[,c(-1)]

I1_advstats <- readxl::read_excel('SERIEA_SPREAD.xlsx')
I1_advstats <- I1_advstats[,-1]
SERIEA_FINALSPREAD <- dplyr::left_join(I1_spreaducl,SERIEA_spread)
SERIEA_FINALSPREAD <- SERIEA_FINALSPREAD %>% dplyr::relocate(51,.after = 37)
unlink('SERIEA_FINALSPREAD')
write.xlsx(SERIEA_FINALSPREAD,'SERIEA_FINALSPREAD.xlsx')
#####################################################################################################################################
SP1_spreaducl <- readxl::read_excel('../Rsoccer/SP1_spread.xlsx')
SP1_spreaducl <- SP1_spreaducl[,c(-1)]

SP1_advstats <- readxl::read_excel('LALIGA_SPREAD.xlsx')
SP1_advstats <- SP1_advstats[,-1]
LALIGA_FINALSPREAD <- dplyr::left_join(SP1_spreaducl,LALIGA_spread)
LALIGA_FINALSPREAD <- LALIGA_FINALSPREAD %>% dplyr::relocate(51,.after = 37)
unlink('LALIGA_FINALSPREAD')
write.xlsx(LALIGA_FINALSPREAD,'LALIGA_FINALSPREAD.xlsx')
#################################################################################################
#CREATE FINAL SPREAD
F1_spreaducl <- readxl::read_excel('../Rsoccer/F1_spread.xlsx')
F1_spreaducl <- F1_spreaducl[,c(-1)]

F1_advstats <- readxl::read_excel('LIGUEONE_SPREAD.xlsx')
F1_advstats <- F1_advstats[,-1]
LIGUEONE_FINALSPREAD <- dplyr::left_join(F1_spreaducl,LIGUEONE_spread)
LIGUEONE_FINALSPREAD <- LIGUEONE_FINALSPREAD %>% dplyr::relocate(51,.after = 37)
unlink('LIGUEONE_FINALSPREAD')
write.xlsx(LIGUEONE_FINALSPREAD,'LIGUEONE_FINALSPREAD.xlsx')

#############################################################################################################
#REFEREES
EPL_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM EPL_FINALSPREAD GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('EPL_refereestats.xlsx')
write.xlsx(EPL_refereestats,'EPL_refereestats.xlsx')
###########################################################
BUNDES_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM BUNDES_FINALSPREAD GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('BUNDES_refereestats.xlsx')
write.xlsx(BUNDES_refereestats,'BUNDES_refereestats.xlsx')
###########################################################
SERIEA_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM SERIEA_FINALSPREAD GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('SERIEA_refereestats.xlsx')
write.xlsx(SERIEA_refereestats,'SERIEA_refereestats.xlsx')
###########################################################
LALIGA_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM LALIGA_FINALSPREAD GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('LALIGA_refereestats.xlsx')
write.xlsx(LALIGA_refereestats,'LALIGA_refereestats.xlsx')
############################################################
LIGUEONE_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM LIGUEONE_FINALSPREAD GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('LIGUEONE_refereestats.xlsx')
write.xlsx(LIGUEONE_refereestats,'LIGUEONE_refereestats.xlsx')

##############################################################




















