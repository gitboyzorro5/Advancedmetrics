#analyse times of first yellow card
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library(stringr)
library(stringi)
BIGFIVE_analytics <- readxl::read_excel('BIGFIVE20232024.xlsx')
BIGFIVE_analytics <- BIGFIVE_analytics[,-1]
BIGFIVE_analytics <- as.data.frame(BIGFIVE_analytics)

#E0
E0_matchfirstgoal <- subset(BIGFIVE_analytics, Div == "E0")

e0_matchfirstgoal_h <- tapply(E0_matchfirstgoal$Home_first_GoalTime, E0_matchfirstgoal[c("HomeTeam", "Date")],mean)
e0_matchfirstgoal_a <- tapply(E0_matchfirstgoal$Away_first_GoalTime, E0_matchfirstgoal[c("AwayTeam", "Date")],mean)

e0_matchfirstgoal_h[is.na(e0_matchfirstgoal_h)] <- ""
e0_matchfirstgoal_a[is.na(e0_matchfirstgoal_a)] <- ""

for(e0_rowhgoal in 1:nrow(e0_matchfirstgoal_h)) {
  for(e0_colhgoal in 1:ncol(e0_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(e0_rowagoal in 1:nrow(e0_matchfirstgoal_a)) {
      for(e0_colagoal in 1:ncol(e0_matchfirstgoal_a)) {
        ifelse(!e0_matchfirstgoal_a[e0_rowagoal,e0_colagoal]=="",e0_matchfirstgoal_h[e0_rowagoal,e0_colagoal] <- e0_matchfirstgoal_a[e0_rowagoal,e0_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

e0_home_games <- c()
e0_away_games <-c()

for (i_e0 in 1:length(e0_teams))
{

  e0_home_games[i_e0] <- nrow(E0_matchfirstgoal[E0_matchfirstgoal$HomeTeam == e0_teams[i_e0],])
  e0_away_games[i_e0]  <- nrow(E0_matchfirstgoal[E0_matchfirstgoal$AwayTeam == e0_teams[i_e0],])
}
e0_games_played <- e0_home_games + e0_away_games
e0_last_n_games <- e0_games_played[1]

e0_totalrounds <-  (length(e0_teams) - 1 )*2

final_e0_mfgoal <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_mfgoal <- c()
sum_e0_zero_mfgoal <- c()
sum_e0_1to15_mfgoal <- c()
sum_e0_15to30_mfgoal <- c()
sum_e0_30to45_mfgoal <- c()
sum_e0_45to60_mfgoal <- c()
sum_e0_60to75_mfgoal <- c()
sum_e0_75to100_mfgoal <- c()
avgr_e0_mfgoal <- c()
sdr_e0_mfgoal <- c()
l6_form_e0_mfgoalsplitted <- c()
form_e0_mfgoal <- c()
for(index_e0_mfgoal in 1:length(e0_teams))
{
  for(index_e0_mfgoal_cols in 1:e0_totalrounds)
  {
    index_e0_mfgoal  <- row.names(e0_matchfirstgoal_h) == e0_teams[index_e0_mfgoal]
    form_e0_mfgoal <- e0_matchfirstgoal_h[index_e0_mfgoal ]
    deleted_form_e0_mfgoal <- form_e0_mfgoal[!form_e0_mfgoal[] == ""]
    l6_form_e0_mfgoal <- tail(deleted_form_e0_mfgoal,e0_last_n_games)
    l6_form_e0_mfgoal <- as.numeric(l6_form_e0_mfgoal)
    suml6_e0_mfgoal[index_e0_mfgoal] <- sum(l6_form_e0_mfgoal)
    suml6_e0_mfgoal[index_e0_mfgoal] <- paste(suml6_e0_mfgoal[index_e0_mfgoal],sep = "")
    sum_e0_zero_mfgoal[index_e0_mfgoal] <- length(which(l6_form_e0_mfgoal == 0))
    sum_e0_zero_mfgoal[index_e0_mfgoal] <- paste(sum_e0_zero_mfgoal[index_e0_mfgoal],sep = "")
    sum_e0_1to15_mfgoal[index_e0_mfgoal] <- length(which(l6_form_e0_mfgoal >= 1 & l6_form_e0_mfgoal <= 15))
    sum_e0_1to15_mfgoal[index_e0_mfgoal] <- paste(sum_e0_1to15_mfgoal[index_e0_mfgoal],sep = "")
    sum_e0_15to30_mfgoal[index_e0_mfgoal] <- length(which(l6_form_e0_mfgoal >= 15 & l6_form_e0_mfgoal <= 30))
    sum_e0_15to30_mfgoal[index_e0_mfgoal] <- paste(sum_e0_15to30_mfgoal[index_e0_mfgoal],sep = "")
    sum_e0_30to45_mfgoal[index_e0_mfgoal] <- length(which(l6_form_e0_mfgoal >= 30 & l6_form_e0_mfgoal <= 45))
    sum_e0_30to45_mfgoal[index_e0_mfgoal] <- paste(sum_e0_30to45_mfgoal[index_e0_mfgoal],sep = "")
    sum_e0_45to60_mfgoal[index_e0_mfgoal] <- length(which(l6_form_e0_mfgoal >= 45 & l6_form_e0_mfgoal <= 60))
    sum_e0_45to60_mfgoal[index_e0_mfgoal] <- paste(sum_e0_45to60_mfgoal[index_e0_mfgoal],sep = "")
    sum_e0_60to75_mfgoal[index_e0_mfgoal] <- length(which(l6_form_e0_mfgoal >= 60 & l6_form_e0_mfgoal <= 75))
    sum_e0_60to75_mfgoal[index_e0_mfgoal] <- paste(sum_e0_60to75_mfgoal[index_e0_mfgoal],sep = "")
    sum_e0_75to100_mfgoal[index_e0_mfgoal] <- length(which(l6_form_e0_mfgoal >= 75 & l6_form_e0_mfgoal <= 100))
    sum_e0_75to100_mfgoal[index_e0_mfgoal] <- paste(sum_e0_75to100_mfgoal[index_e0_mfgoal],sep = "")
    avgr_e0_mfgoal[index_e0_mfgoal] <- mean(l6_form_e0_mfgoal)
    avgr_e0_mfgoal[index_e0_mfgoal] <- paste(avgr_e0_mfgoal[index_e0_mfgoal],sep = "")
    sdr_e0_mfgoal[index_e0_mfgoal] <- sd(l6_form_e0_mfgoal)
    sdr_e0_mfgoal[index_e0_mfgoal] <- paste(sdr_e0_mfgoal[index_e0_mfgoal],sep = "")
    l6_form_e0_mfgoal <- as.character(l6_form_e0_mfgoal)
    #l6_form_e0_mfgoal_flattened <- stri_paste(l6_form_e0_mfgoal,collapse = '')
    #l6_form_e0_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_e0_mfgoal_flattened),"")[[1]])
    final_e0_mfgoal[index_e0_mfgoal,index_e0_mfgoal_cols] <- l6_form_e0_mfgoal[index_e0_mfgoal_cols]
  }
}

final_e0_mfgoal[is.na(final_e0_mfgoal)] <- ""
e0_matchfirstgoalmatrix <- cbind(e0_teams,final_e0_mfgoal,suml6_e0_mfgoal,sum_e0_zero_mfgoal,sum_e0_1to15_mfgoal,sum_e0_15to30_mfgoal,sum_e0_30to45_mfgoal,sum_e0_45to60_mfgoal,sum_e0_60to75_mfgoal,sum_e0_75to100_mfgoal,avgr_e0_mfgoal,sdr_e0_mfgoal)

unlink('Analytics/BIGFIVE/MatchFirstGoalanalysis.xlsx')
write.xlsx(e0_matchfirstgoalmatrix,"Analytics/BIGFIVE/MatchFirstGoalanalysis.xlsx", sheetName = "E0")
####################################################################################################################################################################################
####################################################################################################################################################################################
#SP1
SP1_matchfirstgoal <- subset(BIGFIVE_analytics, Div == "SP1")

sp1_matchfirstgoal_h <- tapply(SP1_matchfirstgoal$Home_first_GoalTime, SP1_matchfirstgoal[c("HomeTeam", "Date")],mean)
sp1_matchfirstgoal_a <- tapply(SP1_matchfirstgoal$Away_first_GoalTime, SP1_matchfirstgoal[c("AwayTeam", "Date")],mean)

sp1_matchfirstgoal_h[is.na(sp1_matchfirstgoal_h)] <- ""
sp1_matchfirstgoal_a[is.na(sp1_matchfirstgoal_a)] <- ""

for(sp1_rowhgoal in 1:nrow(sp1_matchfirstgoal_h)) {
  for(sp1_colhgoal in 1:ncol(sp1_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowagoal in 1:nrow(sp1_matchfirstgoal_a)) {
      for(sp1_colagoal in 1:ncol(sp1_matchfirstgoal_a)) {
        ifelse(!sp1_matchfirstgoal_a[sp1_rowagoal,sp1_colagoal]=="",sp1_matchfirstgoal_h[sp1_rowagoal,sp1_colagoal] <- sp1_matchfirstgoal_a[sp1_rowagoal,sp1_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

sp1_home_games <- c()
sp1_away_games <-c()

for (i_sp1 in 1:length(sp1_teams))
{

  sp1_home_games[i_sp1] <- nrow(SP1_matchfirstgoal[SP1_matchfirstgoal$HomeTeam == sp1_teams[i_sp1],])
  sp1_away_games[i_sp1]  <- nrow(SP1_matchfirstgoal[SP1_matchfirstgoal$AwayTeam == sp1_teams[i_sp1],])
}
sp1_games_played <- sp1_home_games + sp1_away_games
sp1_last_n_games <- sp1_games_played[1]

sp1_totalrounds <-  (length(sp1_teams) - 1 )*2

final_sp1_mfgoal <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_mfgoal <- c()
sum_sp1_zero_mfgoal <- c()
sum_sp1_1to15_mfgoal <- c()
sum_sp1_15to30_mfgoal <- c()
sum_sp1_30to45_mfgoal <- c()
sum_sp1_45to60_mfgoal <- c()
sum_sp1_60to75_mfgoal <- c()
sum_sp1_75to100_mfgoal <- c()
avgr_sp1_mfgoal <- c()
sdr_sp1_mfgoal <- c()
l6_form_sp1_mfgoalsplitted <- c()
form_sp1_mfgoal <- c()
for(index_sp1_mfgoal in 1:length(sp1_teams))
{
  for(index_sp1_mfgoal_cols in 1:sp1_totalrounds)
  {
    index_sp1_mfgoal  <- row.names(sp1_matchfirstgoal_h) == sp1_teams[index_sp1_mfgoal]
    form_sp1_mfgoal <- sp1_matchfirstgoal_h[index_sp1_mfgoal ]
    deleted_form_sp1_mfgoal <- form_sp1_mfgoal[!form_sp1_mfgoal[] == ""]
    l6_form_sp1_mfgoal <- tail(deleted_form_sp1_mfgoal,sp1_last_n_games)
    l6_form_sp1_mfgoal <- as.numeric(l6_form_sp1_mfgoal)
    suml6_sp1_mfgoal[index_sp1_mfgoal] <- sum(l6_form_sp1_mfgoal)
    suml6_sp1_mfgoal[index_sp1_mfgoal] <- paste(suml6_sp1_mfgoal[index_sp1_mfgoal],sep = "")
    sum_sp1_zero_mfgoal[index_sp1_mfgoal] <- length(which(l6_form_sp1_mfgoal == 0))
    sum_sp1_zero_mfgoal[index_sp1_mfgoal] <- paste(sum_sp1_zero_mfgoal[index_sp1_mfgoal],sep = "")
    sum_sp1_1to15_mfgoal[index_sp1_mfgoal] <- length(which(l6_form_sp1_mfgoal >= 1 & l6_form_sp1_mfgoal <= 15))
    sum_sp1_1to15_mfgoal[index_sp1_mfgoal] <- paste(sum_sp1_1to15_mfgoal[index_sp1_mfgoal],sep = "")
    sum_sp1_15to30_mfgoal[index_sp1_mfgoal] <- length(which(l6_form_sp1_mfgoal >= 15 & l6_form_sp1_mfgoal <= 30))
    sum_sp1_15to30_mfgoal[index_sp1_mfgoal] <- paste(sum_sp1_15to30_mfgoal[index_sp1_mfgoal],sep = "")
    sum_sp1_30to45_mfgoal[index_sp1_mfgoal] <- length(which(l6_form_sp1_mfgoal >= 30 & l6_form_sp1_mfgoal <= 45))
    sum_sp1_30to45_mfgoal[index_sp1_mfgoal] <- paste(sum_sp1_30to45_mfgoal[index_sp1_mfgoal],sep = "")
    sum_sp1_45to60_mfgoal[index_sp1_mfgoal] <- length(which(l6_form_sp1_mfgoal >= 45 & l6_form_sp1_mfgoal <= 60))
    sum_sp1_45to60_mfgoal[index_sp1_mfgoal] <- paste(sum_sp1_45to60_mfgoal[index_sp1_mfgoal],sep = "")
    sum_sp1_60to75_mfgoal[index_sp1_mfgoal] <- length(which(l6_form_sp1_mfgoal >= 60 & l6_form_sp1_mfgoal <= 75))
    sum_sp1_60to75_mfgoal[index_sp1_mfgoal] <- paste(sum_sp1_60to75_mfgoal[index_sp1_mfgoal],sep = "")
    sum_sp1_75to100_mfgoal[index_sp1_mfgoal] <- length(which(l6_form_sp1_mfgoal >= 75 & l6_form_sp1_mfgoal <= 100))
    sum_sp1_75to100_mfgoal[index_sp1_mfgoal] <- paste(sum_sp1_75to100_mfgoal[index_sp1_mfgoal],sep = "")
    avgr_sp1_mfgoal[index_sp1_mfgoal] <- mean(l6_form_sp1_mfgoal)
    avgr_sp1_mfgoal[index_sp1_mfgoal] <- paste(avgr_sp1_mfgoal[index_sp1_mfgoal],sep = "")
    sdr_sp1_mfgoal[index_sp1_mfgoal] <- sd(l6_form_sp1_mfgoal)
    sdr_sp1_mfgoal[index_sp1_mfgoal] <- paste(sdr_sp1_mfgoal[index_sp1_mfgoal],sep = "")
    l6_form_sp1_mfgoal <- as.character(l6_form_sp1_mfgoal)
    #l6_form_sp1_mfgoal_flattened <- stri_paste(l6_form_sp1_mfgoal,collapse = '')
    #l6_form_sp1_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_sp1_mfgoal_flattened),"")[[1]])
    final_sp1_mfgoal[index_sp1_mfgoal,index_sp1_mfgoal_cols] <- l6_form_sp1_mfgoal[index_sp1_mfgoal_cols]
  }
}

final_sp1_mfgoal[is.na(final_sp1_mfgoal)] <- ""
sp1_matchfirstgoalmatrix <- cbind(sp1_teams,final_sp1_mfgoal,suml6_sp1_mfgoal,sum_sp1_zero_mfgoal,sum_sp1_1to15_mfgoal,sum_sp1_15to30_mfgoal,sum_sp1_30to45_mfgoal,sum_sp1_45to60_mfgoal,sum_sp1_60to75_mfgoal,sum_sp1_75to100_mfgoal,avgr_sp1_mfgoal,sdr_sp1_mfgoal)
write.xlsx(sp1_matchfirstgoalmatrix,"Analytics/BIGFIVE/MatchFirstGoalanalysis.xlsx", sheetName = "SP1", append = TRUE)
###################################################################################################################################################################################
###################################################################################################################################################################################
#D1
D1_matchfirstgoal <- subset(BIGFIVE_analytics, Div == "D1")

d1_matchfirstgoal_h <- tapply(D1_matchfirstgoal$Home_first_GoalTime, D1_matchfirstgoal[c("HomeTeam", "Date")],mean)
d1_matchfirstgoal_a <- tapply(D1_matchfirstgoal$Away_first_GoalTime, D1_matchfirstgoal[c("AwayTeam", "Date")],mean)

d1_matchfirstgoal_h[is.na(d1_matchfirstgoal_h)] <- ""
d1_matchfirstgoal_a[is.na(d1_matchfirstgoal_a)] <- ""

for(d1_rowhgoal in 1:nrow(d1_matchfirstgoal_h)) {
  for(d1_colhgoal in 1:ncol(d1_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(d1_rowagoal in 1:nrow(d1_matchfirstgoal_a)) {
      for(d1_colagoal in 1:ncol(d1_matchfirstgoal_a)) {
        ifelse(!d1_matchfirstgoal_a[d1_rowagoal,d1_colagoal]=="",d1_matchfirstgoal_h[d1_rowagoal,d1_colagoal] <- d1_matchfirstgoal_a[d1_rowagoal,d1_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

d1_home_games <- c()
d1_away_games <-c()

for (i_d1 in 1:length(d1_teams))
{

  d1_home_games[i_d1] <- nrow(D1_matchfirstgoal[D1_matchfirstgoal$HomeTeam == d1_teams[i_d1],])
  d1_away_games[i_d1]  <- nrow(D1_matchfirstgoal[D1_matchfirstgoal$AwayTeam == d1_teams[i_d1],])
}
d1_games_played <- d1_home_games + d1_away_games
d1_last_n_games <- d1_games_played[1]

d1_totalrounds <-  (length(d1_teams) - 1 )*2

final_d1_mfgoal <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_mfgoal <- c()
sum_d1_zero_mfgoal <- c()
sum_d1_1to15_mfgoal <- c()
sum_d1_15to30_mfgoal <- c()
sum_d1_30to45_mfgoal <- c()
sum_d1_45to60_mfgoal <- c()
sum_d1_60to75_mfgoal <- c()
sum_d1_75to100_mfgoal <- c()
avgr_d1_mfgoal <- c()
sdr_d1_mfgoal <- c()
l6_form_d1_mfgoalsplitted <- c()
form_d1_mfgoal <- c()
for(index_d1_mfgoal in 1:length(d1_teams))
{
  for(index_d1_mfgoal_cols in 1:d1_totalrounds)
  {
    index_d1_mfgoal  <- row.names(d1_matchfirstgoal_h) == d1_teams[index_d1_mfgoal]
    form_d1_mfgoal <- d1_matchfirstgoal_h[index_d1_mfgoal ]
    deleted_form_d1_mfgoal <- form_d1_mfgoal[!form_d1_mfgoal[] == ""]
    l6_form_d1_mfgoal <- tail(deleted_form_d1_mfgoal,d1_last_n_games)
    l6_form_d1_mfgoal <- as.numeric(l6_form_d1_mfgoal)
    suml6_d1_mfgoal[index_d1_mfgoal] <- sum(l6_form_d1_mfgoal)
    suml6_d1_mfgoal[index_d1_mfgoal] <- paste(suml6_d1_mfgoal[index_d1_mfgoal],sep = "")
    sum_d1_zero_mfgoal[index_d1_mfgoal] <- length(which(l6_form_d1_mfgoal == 0))
    sum_d1_zero_mfgoal[index_d1_mfgoal] <- paste(sum_d1_zero_mfgoal[index_d1_mfgoal],sep = "")
    sum_d1_1to15_mfgoal[index_d1_mfgoal] <- length(which(l6_form_d1_mfgoal >= 1 & l6_form_d1_mfgoal <= 15))
    sum_d1_1to15_mfgoal[index_d1_mfgoal] <- paste(sum_d1_1to15_mfgoal[index_d1_mfgoal],sep = "")
    sum_d1_15to30_mfgoal[index_d1_mfgoal] <- length(which(l6_form_d1_mfgoal >= 15 & l6_form_d1_mfgoal <= 30))
    sum_d1_15to30_mfgoal[index_d1_mfgoal] <- paste(sum_d1_15to30_mfgoal[index_d1_mfgoal],sep = "")
    sum_d1_30to45_mfgoal[index_d1_mfgoal] <- length(which(l6_form_d1_mfgoal >= 30 & l6_form_d1_mfgoal <= 45))
    sum_d1_30to45_mfgoal[index_d1_mfgoal] <- paste(sum_d1_30to45_mfgoal[index_d1_mfgoal],sep = "")
    sum_d1_45to60_mfgoal[index_d1_mfgoal] <- length(which(l6_form_d1_mfgoal >= 45 & l6_form_d1_mfgoal <= 60))
    sum_d1_45to60_mfgoal[index_d1_mfgoal] <- paste(sum_d1_45to60_mfgoal[index_d1_mfgoal],sep = "")
    sum_d1_60to75_mfgoal[index_d1_mfgoal] <- length(which(l6_form_d1_mfgoal >= 60 & l6_form_d1_mfgoal <= 75))
    sum_d1_60to75_mfgoal[index_d1_mfgoal] <- paste(sum_d1_60to75_mfgoal[index_d1_mfgoal],sep = "")
    sum_d1_75to100_mfgoal[index_d1_mfgoal] <- length(which(l6_form_d1_mfgoal >= 75 & l6_form_d1_mfgoal <= 100))
    sum_d1_75to100_mfgoal[index_d1_mfgoal] <- paste(sum_d1_75to100_mfgoal[index_d1_mfgoal],sep = "")
    avgr_d1_mfgoal[index_d1_mfgoal] <- mean(l6_form_d1_mfgoal)
    avgr_d1_mfgoal[index_d1_mfgoal] <- paste(avgr_d1_mfgoal[index_d1_mfgoal],sep = "")
    sdr_d1_mfgoal[index_d1_mfgoal] <- sd(l6_form_d1_mfgoal)
    sdr_d1_mfgoal[index_d1_mfgoal] <- paste(sdr_d1_mfgoal[index_d1_mfgoal],sep = "")
    l6_form_d1_mfgoal <- as.character(l6_form_d1_mfgoal)
    #l6_form_d1_mfgoal_flattened <- stri_paste(l6_form_d1_mfgoal,collapse = '')
    #l6_form_d1_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_d1_mfgoal_flattened),"")[[1]])
    final_d1_mfgoal[index_d1_mfgoal,index_d1_mfgoal_cols] <- l6_form_d1_mfgoal[index_d1_mfgoal_cols]
  }
}

final_d1_mfgoal[is.na(final_d1_mfgoal)] <- ""
d1_matchfirstgoalmatrix <- cbind(d1_teams,final_d1_mfgoal,suml6_d1_mfgoal,sum_d1_zero_mfgoal,sum_d1_1to15_mfgoal,sum_d1_15to30_mfgoal,sum_d1_30to45_mfgoal,sum_d1_45to60_mfgoal,sum_d1_60to75_mfgoal,sum_d1_75to100_mfgoal,avgr_d1_mfgoal,sdr_d1_mfgoal)
write.xlsx(d1_matchfirstgoalmatrix,"Analytics/BIGFIVE/MatchFirstGoalanalysis.xlsx", sheetName = "D1", append = TRUE)
#########################################################################################################################################################################################
#########################################################################################################################################################################################
#F1
F1_matchfirstgoal <- subset(BIGFIVE_analytics, Div == "F1")

f1_matchfirstgoal_h <- tapply(F1_matchfirstgoal$Home_first_GoalTime, F1_matchfirstgoal[c("HomeTeam", "Date")],mean)
f1_matchfirstgoal_a <- tapply(F1_matchfirstgoal$Away_first_GoalTime, F1_matchfirstgoal[c("AwayTeam", "Date")],mean)

f1_matchfirstgoal_h[is.na(f1_matchfirstgoal_h)] <- ""
f1_matchfirstgoal_a[is.na(f1_matchfirstgoal_a)] <- ""

for(f1_rowhgoal in 1:nrow(f1_matchfirstgoal_h)) {
  for(f1_colhgoal in 1:ncol(f1_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(f1_rowagoal in 1:nrow(f1_matchfirstgoal_a)) {
      for(f1_colagoal in 1:ncol(f1_matchfirstgoal_a)) {
        ifelse(!f1_matchfirstgoal_a[f1_rowagoal,f1_colagoal]=="",f1_matchfirstgoal_h[f1_rowagoal,f1_colagoal] <- f1_matchfirstgoal_a[f1_rowagoal,f1_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

f1_home_games <- c()
f1_away_games <-c()

for (i_f1 in 1:length(f1_teams))
{

  f1_home_games[i_f1] <- nrow(F1_matchfirstgoal[F1_matchfirstgoal$HomeTeam == f1_teams[i_f1],])
  f1_away_games[i_f1]  <- nrow(F1_matchfirstgoal[F1_matchfirstgoal$AwayTeam == f1_teams[i_f1],])
}
f1_games_played <- f1_home_games + f1_away_games
f1_last_n_games <- f1_games_played[1]

f1_totalrounds <-  (length(f1_teams) - 1 )*2

final_f1_mfgoal <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_mfgoal <- c()
sum_f1_zero_mfgoal <- c()
sum_f1_1to15_mfgoal <- c()
sum_f1_15to30_mfgoal <- c()
sum_f1_30to45_mfgoal <- c()
sum_f1_45to60_mfgoal <- c()
sum_f1_60to75_mfgoal <- c()
sum_f1_75to100_mfgoal <- c()
avgr_f1_mfgoal <- c()
sdr_f1_mfgoal <- c()
l6_form_f1_mfgoalsplitted <- c()
form_f1_mfgoal <- c()
for(index_f1_mfgoal in 1:length(f1_teams))
{
  for(index_f1_mfgoal_cols in 1:f1_totalrounds)
  {
    index_f1_mfgoal  <- row.names(f1_matchfirstgoal_h) == f1_teams[index_f1_mfgoal]
    form_f1_mfgoal <- f1_matchfirstgoal_h[index_f1_mfgoal ]
    deleted_form_f1_mfgoal <- form_f1_mfgoal[!form_f1_mfgoal[] == ""]
    l6_form_f1_mfgoal <- tail(deleted_form_f1_mfgoal,f1_last_n_games)
    l6_form_f1_mfgoal <- as.numeric(l6_form_f1_mfgoal)
    suml6_f1_mfgoal[index_f1_mfgoal] <- sum(l6_form_f1_mfgoal)
    suml6_f1_mfgoal[index_f1_mfgoal] <- paste(suml6_f1_mfgoal[index_f1_mfgoal],sep = "")
    sum_f1_zero_mfgoal[index_f1_mfgoal] <- length(which(l6_form_f1_mfgoal == 0))
    sum_f1_zero_mfgoal[index_f1_mfgoal] <- paste(sum_f1_zero_mfgoal[index_f1_mfgoal],sep = "")
    sum_f1_1to15_mfgoal[index_f1_mfgoal] <- length(which(l6_form_f1_mfgoal >= 1 & l6_form_f1_mfgoal <= 15))
    sum_f1_1to15_mfgoal[index_f1_mfgoal] <- paste(sum_f1_1to15_mfgoal[index_f1_mfgoal],sep = "")
    sum_f1_15to30_mfgoal[index_f1_mfgoal] <- length(which(l6_form_f1_mfgoal >= 15 & l6_form_f1_mfgoal <= 30))
    sum_f1_15to30_mfgoal[index_f1_mfgoal] <- paste(sum_f1_15to30_mfgoal[index_f1_mfgoal],sep = "")
    sum_f1_30to45_mfgoal[index_f1_mfgoal] <- length(which(l6_form_f1_mfgoal >= 30 & l6_form_f1_mfgoal <= 45))
    sum_f1_30to45_mfgoal[index_f1_mfgoal] <- paste(sum_f1_30to45_mfgoal[index_f1_mfgoal],sep = "")
    sum_f1_45to60_mfgoal[index_f1_mfgoal] <- length(which(l6_form_f1_mfgoal >= 45 & l6_form_f1_mfgoal <= 60))
    sum_f1_45to60_mfgoal[index_f1_mfgoal] <- paste(sum_f1_45to60_mfgoal[index_f1_mfgoal],sep = "")
    sum_f1_60to75_mfgoal[index_f1_mfgoal] <- length(which(l6_form_f1_mfgoal >= 60 & l6_form_f1_mfgoal <= 75))
    sum_f1_60to75_mfgoal[index_f1_mfgoal] <- paste(sum_f1_60to75_mfgoal[index_f1_mfgoal],sep = "")
    sum_f1_75to100_mfgoal[index_f1_mfgoal] <- length(which(l6_form_f1_mfgoal >= 75 & l6_form_f1_mfgoal <= 100))
    sum_f1_75to100_mfgoal[index_f1_mfgoal] <- paste(sum_f1_75to100_mfgoal[index_f1_mfgoal],sep = "")
    avgr_f1_mfgoal[index_f1_mfgoal] <- mean(l6_form_f1_mfgoal)
    avgr_f1_mfgoal[index_f1_mfgoal] <- paste(avgr_f1_mfgoal[index_f1_mfgoal],sep = "")
    sdr_f1_mfgoal[index_f1_mfgoal] <- sd(l6_form_f1_mfgoal)
    sdr_f1_mfgoal[index_f1_mfgoal] <- paste(sdr_f1_mfgoal[index_f1_mfgoal],sep = "")
    l6_form_f1_mfgoal <- as.character(l6_form_f1_mfgoal)
    #l6_form_f1_mfgoal_flattened <- stri_paste(l6_form_f1_mfgoal,collapse = '')
    #l6_form_f1_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_f1_mfgoal_flattened),"")[[1]])
    final_f1_mfgoal[index_f1_mfgoal,index_f1_mfgoal_cols] <- l6_form_f1_mfgoal[index_f1_mfgoal_cols]
  }
}

final_f1_mfgoal[is.na(final_f1_mfgoal)] <- ""
f1_matchfirstgoalmatrix <- cbind(f1_teams,final_f1_mfgoal,suml6_f1_mfgoal,sum_f1_zero_mfgoal,sum_f1_1to15_mfgoal,sum_f1_15to30_mfgoal,sum_f1_30to45_mfgoal,sum_f1_45to60_mfgoal,sum_f1_60to75_mfgoal,sum_f1_75to100_mfgoal,avgr_f1_mfgoal,sdr_f1_mfgoal)
write.xlsx(f1_matchfirstgoalmatrix,"Analytics/BIGFIVE/MatchFirstGoalanalysis.xlsx", sheetName = "F1", append = TRUE)
#########################################################################################################################################################################################
#########################################################################################################################################################################################
#I1
I1_matchfirstgoal <- subset(BIGFIVE_analytics, Div == "I1")

i1_matchfirstgoal_h <- tapply(I1_matchfirstgoal$Home_first_GoalTime, I1_matchfirstgoal[c("HomeTeam", "Date")],mean)
i1_matchfirstgoal_a <- tapply(I1_matchfirstgoal$Away_first_GoalTime, I1_matchfirstgoal[c("AwayTeam", "Date")],mean)

i1_matchfirstgoal_h[is.na(i1_matchfirstgoal_h)] <- ""
i1_matchfirstgoal_a[is.na(i1_matchfirstgoal_a)] <- ""

for(i1_rowhgoal in 1:nrow(i1_matchfirstgoal_h)) {
  for(i1_colhgoal in 1:ncol(i1_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(i1_rowagoal in 1:nrow(i1_matchfirstgoal_a)) {
      for(i1_colagoal in 1:ncol(i1_matchfirstgoal_a)) {
        ifelse(!i1_matchfirstgoal_a[i1_rowagoal,i1_colagoal]=="",i1_matchfirstgoal_h[i1_rowagoal,i1_colagoal] <- i1_matchfirstgoal_a[i1_rowagoal,i1_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

i1_home_games <- c()
i1_away_games <-c()

for (i_i1 in 1:length(i1_teams))
{

  i1_home_games[i_i1] <- nrow(I1_matchfirstgoal[I1_matchfirstgoal$HomeTeam == i1_teams[i_i1],])
  i1_away_games[i_i1]  <- nrow(I1_matchfirstgoal[I1_matchfirstgoal$AwayTeam == i1_teams[i_i1],])
}
i1_games_played <- i1_home_games + i1_away_games
i1_last_n_games <- i1_games_played[1]

i1_totalrounds <-  (length(i1_teams) - 1 )*2

final_i1_mfgoal <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_mfgoal <- c()
sum_i1_zero_mfgoal <- c()
sum_i1_1to15_mfgoal <- c()
sum_i1_15to30_mfgoal <- c()
sum_i1_30to45_mfgoal <- c()
sum_i1_45to60_mfgoal <- c()
sum_i1_60to75_mfgoal <- c()
sum_i1_75to100_mfgoal <- c()
avgr_i1_mfgoal <- c()
sdr_i1_mfgoal <- c()
l6_form_i1_mfgoalsplitted <- c()
form_i1_mfgoal <- c()
for(index_i1_mfgoal in 1:length(i1_teams))
{
  for(index_i1_mfgoal_cols in 1:i1_totalrounds)
  {
    index_i1_mfgoal  <- row.names(i1_matchfirstgoal_h) == i1_teams[index_i1_mfgoal]
    form_i1_mfgoal <- i1_matchfirstgoal_h[index_i1_mfgoal ]
    deleted_form_i1_mfgoal <- form_i1_mfgoal[!form_i1_mfgoal[] == ""]
    l6_form_i1_mfgoal <- tail(deleted_form_i1_mfgoal,i1_last_n_games)
    l6_form_i1_mfgoal <- as.numeric(l6_form_i1_mfgoal)
    suml6_i1_mfgoal[index_i1_mfgoal] <- sum(l6_form_i1_mfgoal)
    suml6_i1_mfgoal[index_i1_mfgoal] <- paste(suml6_i1_mfgoal[index_i1_mfgoal],sep = "")
    sum_i1_zero_mfgoal[index_i1_mfgoal] <- length(which(l6_form_i1_mfgoal == 0))
    sum_i1_zero_mfgoal[index_i1_mfgoal] <- paste(sum_i1_zero_mfgoal[index_i1_mfgoal],sep = "")
    sum_i1_1to15_mfgoal[index_i1_mfgoal] <- length(which(l6_form_i1_mfgoal >= 1 & l6_form_i1_mfgoal <= 15))
    sum_i1_1to15_mfgoal[index_i1_mfgoal] <- paste(sum_i1_1to15_mfgoal[index_i1_mfgoal],sep = "")
    sum_i1_15to30_mfgoal[index_i1_mfgoal] <- length(which(l6_form_i1_mfgoal >= 15 & l6_form_i1_mfgoal <= 30))
    sum_i1_15to30_mfgoal[index_i1_mfgoal] <- paste(sum_i1_15to30_mfgoal[index_i1_mfgoal],sep = "")
    sum_i1_30to45_mfgoal[index_i1_mfgoal] <- length(which(l6_form_i1_mfgoal >= 30 & l6_form_i1_mfgoal <= 45))
    sum_i1_30to45_mfgoal[index_i1_mfgoal] <- paste(sum_i1_30to45_mfgoal[index_i1_mfgoal],sep = "")
    sum_i1_45to60_mfgoal[index_i1_mfgoal] <- length(which(l6_form_i1_mfgoal >= 45 & l6_form_i1_mfgoal <= 60))
    sum_i1_45to60_mfgoal[index_i1_mfgoal] <- paste(sum_i1_45to60_mfgoal[index_i1_mfgoal],sep = "")
    sum_i1_60to75_mfgoal[index_i1_mfgoal] <- length(which(l6_form_i1_mfgoal >= 60 & l6_form_i1_mfgoal <= 75))
    sum_i1_60to75_mfgoal[index_i1_mfgoal] <- paste(sum_i1_60to75_mfgoal[index_i1_mfgoal],sep = "")
    sum_i1_75to100_mfgoal[index_i1_mfgoal] <- length(which(l6_form_i1_mfgoal >= 75 & l6_form_i1_mfgoal <= 100))
    sum_i1_75to100_mfgoal[index_i1_mfgoal] <- paste(sum_i1_75to100_mfgoal[index_i1_mfgoal],sep = "")
    avgr_i1_mfgoal[index_i1_mfgoal] <- mean(l6_form_i1_mfgoal)
    avgr_i1_mfgoal[index_i1_mfgoal] <- paste(avgr_i1_mfgoal[index_i1_mfgoal],sep = "")
    sdr_i1_mfgoal[index_i1_mfgoal] <- sd(l6_form_i1_mfgoal)
    sdr_i1_mfgoal[index_i1_mfgoal] <- paste(sdr_i1_mfgoal[index_i1_mfgoal],sep = "")
    l6_form_i1_mfgoal <- as.character(l6_form_i1_mfgoal)
    #l6_form_i1_mfgoal_flattened <- stri_paste(l6_form_i1_mfgoal,collapse = '')
    #l6_form_i1_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_i1_mfgoal_flattened),"")[[1]])
    final_i1_mfgoal[index_i1_mfgoal,index_i1_mfgoal_cols] <- l6_form_i1_mfgoal[index_i1_mfgoal_cols]
  }
}

final_i1_mfgoal[is.na(final_i1_mfgoal)] <- ""
i1_matchfirstgoalmatrix <- cbind(i1_teams,final_i1_mfgoal,suml6_i1_mfgoal,sum_i1_zero_mfgoal,sum_i1_1to15_mfgoal,sum_i1_15to30_mfgoal,sum_i1_30to45_mfgoal,sum_i1_45to60_mfgoal,sum_i1_60to75_mfgoal,sum_i1_75to100_mfgoal,avgr_i1_mfgoal,sdr_i1_mfgoal)
write.xlsx(i1_matchfirstgoalmatrix,"Analytics/BIGFIVE/MatchFirstGoalanalysis.xlsx", sheetName = "I1", append = TRUE)
######################################################################################################################################################################################
######################################################################################################################################################################################
################
#OTHER LEAGUES##
################
OTHERLEAGUES_analytics <- readxl::read_excel('OTHERLEAGUES20232024.xlsx')
OTHERLEAGUES_analytics <- OTHERLEAGUES_analytics[,-1]
OTHERLEAGUES_analytics <- as.data.frame(OTHERLEAGUES_analytics)

#B1
B1_matchfirstgoal <- subset(OTHERLEAGUES_analytics, Div == "B1")

b1_matchfirstgoal_h <- tapply(B1_matchfirstgoal$Home_first_GoalTime, B1_matchfirstgoal[c("HomeTeam", "Date")],mean)
b1_matchfirstgoal_a <- tapply(B1_matchfirstgoal$Away_first_GoalTime, B1_matchfirstgoal[c("AwayTeam", "Date")],mean)

b1_matchfirstgoal_h[is.na(b1_matchfirstgoal_h)] <- ""
b1_matchfirstgoal_a[is.na(b1_matchfirstgoal_a)] <- ""

for(b1_rowhgoal in 1:nrow(b1_matchfirstgoal_h)) {
  for(b1_colhgoal in 1:ncol(b1_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(b1_rowagoal in 1:nrow(b1_matchfirstgoal_a)) {
      for(b1_colagoal in 1:ncol(b1_matchfirstgoal_a)) {
        ifelse(!b1_matchfirstgoal_a[b1_rowagoal,b1_colagoal]=="",b1_matchfirstgoal_h[b1_rowagoal,b1_colagoal] <- b1_matchfirstgoal_a[b1_rowagoal,b1_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

b1_home_games <- c()
b1_away_games <-c()

for (i_b1 in 1:length(b1_teams))
{

  b1_home_games[i_b1] <- nrow(B1_matchfirstgoal[B1_matchfirstgoal$HomeTeam == b1_teams[i_b1],])
  b1_away_games[i_b1]  <- nrow(B1_matchfirstgoal[B1_matchfirstgoal$AwayTeam == b1_teams[i_b1],])
}
b1_games_played <- b1_home_games + b1_away_games
b1_last_n_games <- b1_games_played[1]

b1_totalrounds <-  (length(b1_teams) - 1 )*2

final_b1_mfgoal <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
suml6_b1_mfgoal <- c()
sum_b1_zero_mfgoal <- c()
sum_b1_1to15_mfgoal <- c()
sum_b1_15to30_mfgoal <- c()
sum_b1_30to45_mfgoal <- c()
sum_b1_45to60_mfgoal <- c()
sum_b1_60to75_mfgoal <- c()
sum_b1_75to100_mfgoal <- c()
avgr_b1_mfgoal <- c()
sdr_b1_mfgoal <- c()
l6_form_b1_mfgoalsplitted <- c()
form_b1_mfgoal <- c()
for(index_b1_mfgoal in 1:length(b1_teams))
{
  for(index_b1_mfgoal_cols in 1:b1_totalrounds)
  {
    index_b1_mfgoal  <- row.names(b1_matchfirstgoal_h) == b1_teams[index_b1_mfgoal]
    form_b1_mfgoal <- b1_matchfirstgoal_h[index_b1_mfgoal ]
    deleted_form_b1_mfgoal <- form_b1_mfgoal[!form_b1_mfgoal[] == ""]
    l6_form_b1_mfgoal <- tail(deleted_form_b1_mfgoal,b1_last_n_games)
    l6_form_b1_mfgoal <- as.numeric(l6_form_b1_mfgoal)
    suml6_b1_mfgoal[index_b1_mfgoal] <- sum(l6_form_b1_mfgoal)
    suml6_b1_mfgoal[index_b1_mfgoal] <- paste(suml6_b1_mfgoal[index_b1_mfgoal],sep = "")
    sum_b1_zero_mfgoal[index_b1_mfgoal] <- length(which(l6_form_b1_mfgoal == 0))
    sum_b1_zero_mfgoal[index_b1_mfgoal] <- paste(sum_b1_zero_mfgoal[index_b1_mfgoal],sep = "")
    sum_b1_1to15_mfgoal[index_b1_mfgoal] <- length(which(l6_form_b1_mfgoal >= 1 & l6_form_b1_mfgoal <= 15))
    sum_b1_1to15_mfgoal[index_b1_mfgoal] <- paste(sum_b1_1to15_mfgoal[index_b1_mfgoal],sep = "")
    sum_b1_15to30_mfgoal[index_b1_mfgoal] <- length(which(l6_form_b1_mfgoal >= 15 & l6_form_b1_mfgoal <= 30))
    sum_b1_15to30_mfgoal[index_b1_mfgoal] <- paste(sum_b1_15to30_mfgoal[index_b1_mfgoal],sep = "")
    sum_b1_30to45_mfgoal[index_b1_mfgoal] <- length(which(l6_form_b1_mfgoal >= 30 & l6_form_b1_mfgoal <= 45))
    sum_b1_30to45_mfgoal[index_b1_mfgoal] <- paste(sum_b1_30to45_mfgoal[index_b1_mfgoal],sep = "")
    sum_b1_45to60_mfgoal[index_b1_mfgoal] <- length(which(l6_form_b1_mfgoal >= 45 & l6_form_b1_mfgoal <= 60))
    sum_b1_45to60_mfgoal[index_b1_mfgoal] <- paste(sum_b1_45to60_mfgoal[index_b1_mfgoal],sep = "")
    sum_b1_60to75_mfgoal[index_b1_mfgoal] <- length(which(l6_form_b1_mfgoal >= 60 & l6_form_b1_mfgoal <= 75))
    sum_b1_60to75_mfgoal[index_b1_mfgoal] <- paste(sum_b1_60to75_mfgoal[index_b1_mfgoal],sep = "")
    sum_b1_75to100_mfgoal[index_b1_mfgoal] <- length(which(l6_form_b1_mfgoal >= 75 & l6_form_b1_mfgoal <= 100))
    sum_b1_75to100_mfgoal[index_b1_mfgoal] <- paste(sum_b1_75to100_mfgoal[index_b1_mfgoal],sep = "")
    avgr_b1_mfgoal[index_b1_mfgoal] <- mean(l6_form_b1_mfgoal)
    avgr_b1_mfgoal[index_b1_mfgoal] <- paste(avgr_b1_mfgoal[index_b1_mfgoal],sep = "")
    sdr_b1_mfgoal[index_b1_mfgoal] <- sd(l6_form_b1_mfgoal)
    sdr_b1_mfgoal[index_b1_mfgoal] <- paste(sdr_b1_mfgoal[index_b1_mfgoal],sep = "")
    l6_form_b1_mfgoal <- as.character(l6_form_b1_mfgoal)
    #l6_form_b1_mfgoal_flattened <- stri_paste(l6_form_b1_mfgoal,collapse = '')
    #l6_form_b1_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_b1_mfgoal_flattened),"")[[1]])
    final_b1_mfgoal[index_b1_mfgoal,index_b1_mfgoal_cols] <- l6_form_b1_mfgoal[index_b1_mfgoal_cols]
  }
}

final_b1_mfgoal[is.na(final_b1_mfgoal)] <- ""
b1_matchfirstgoalmatrix <- cbind(b1_teams,final_b1_mfgoal,suml6_b1_mfgoal,sum_b1_zero_mfgoal,sum_b1_1to15_mfgoal,sum_b1_15to30_mfgoal,sum_b1_30to45_mfgoal,sum_b1_45to60_mfgoal,sum_b1_60to75_mfgoal,sum_b1_75to100_mfgoal,avgr_b1_mfgoal,sdr_b1_mfgoal)
unlink('Analytics/OTHERLEAGUES/MatchFirstGoalanalysis.xlsx')
write.xlsx(b1_matchfirstgoalmatrix,"Analytics/OTHERLEAGUES/MatchFirstGoalanalysis.xlsx", sheetName = "B1", append = TRUE)
#######################################################################################################################################################################
#######################################################################################################################################################################
#D2
D2_matchfirstgoal <- subset(OTHERLEAGUES_analytics, Div == "D2")

d2_matchfirstgoal_h <- tapply(D2_matchfirstgoal$Home_first_GoalTime, D2_matchfirstgoal[c("HomeTeam", "Date")],mean)
d2_matchfirstgoal_a <- tapply(D2_matchfirstgoal$Away_first_GoalTime, D2_matchfirstgoal[c("AwayTeam", "Date")],mean)

d2_matchfirstgoal_h[is.na(d2_matchfirstgoal_h)] <- ""
d2_matchfirstgoal_a[is.na(d2_matchfirstgoal_a)] <- ""

for(d2_rowhgoal in 1:nrow(d2_matchfirstgoal_h)) {
  for(d2_colhgoal in 1:ncol(d2_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(d2_rowagoal in 1:nrow(d2_matchfirstgoal_a)) {
      for(d2_colagoal in 1:ncol(d2_matchfirstgoal_a)) {
        ifelse(!d2_matchfirstgoal_a[d2_rowagoal,d2_colagoal]=="",d2_matchfirstgoal_h[d2_rowagoal,d2_colagoal] <- d2_matchfirstgoal_a[d2_rowagoal,d2_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

d2_home_games <- c()
d2_away_games <-c()

for (i_d2 in 1:length(d2_teams))
{

  d2_home_games[i_d2] <- nrow(D2_matchfirstgoal[D2_matchfirstgoal$HomeTeam == d2_teams[i_d2],])
  d2_away_games[i_d2]  <- nrow(D2_matchfirstgoal[D2_matchfirstgoal$AwayTeam == d2_teams[i_d2],])
}
d2_games_played <- d2_home_games + d2_away_games
d2_last_n_games <- d2_games_played[1]

d2_totalrounds <-  (length(d2_teams) - 1 )*2

final_d2_mfgoal <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
suml6_d2_mfgoal <- c()
sum_d2_zero_mfgoal <- c()
sum_d2_1to15_mfgoal <- c()
sum_d2_15to30_mfgoal <- c()
sum_d2_30to45_mfgoal <- c()
sum_d2_45to60_mfgoal <- c()
sum_d2_60to75_mfgoal <- c()
sum_d2_75to100_mfgoal <- c()
avgr_d2_mfgoal <- c()
sdr_d2_mfgoal <- c()
l6_form_d2_mfgoalsplitted <- c()
form_d2_mfgoal <- c()
for(index_d2_mfgoal in 1:length(d2_teams))
{
  for(index_d2_mfgoal_cols in 1:d2_totalrounds)
  {
    index_d2_mfgoal  <- row.names(d2_matchfirstgoal_h) == d2_teams[index_d2_mfgoal]
    form_d2_mfgoal <- d2_matchfirstgoal_h[index_d2_mfgoal ]
    deleted_form_d2_mfgoal <- form_d2_mfgoal[!form_d2_mfgoal[] == ""]
    l6_form_d2_mfgoal <- tail(deleted_form_d2_mfgoal,d2_last_n_games)
    l6_form_d2_mfgoal <- as.numeric(l6_form_d2_mfgoal)
    suml6_d2_mfgoal[index_d2_mfgoal] <- sum(l6_form_d2_mfgoal)
    suml6_d2_mfgoal[index_d2_mfgoal] <- paste(suml6_d2_mfgoal[index_d2_mfgoal],sep = "")
    sum_d2_zero_mfgoal[index_d2_mfgoal] <- length(which(l6_form_d2_mfgoal == 0))
    sum_d2_zero_mfgoal[index_d2_mfgoal] <- paste(sum_d2_zero_mfgoal[index_d2_mfgoal],sep = "")
    sum_d2_1to15_mfgoal[index_d2_mfgoal] <- length(which(l6_form_d2_mfgoal >= 1 & l6_form_d2_mfgoal <= 15))
    sum_d2_1to15_mfgoal[index_d2_mfgoal] <- paste(sum_d2_1to15_mfgoal[index_d2_mfgoal],sep = "")
    sum_d2_15to30_mfgoal[index_d2_mfgoal] <- length(which(l6_form_d2_mfgoal >= 15 & l6_form_d2_mfgoal <= 30))
    sum_d2_15to30_mfgoal[index_d2_mfgoal] <- paste(sum_d2_15to30_mfgoal[index_d2_mfgoal],sep = "")
    sum_d2_30to45_mfgoal[index_d2_mfgoal] <- length(which(l6_form_d2_mfgoal >= 30 & l6_form_d2_mfgoal <= 45))
    sum_d2_30to45_mfgoal[index_d2_mfgoal] <- paste(sum_d2_30to45_mfgoal[index_d2_mfgoal],sep = "")
    sum_d2_45to60_mfgoal[index_d2_mfgoal] <- length(which(l6_form_d2_mfgoal >= 45 & l6_form_d2_mfgoal <= 60))
    sum_d2_45to60_mfgoal[index_d2_mfgoal] <- paste(sum_d2_45to60_mfgoal[index_d2_mfgoal],sep = "")
    sum_d2_60to75_mfgoal[index_d2_mfgoal] <- length(which(l6_form_d2_mfgoal >= 60 & l6_form_d2_mfgoal <= 75))
    sum_d2_60to75_mfgoal[index_d2_mfgoal] <- paste(sum_d2_60to75_mfgoal[index_d2_mfgoal],sep = "")
    sum_d2_75to100_mfgoal[index_d2_mfgoal] <- length(which(l6_form_d2_mfgoal >= 75 & l6_form_d2_mfgoal <= 100))
    sum_d2_75to100_mfgoal[index_d2_mfgoal] <- paste(sum_d2_75to100_mfgoal[index_d2_mfgoal],sep = "")
    avgr_d2_mfgoal[index_d2_mfgoal] <- mean(l6_form_d2_mfgoal)
    avgr_d2_mfgoal[index_d2_mfgoal] <- paste(avgr_d2_mfgoal[index_d2_mfgoal],sep = "")
    sdr_d2_mfgoal[index_d2_mfgoal] <- sd(l6_form_d2_mfgoal)
    sdr_d2_mfgoal[index_d2_mfgoal] <- paste(sdr_d2_mfgoal[index_d2_mfgoal],sep = "")
    l6_form_d2_mfgoal <- as.character(l6_form_d2_mfgoal)
    #l6_form_d2_mfgoal_flattened <- stri_paste(l6_form_d2_mfgoal,collapse = '')
    #l6_form_d2_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_d2_mfgoal_flattened),"")[[1]])
    final_d2_mfgoal[index_d2_mfgoal,index_d2_mfgoal_cols] <- l6_form_d2_mfgoal[index_d2_mfgoal_cols]
  }
}

final_d2_mfgoal[is.na(final_d2_mfgoal)] <- ""
d2_matchfirstgoalmatrix <- cbind(d2_teams,final_d2_mfgoal,suml6_d2_mfgoal,sum_d2_zero_mfgoal,sum_d2_1to15_mfgoal,sum_d2_15to30_mfgoal,sum_d2_30to45_mfgoal,sum_d2_45to60_mfgoal,sum_d2_60to75_mfgoal,sum_d2_75to100_mfgoal,avgr_d2_mfgoal,sdr_d2_mfgoal)
write.xlsx(d2_matchfirstgoalmatrix,"Analytics/OTHERLEAGUES/MatchFirstGoalanalysis.xlsx", sheetName = "D2", append = TRUE)
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#E1
E1_matchfirstgoal <- subset(OTHERLEAGUES_analytics, Div == "E1")

e1_matchfirstgoal_h <- tapply(E1_matchfirstgoal$Home_first_GoalTime, E1_matchfirstgoal[c("HomeTeam", "Date")],mean)
e1_matchfirstgoal_a <- tapply(E1_matchfirstgoal$Away_first_GoalTime, E1_matchfirstgoal[c("AwayTeam", "Date")],mean)

e1_matchfirstgoal_h[is.na(e1_matchfirstgoal_h)] <- ""
e1_matchfirstgoal_a[is.na(e1_matchfirstgoal_a)] <- ""

for(e1_rowhgoal in 1:nrow(e1_matchfirstgoal_h)) {
  for(e1_colhgoal in 1:ncol(e1_matchfirstgoal_h)) {

    # print(my_matrix[row, col])
    for(e1_rowagoal in 1:nrow(e1_matchfirstgoal_a)) {
      for(e1_colagoal in 1:ncol(e1_matchfirstgoal_a)) {
        ifelse(!e1_matchfirstgoal_a[e1_rowagoal,e1_colagoal]=="",e1_matchfirstgoal_h[e1_rowagoal,e1_colagoal] <- e1_matchfirstgoal_a[e1_rowagoal,e1_colagoal],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

e1_home_games <- c()
e1_away_games <-c()

for (i_e1 in 1:length(e1_teams))
{

  e1_home_games[i_e1] <- nrow(E1_matchfirstgoal[E1_matchfirstgoal$HomeTeam == e1_teams[i_e1],])
  e1_away_games[i_e1]  <- nrow(E1_matchfirstgoal[E1_matchfirstgoal$AwayTeam == e1_teams[i_e1],])
}
e1_games_played <- e1_home_games + e1_away_games
e1_last_n_games <- e1_games_played[1]

e1_totalrounds <-  (length(e1_teams) - 1 )*2

final_e1_mfgoal <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
suml6_e1_mfgoal <- c()
sum_e1_zero_mfgoal <- c()
sum_e1_1to15_mfgoal <- c()
sum_e1_15to30_mfgoal <- c()
sum_e1_30to45_mfgoal <- c()
sum_e1_45to60_mfgoal <- c()
sum_e1_60to75_mfgoal <- c()
sum_e1_75to100_mfgoal <- c()
avgr_e1_mfgoal <- c()
sdr_e1_mfgoal <- c()
l6_form_e1_mfgoalsplitted <- c()
form_e1_mfgoal <- c()
for(index_e1_mfgoal in 1:length(e1_teams))
{
  for(index_e1_mfgoal_cols in 1:e1_totalrounds)
  {
    index_e1_mfgoal  <- row.names(e1_matchfirstgoal_h) == e1_teams[index_e1_mfgoal]
    form_e1_mfgoal <- e1_matchfirstgoal_h[index_e1_mfgoal ]
    deleted_form_e1_mfgoal <- form_e1_mfgoal[!form_e1_mfgoal[] == ""]
    l6_form_e1_mfgoal <- tail(deleted_form_e1_mfgoal,e1_last_n_games)
    l6_form_e1_mfgoal <- as.numeric(l6_form_e1_mfgoal)
    suml6_e1_mfgoal[index_e1_mfgoal] <- sum(l6_form_e1_mfgoal)
    suml6_e1_mfgoal[index_e1_mfgoal] <- paste(suml6_e1_mfgoal[index_e1_mfgoal],sep = "")
    sum_e1_zero_mfgoal[index_e1_mfgoal] <- length(which(l6_form_e1_mfgoal == 0))
    sum_e1_zero_mfgoal[index_e1_mfgoal] <- paste(sum_e1_zero_mfgoal[index_e1_mfgoal],sep = "")
    sum_e1_1to15_mfgoal[index_e1_mfgoal] <- length(which(l6_form_e1_mfgoal >= 1 & l6_form_e1_mfgoal <= 15))
    sum_e1_1to15_mfgoal[index_e1_mfgoal] <- paste(sum_e1_1to15_mfgoal[index_e1_mfgoal],sep = "")
    sum_e1_15to30_mfgoal[index_e1_mfgoal] <- length(which(l6_form_e1_mfgoal >= 15 & l6_form_e1_mfgoal <= 30))
    sum_e1_15to30_mfgoal[index_e1_mfgoal] <- paste(sum_e1_15to30_mfgoal[index_e1_mfgoal],sep = "")
    sum_e1_30to45_mfgoal[index_e1_mfgoal] <- length(which(l6_form_e1_mfgoal >= 30 & l6_form_e1_mfgoal <= 45))
    sum_e1_30to45_mfgoal[index_e1_mfgoal] <- paste(sum_e1_30to45_mfgoal[index_e1_mfgoal],sep = "")
    sum_e1_45to60_mfgoal[index_e1_mfgoal] <- length(which(l6_form_e1_mfgoal >= 45 & l6_form_e1_mfgoal <= 60))
    sum_e1_45to60_mfgoal[index_e1_mfgoal] <- paste(sum_e1_45to60_mfgoal[index_e1_mfgoal],sep = "")
    sum_e1_60to75_mfgoal[index_e1_mfgoal] <- length(which(l6_form_e1_mfgoal >= 60 & l6_form_e1_mfgoal <= 75))
    sum_e1_60to75_mfgoal[index_e1_mfgoal] <- paste(sum_e1_60to75_mfgoal[index_e1_mfgoal],sep = "")
    sum_e1_75to100_mfgoal[index_e1_mfgoal] <- length(which(l6_form_e1_mfgoal >= 75 & l6_form_e1_mfgoal <= 100))
    sum_e1_75to100_mfgoal[index_e1_mfgoal] <- paste(sum_e1_75to100_mfgoal[index_e1_mfgoal],sep = "")
    avgr_e1_mfgoal[index_e1_mfgoal] <- mean(l6_form_e1_mfgoal)
    avgr_e1_mfgoal[index_e1_mfgoal] <- paste(avgr_e1_mfgoal[index_e1_mfgoal],sep = "")
    sdr_e1_mfgoal[index_e1_mfgoal] <- sd(l6_form_e1_mfgoal)
    sdr_e1_mfgoal[index_e1_mfgoal] <- paste(sdr_e1_mfgoal[index_e1_mfgoal],sep = "")
    l6_form_e1_mfgoal <- as.character(l6_form_e1_mfgoal)
    #l6_form_e1_mfgoal_flattened <- stri_paste(l6_form_e1_mfgoal,collapse = '')
    #l6_form_e1_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_e1_mfgoal_flattened),"")[[1]])
    final_e1_mfgoal[index_e1_mfgoal,index_e1_mfgoal_cols] <- l6_form_e1_mfgoal[index_e1_mfgoal_cols]
  }
}

final_e1_mfgoal[is.na(final_e1_mfgoal)] <- ""
e1_matchfirstgoalmatrix <- cbind(e1_teams,final_e1_mfgoal,suml6_e1_mfgoal,sum_e1_zero_mfgoal,sum_e1_1to15_mfgoal,sum_e1_15to30_mfgoal,sum_e1_30to45_mfgoal,sum_e1_45to60_mfgoal,sum_e1_60to75_mfgoal,sum_e1_75to100_mfgoal,avgr_e1_mfgoal,sdr_e1_mfgoal)
write.xlsx(e1_matchfirstgoalmatrix,"Analytics/OTHERLEAGUES/MatchFirstGoalanalysis.xlsx", sheetName = "E1", append = TRUE)
################################################################################################################################################################################
################################################################################################################################################################################
# #SC0
# SC0_matchfirstgoal <- readxl::read_excel('../Rsoccer/SC0_SPREAD.xlsx')
# SC0_matchfirstgoal <- SC0_matchfirstgoal[,c(-1)]
#
# sc0_matchfirstgoal_h <- tapply(SC0_matchfirstgoal$Home_first_GoalTime, SC0_matchfirstgoal[c("HomeTeam", "Date")],mean)
# sc0_matchfirstgoal_a <- tapply(SC0_matchfirstgoal$Away_first_GoalTime, SC0_matchfirstgoal[c("AwayTeam", "Date")],mean)
#
# sc0_matchfirstgoal_h[is.na(sc0_matchfirstgoal_h)] <- ""
# sc0_matchfirstgoal_a[is.na(sc0_matchfirstgoal_a)] <- ""
#
# for(sc0_rowhgoal in 1:nrow(sc0_matchfirstgoal_h)) {
#   for(sc0_colhgoal in 1:ncol(sc0_matchfirstgoal_h)) {
#
#     # print(my_matrix[row, col])
#     for(sc0_rowagoal in 1:nrow(sc0_matchfirstgoal_a)) {
#       for(sc0_colagoal in 1:ncol(sc0_matchfirstgoal_a)) {
#         ifelse(!sc0_matchfirstgoal_a[sc0_rowagoal,sc0_colagoal]=="",sc0_matchfirstgoal_h[sc0_rowagoal,sc0_colagoal] <- sc0_matchfirstgoal_a[sc0_rowagoal,sc0_colagoal],next)
#         #print(my_matrix[row, col])
#       }
#     }
#
#   }
# }
#
# sc0_home_games <- c()
# sc0_away_games <-c()
#
# for (i_sc0 in 1:length(sc0_teams))
# {
#
#   sc0_home_games[i_sc0] <- nrow(SC0_matchfirstgoal[SC0_matchfirstgoal$HomeTeam == sc0_teams[i_sc0],])
#   sc0_away_games[i_sc0]  <- nrow(SC0_matchfirstgoal[SC0_matchfirstgoal$AwayTeam == sc0_teams[i_sc0],])
# }
# sc0_games_played <- sc0_home_games + sc0_away_games
# sc0_last_n_games <- sc0_games_played[1]
#
# sc0_totalrounds <-  (length(sc0_teams) - 1 )*2
#
# final_sc0_mfgoal <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
# suml6_sc0_mfgoal <- c()
# sum_sc0_zero_mfgoal <- c()
# sum_sc0_1to15_mfgoal <- c()
# sum_sc0_15to30_mfgoal <- c()
# sum_sc0_30to45_mfgoal <- c()
# sum_sc0_45to60_mfgoal <- c()
# sum_sc0_60to75_mfgoal <- c()
# sum_sc0_75to100_mfgoal <- c()
# avgr_sc0_mfgoal <- c()
# sdr_sc0_mfgoal <- c()
# l6_form_sc0_mfgoalsplitted <- c()
# form_sc0_mfgoal <- c()
# for(index_sc0_mfgoal in 1:length(sc0_teams))
# {
#   for(index_sc0_mfgoal_cols in 1:sc0_totalrounds)
#   {
#     index_sc0_mfgoal  <- row.names(sc0_matchfirstgoal_h) == sc0_teams[index_sc0_mfgoal]
#     form_sc0_mfgoal <- sc0_matchfirstgoal_h[index_sc0_mfgoal ]
#     deleted_form_sc0_mfgoal <- form_sc0_mfgoal[!form_sc0_mfgoal[] == ""]
#     l6_form_sc0_mfgoal <- tail(deleted_form_sc0_mfgoal,sc0_last_n_games)
#     l6_form_sc0_mfgoal <- as.numeric(l6_form_sc0_mfgoal)
#     suml6_sc0_mfgoal[index_sc0_mfgoal] <- sum(l6_form_sc0_mfgoal)
#     suml6_sc0_mfgoal[index_sc0_mfgoal] <- paste(suml6_sc0_mfgoal[index_sc0_mfgoal],sep = "")
#     sum_sc0_zero_mfgoal[index_sc0_mfgoal] <- length(which(l6_form_sc0_mfgoal == 0))
#     sum_sc0_zero_mfgoal[index_sc0_mfgoal] <- paste(sum_sc0_zero_mfgoal[index_sc0_mfgoal],sep = "")
#     sum_sc0_1to15_mfgoal[index_sc0_mfgoal] <- length(which(l6_form_sc0_mfgoal >= 1 & l6_form_sc0_mfgoal <= 15))
#     sum_sc0_1to15_mfgoal[index_sc0_mfgoal] <- paste(sum_sc0_1to15_mfgoal[index_sc0_mfgoal],sep = "")
#     sum_sc0_15to30_mfgoal[index_sc0_mfgoal] <- length(which(l6_form_sc0_mfgoal >= 15 & l6_form_sc0_mfgoal <= 30))
#     sum_sc0_15to30_mfgoal[index_sc0_mfgoal] <- paste(sum_sc0_15to30_mfgoal[index_sc0_mfgoal],sep = "")
#     sum_sc0_30to45_mfgoal[index_sc0_mfgoal] <- length(which(l6_form_sc0_mfgoal >= 30 & l6_form_sc0_mfgoal <= 45))
#     sum_sc0_30to45_mfgoal[index_sc0_mfgoal] <- paste(sum_sc0_30to45_mfgoal[index_sc0_mfgoal],sep = "")
#     sum_sc0_45to60_mfgoal[index_sc0_mfgoal] <- length(which(l6_form_sc0_mfgoal >= 45 & l6_form_sc0_mfgoal <= 60))
#     sum_sc0_45to60_mfgoal[index_sc0_mfgoal] <- paste(sum_sc0_45to60_mfgoal[index_sc0_mfgoal],sep = "")
#     sum_sc0_60to75_mfgoal[index_sc0_mfgoal] <- length(which(l6_form_sc0_mfgoal >= 60 & l6_form_sc0_mfgoal <= 75))
#     sum_sc0_60to75_mfgoal[index_sc0_mfgoal] <- paste(sum_sc0_60to75_mfgoal[index_sc0_mfgoal],sep = "")
#     sum_sc0_75to100_mfgoal[index_sc0_mfgoal] <- length(which(l6_form_sc0_mfgoal >= 75 & l6_form_sc0_mfgoal <= 100))
#     sum_sc0_75to100_mfgoal[index_sc0_mfgoal] <- paste(sum_sc0_75to100_mfgoal[index_sc0_mfgoal],sep = "")
#     avgr_sc0_mfgoal[index_sc0_mfgoal] <- mean(l6_form_sc0_mfgoal)
#     avgr_sc0_mfgoal[index_sc0_mfgoal] <- paste(avgr_sc0_mfgoal[index_sc0_mfgoal],sep = "")
#     sdr_sc0_mfgoal[index_sc0_mfgoal] <- sd(l6_form_sc0_mfgoal)
#     sdr_sc0_mfgoal[index_sc0_mfgoal] <- paste(sdr_sc0_mfgoal[index_sc0_mfgoal],sep = "")
#     l6_form_sc0_mfgoal <- as.character(l6_form_sc0_mfgoal)
#     #l6_form_sc0_mfgoal_flattened <- stri_paste(l6_form_sc0_mfgoal,collapse = '')
#     #l6_form_sc0_mfgoalsplitted <- as.numeric(strsplit(as.character(l6_form_sc0_mfgoal_flattened),"")[[1]])
#     final_sc0_mfgoal[index_sc0_mfgoal,index_sc0_mfgoal_cols] <- l6_form_sc0_mfgoal[index_sc0_mfgoal_cols]
#   }
# }
#
# final_sc0_mfgoal[is.na(final_sc0_mfgoal)] <- ""
# sc0_matchfirstgoalmatrix <- cbind(sc0_teams,final_sc0_mfgoal,suml6_sc0_mfgoal,sum_sc0_zero_mfgoal,sum_sc0_1to15_mfgoal,sum_sc0_15to30_mfgoal,sum_sc0_30to45_mfgoal,sum_sc0_45to60_mfgoal,sum_sc0_60to75_mfgoal,sum_sc0_75to100_mfgoal,avgr_sc0_mfgoal,sdr_sc0_mfgoal)
# write.xlsx(sc0_matchfirstgoalmatrix,"Analytics/OTHERLEAGUES/MatchFirstGoalanalysis.xlsx", sheetName = "SC0", append = TRUE)
#
# f
