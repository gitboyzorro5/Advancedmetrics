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
E0_matchfirstyc <- subset(BIGFIVE_analytics, Div == "E0")

e0_matchfirstyc_h <- tapply(E0_matchfirstyc$Home_first_YCTime, E0_matchfirstyc[c("HomeTeam", "Date")],mean)
e0_matchfirstyc_a <- tapply(E0_matchfirstyc$Away_first_YCTime, E0_matchfirstyc[c("AwayTeam", "Date")],mean)

e0_matchfirstyc_h[is.na(e0_matchfirstyc_h)] <- ""
e0_matchfirstyc_a[is.na(e0_matchfirstyc_a)] <- ""

for(e0_rowhyc in 1:nrow(e0_matchfirstyc_h)) {
  for(e0_colhyc in 1:ncol(e0_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(e0_rowayc in 1:nrow(e0_matchfirstyc_a)) {
      for(e0_colayc in 1:ncol(e0_matchfirstyc_a)) {
        ifelse(!e0_matchfirstyc_a[e0_rowayc,e0_colayc]=="",e0_matchfirstyc_h[e0_rowayc,e0_colayc] <- e0_matchfirstyc_a[e0_rowayc,e0_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

e0_home_games <- c()
e0_away_games <-c()

for (i_e0 in 1:length(e0_teams))
{

  e0_home_games[i_e0] <- nrow(E0_matchfirstyc[E0_matchfirstyc$HomeTeam == e0_teams[i_e0],])
  e0_away_games[i_e0]  <- nrow(E0_matchfirstyc[E0_matchfirstyc$AwayTeam == e0_teams[i_e0],])
}
e0_games_played <- e0_home_games + e0_away_games
e0_last_n_games <- e0_games_played[1]

e0_totalrounds <-  (length(e0_teams) - 1 )*2

final_e0_mfyc <- matrix(nrow = length(e0_teams),ncol = e0_totalrounds )
suml6_e0_mfyc <- c()
sum_e0_zero_mfyc <- c()
sum_e0_1to15_mfyc <- c()
sum_e0_15to30_mfyc <- c()
sum_e0_30to45_mfyc <- c()
sum_e0_45to60_mfyc <- c()
sum_e0_60to75_mfyc <- c()
sum_e0_75to100_mfyc <- c()
avgr_e0_mfyc <- c()
sdr_e0_mfyc <- c()
l6_form_e0_mfycsplitted <- c()
form_e0_mfyc <- c()
for(index_e0_mfyc in 1:length(e0_teams))
{
  for(index_e0_mfyc_cols in 1:e0_totalrounds)
  {
    index_e0_mfyc  <- row.names(e0_matchfirstyc_h) == e0_teams[index_e0_mfyc]
    form_e0_mfyc <- e0_matchfirstyc_h[index_e0_mfyc ]
    deleted_form_e0_mfyc <- form_e0_mfyc[!form_e0_mfyc[] == ""]
    l6_form_e0_mfyc <- tail(deleted_form_e0_mfyc,e0_last_n_games)
    l6_form_e0_mfyc <- as.numeric(l6_form_e0_mfyc)
    suml6_e0_mfyc[index_e0_mfyc] <- sum(l6_form_e0_mfyc)
    suml6_e0_mfyc[index_e0_mfyc] <- paste(suml6_e0_mfyc[index_e0_mfyc],sep = "")
    sum_e0_zero_mfyc[index_e0_mfyc] <- length(which(l6_form_e0_mfyc == 0))
    sum_e0_zero_mfyc[index_e0_mfyc] <- paste(sum_e0_zero_mfyc[index_e0_mfyc],sep = "")
    sum_e0_1to15_mfyc[index_e0_mfyc] <- length(which(l6_form_e0_mfyc >= 1 & l6_form_e0_mfyc <= 15))
    sum_e0_1to15_mfyc[index_e0_mfyc] <- paste(sum_e0_1to15_mfyc[index_e0_mfyc],sep = "")
    sum_e0_15to30_mfyc[index_e0_mfyc] <- length(which(l6_form_e0_mfyc >= 15 & l6_form_e0_mfyc <= 30))
    sum_e0_15to30_mfyc[index_e0_mfyc] <- paste(sum_e0_15to30_mfyc[index_e0_mfyc],sep = "")
    sum_e0_30to45_mfyc[index_e0_mfyc] <- length(which(l6_form_e0_mfyc >= 30 & l6_form_e0_mfyc <= 45))
    sum_e0_30to45_mfyc[index_e0_mfyc] <- paste(sum_e0_30to45_mfyc[index_e0_mfyc],sep = "")
    sum_e0_45to60_mfyc[index_e0_mfyc] <- length(which(l6_form_e0_mfyc >= 45 & l6_form_e0_mfyc <= 60))
    sum_e0_45to60_mfyc[index_e0_mfyc] <- paste(sum_e0_45to60_mfyc[index_e0_mfyc],sep = "")
    sum_e0_60to75_mfyc[index_e0_mfyc] <- length(which(l6_form_e0_mfyc >= 60 & l6_form_e0_mfyc <= 75))
    sum_e0_60to75_mfyc[index_e0_mfyc] <- paste(sum_e0_60to75_mfyc[index_e0_mfyc],sep = "")
    sum_e0_75to100_mfyc[index_e0_mfyc] <- length(which(l6_form_e0_mfyc >= 75 & l6_form_e0_mfyc <= 100))
    sum_e0_75to100_mfyc[index_e0_mfyc] <- paste(sum_e0_75to100_mfyc[index_e0_mfyc],sep = "")
    avgr_e0_mfyc[index_e0_mfyc] <- mean(l6_form_e0_mfyc)
    avgr_e0_mfyc[index_e0_mfyc] <- paste(avgr_e0_mfyc[index_e0_mfyc],sep = "")
    sdr_e0_mfyc[index_e0_mfyc] <- sd(l6_form_e0_mfyc)
    sdr_e0_mfyc[index_e0_mfyc] <- paste(sdr_e0_mfyc[index_e0_mfyc],sep = "")
    l6_form_e0_mfyc <- as.character(l6_form_e0_mfyc)
    #l6_form_e0_mfyc_flattened <- stri_paste(l6_form_e0_mfyc,collapse = '')
    #l6_form_e0_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_e0_mfyc_flattened),"")[[1]])
    final_e0_mfyc[index_e0_mfyc,index_e0_mfyc_cols] <- l6_form_e0_mfyc[index_e0_mfyc_cols]
  }
}

final_e0_mfyc[is.na(final_e0_mfyc)] <- ""
e0_matchfirstycmatrix <- cbind(e0_teams,final_e0_mfyc,suml6_e0_mfyc,sum_e0_zero_mfyc,sum_e0_1to15_mfyc,sum_e0_15to30_mfyc,sum_e0_30to45_mfyc,sum_e0_45to60_mfyc,sum_e0_60to75_mfyc,sum_e0_75to100_mfyc,avgr_e0_mfyc,sdr_e0_mfyc)

unlink('Analytics/BIGFIVE/MatchFirstYCanalysis.xlsx')
write.xlsx(e0_matchfirstycmatrix,"Analytics/BIGFIVE/MatchFirstYCanalysis.xlsx", sheetName = "E0")
####################################################################################################################################################################################
####################################################################################################################################################################################
#SP1
SP1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "SP1")

sp1_matchfirstyc_h <- tapply(SP1_matchfirstyc$Home_first_YCTime, SP1_matchfirstyc[c("HomeTeam", "Date")],mean)
sp1_matchfirstyc_a <- tapply(SP1_matchfirstyc$Away_first_YCTime, SP1_matchfirstyc[c("AwayTeam", "Date")],mean)

sp1_matchfirstyc_h[is.na(sp1_matchfirstyc_h)] <- ""
sp1_matchfirstyc_a[is.na(sp1_matchfirstyc_a)] <- ""

for(sp1_rowhyc in 1:nrow(sp1_matchfirstyc_h)) {
  for(sp1_colhyc in 1:ncol(sp1_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(sp1_rowayc in 1:nrow(sp1_matchfirstyc_a)) {
      for(sp1_colayc in 1:ncol(sp1_matchfirstyc_a)) {
        ifelse(!sp1_matchfirstyc_a[sp1_rowayc,sp1_colayc]=="",sp1_matchfirstyc_h[sp1_rowayc,sp1_colayc] <- sp1_matchfirstyc_a[sp1_rowayc,sp1_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

sp1_home_games <- c()
sp1_away_games <-c()

for (i_sp1 in 1:length(sp1_teams))
{

  sp1_home_games[i_sp1] <- nrow(SP1_matchfirstyc[SP1_matchfirstyc$HomeTeam == sp1_teams[i_sp1],])
  sp1_away_games[i_sp1]  <- nrow(SP1_matchfirstyc[SP1_matchfirstyc$AwayTeam == sp1_teams[i_sp1],])
}
sp1_games_played <- sp1_home_games + sp1_away_games
sp1_last_n_games <- sp1_games_played[1]

sp1_totalrounds <-  (length(sp1_teams) - 1 )*2

final_sp1_mfyc <- matrix(nrow = length(sp1_teams),ncol = sp1_totalrounds )
suml6_sp1_mfyc <- c()
sum_sp1_zero_mfyc <- c()
sum_sp1_1to15_mfyc <- c()
sum_sp1_15to30_mfyc <- c()
sum_sp1_30to45_mfyc <- c()
sum_sp1_45to60_mfyc <- c()
sum_sp1_60to75_mfyc <- c()
sum_sp1_75to100_mfyc <- c()
avgr_sp1_mfyc <- c()
sdr_sp1_mfyc <- c()
l6_form_sp1_mfycsplitted <- c()
form_sp1_mfyc <- c()
for(index_sp1_mfyc in 1:length(sp1_teams))
{
  for(index_sp1_mfyc_cols in 1:sp1_totalrounds)
  {
    index_sp1_mfyc  <- row.names(sp1_matchfirstyc_h) == sp1_teams[index_sp1_mfyc]
    form_sp1_mfyc <- sp1_matchfirstyc_h[index_sp1_mfyc ]
    deleted_form_sp1_mfyc <- form_sp1_mfyc[!form_sp1_mfyc[] == ""]
    l6_form_sp1_mfyc <- tail(deleted_form_sp1_mfyc,sp1_last_n_games)
    l6_form_sp1_mfyc <- as.numeric(l6_form_sp1_mfyc)
    suml6_sp1_mfyc[index_sp1_mfyc] <- sum(l6_form_sp1_mfyc)
    suml6_sp1_mfyc[index_sp1_mfyc] <- paste(suml6_sp1_mfyc[index_sp1_mfyc],sep = "")
    sum_sp1_zero_mfyc[index_sp1_mfyc] <- length(which(l6_form_sp1_mfyc == 0))
    sum_sp1_zero_mfyc[index_sp1_mfyc] <- paste(sum_sp1_zero_mfyc[index_sp1_mfyc],sep = "")
    sum_sp1_1to15_mfyc[index_sp1_mfyc] <- length(which(l6_form_sp1_mfyc >= 1 & l6_form_sp1_mfyc <= 15))
    sum_sp1_1to15_mfyc[index_sp1_mfyc] <- paste(sum_sp1_1to15_mfyc[index_sp1_mfyc],sep = "")
    sum_sp1_15to30_mfyc[index_sp1_mfyc] <- length(which(l6_form_sp1_mfyc >= 15 & l6_form_sp1_mfyc <= 30))
    sum_sp1_15to30_mfyc[index_sp1_mfyc] <- paste(sum_sp1_15to30_mfyc[index_sp1_mfyc],sep = "")
    sum_sp1_30to45_mfyc[index_sp1_mfyc] <- length(which(l6_form_sp1_mfyc >= 30 & l6_form_sp1_mfyc <= 45))
    sum_sp1_30to45_mfyc[index_sp1_mfyc] <- paste(sum_sp1_30to45_mfyc[index_sp1_mfyc],sep = "")
    sum_sp1_45to60_mfyc[index_sp1_mfyc] <- length(which(l6_form_sp1_mfyc >= 45 & l6_form_sp1_mfyc <= 60))
    sum_sp1_45to60_mfyc[index_sp1_mfyc] <- paste(sum_sp1_45to60_mfyc[index_sp1_mfyc],sep = "")
    sum_sp1_60to75_mfyc[index_sp1_mfyc] <- length(which(l6_form_sp1_mfyc >= 60 & l6_form_sp1_mfyc <= 75))
    sum_sp1_60to75_mfyc[index_sp1_mfyc] <- paste(sum_sp1_60to75_mfyc[index_sp1_mfyc],sep = "")
    sum_sp1_75to100_mfyc[index_sp1_mfyc] <- length(which(l6_form_sp1_mfyc >= 75 & l6_form_sp1_mfyc <= 100))
    sum_sp1_75to100_mfyc[index_sp1_mfyc] <- paste(sum_sp1_75to100_mfyc[index_sp1_mfyc],sep = "")
    avgr_sp1_mfyc[index_sp1_mfyc] <- mean(l6_form_sp1_mfyc)
    avgr_sp1_mfyc[index_sp1_mfyc] <- paste(avgr_sp1_mfyc[index_sp1_mfyc],sep = "")
    sdr_sp1_mfyc[index_sp1_mfyc] <- sd(l6_form_sp1_mfyc)
    sdr_sp1_mfyc[index_sp1_mfyc] <- paste(sdr_sp1_mfyc[index_sp1_mfyc],sep = "")
    l6_form_sp1_mfyc <- as.character(l6_form_sp1_mfyc)
    #l6_form_sp1_mfyc_flattened <- stri_paste(l6_form_sp1_mfyc,collapse = '')
    #l6_form_sp1_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_sp1_mfyc_flattened),"")[[1]])
    final_sp1_mfyc[index_sp1_mfyc,index_sp1_mfyc_cols] <- l6_form_sp1_mfyc[index_sp1_mfyc_cols]
  }
}

final_sp1_mfyc[is.na(final_sp1_mfyc)] <- ""
sp1_matchfirstycmatrix <- cbind(sp1_teams,final_sp1_mfyc,suml6_sp1_mfyc,sum_sp1_zero_mfyc,sum_sp1_1to15_mfyc,sum_sp1_15to30_mfyc,sum_sp1_30to45_mfyc,sum_sp1_45to60_mfyc,sum_sp1_60to75_mfyc,sum_sp1_75to100_mfyc,avgr_sp1_mfyc,sdr_sp1_mfyc)
write.xlsx(sp1_matchfirstycmatrix,"Analytics/BIGFIVE/MatchFirstYCanalysis.xlsx", sheetName = "SP1", append = TRUE)
###################################################################################################################################################################################
###################################################################################################################################################################################
#D1
D1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "D1")

d1_matchfirstyc_h <- tapply(D1_matchfirstyc$Home_first_YCTime, D1_matchfirstyc[c("HomeTeam", "Date")],mean)
d1_matchfirstyc_a <- tapply(D1_matchfirstyc$Away_first_YCTime, D1_matchfirstyc[c("AwayTeam", "Date")],mean)

d1_matchfirstyc_h[is.na(d1_matchfirstyc_h)] <- ""
d1_matchfirstyc_a[is.na(d1_matchfirstyc_a)] <- ""

for(d1_rowhyc in 1:nrow(d1_matchfirstyc_h)) {
  for(d1_colhyc in 1:ncol(d1_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(d1_rowayc in 1:nrow(d1_matchfirstyc_a)) {
      for(d1_colayc in 1:ncol(d1_matchfirstyc_a)) {
        ifelse(!d1_matchfirstyc_a[d1_rowayc,d1_colayc]=="",d1_matchfirstyc_h[d1_rowayc,d1_colayc] <- d1_matchfirstyc_a[d1_rowayc,d1_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

d1_home_games <- c()
d1_away_games <-c()

for (i_d1 in 1:length(d1_teams))
{

  d1_home_games[i_d1] <- nrow(D1_matchfirstyc[D1_matchfirstyc$HomeTeam == d1_teams[i_d1],])
  d1_away_games[i_d1]  <- nrow(D1_matchfirstyc[D1_matchfirstyc$AwayTeam == d1_teams[i_d1],])
}
d1_games_played <- d1_home_games + d1_away_games
d1_last_n_games <- d1_games_played[1]

d1_totalrounds <-  (length(d1_teams) - 1 )*2

final_d1_mfyc <- matrix(nrow = length(d1_teams),ncol = d1_totalrounds )
suml6_d1_mfyc <- c()
sum_d1_zero_mfyc <- c()
sum_d1_1to15_mfyc <- c()
sum_d1_15to30_mfyc <- c()
sum_d1_30to45_mfyc <- c()
sum_d1_45to60_mfyc <- c()
sum_d1_60to75_mfyc <- c()
sum_d1_75to100_mfyc <- c()
avgr_d1_mfyc <- c()
sdr_d1_mfyc <- c()
l6_form_d1_mfycsplitted <- c()
form_d1_mfyc <- c()
for(index_d1_mfyc in 1:length(d1_teams))
{
  for(index_d1_mfyc_cols in 1:d1_totalrounds)
  {
    index_d1_mfyc  <- row.names(d1_matchfirstyc_h) == d1_teams[index_d1_mfyc]
    form_d1_mfyc <- d1_matchfirstyc_h[index_d1_mfyc ]
    deleted_form_d1_mfyc <- form_d1_mfyc[!form_d1_mfyc[] == ""]
    l6_form_d1_mfyc <- tail(deleted_form_d1_mfyc,d1_last_n_games)
    l6_form_d1_mfyc <- as.numeric(l6_form_d1_mfyc)
    suml6_d1_mfyc[index_d1_mfyc] <- sum(l6_form_d1_mfyc)
    suml6_d1_mfyc[index_d1_mfyc] <- paste(suml6_d1_mfyc[index_d1_mfyc],sep = "")
    sum_d1_zero_mfyc[index_d1_mfyc] <- length(which(l6_form_d1_mfyc == 0))
    sum_d1_zero_mfyc[index_d1_mfyc] <- paste(sum_d1_zero_mfyc[index_d1_mfyc],sep = "")
    sum_d1_1to15_mfyc[index_d1_mfyc] <- length(which(l6_form_d1_mfyc >= 1 & l6_form_d1_mfyc <= 15))
    sum_d1_1to15_mfyc[index_d1_mfyc] <- paste(sum_d1_1to15_mfyc[index_d1_mfyc],sep = "")
    sum_d1_15to30_mfyc[index_d1_mfyc] <- length(which(l6_form_d1_mfyc >= 15 & l6_form_d1_mfyc <= 30))
    sum_d1_15to30_mfyc[index_d1_mfyc] <- paste(sum_d1_15to30_mfyc[index_d1_mfyc],sep = "")
    sum_d1_30to45_mfyc[index_d1_mfyc] <- length(which(l6_form_d1_mfyc >= 30 & l6_form_d1_mfyc <= 45))
    sum_d1_30to45_mfyc[index_d1_mfyc] <- paste(sum_d1_30to45_mfyc[index_d1_mfyc],sep = "")
    sum_d1_45to60_mfyc[index_d1_mfyc] <- length(which(l6_form_d1_mfyc >= 45 & l6_form_d1_mfyc <= 60))
    sum_d1_45to60_mfyc[index_d1_mfyc] <- paste(sum_d1_45to60_mfyc[index_d1_mfyc],sep = "")
    sum_d1_60to75_mfyc[index_d1_mfyc] <- length(which(l6_form_d1_mfyc >= 60 & l6_form_d1_mfyc <= 75))
    sum_d1_60to75_mfyc[index_d1_mfyc] <- paste(sum_d1_60to75_mfyc[index_d1_mfyc],sep = "")
    sum_d1_75to100_mfyc[index_d1_mfyc] <- length(which(l6_form_d1_mfyc >= 75 & l6_form_d1_mfyc <= 100))
    sum_d1_75to100_mfyc[index_d1_mfyc] <- paste(sum_d1_75to100_mfyc[index_d1_mfyc],sep = "")
    avgr_d1_mfyc[index_d1_mfyc] <- mean(l6_form_d1_mfyc)
    avgr_d1_mfyc[index_d1_mfyc] <- paste(avgr_d1_mfyc[index_d1_mfyc],sep = "")
    sdr_d1_mfyc[index_d1_mfyc] <- sd(l6_form_d1_mfyc)
    sdr_d1_mfyc[index_d1_mfyc] <- paste(sdr_d1_mfyc[index_d1_mfyc],sep = "")
    l6_form_d1_mfyc <- as.character(l6_form_d1_mfyc)
    #l6_form_d1_mfyc_flattened <- stri_paste(l6_form_d1_mfyc,collapse = '')
    #l6_form_d1_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_d1_mfyc_flattened),"")[[1]])
    final_d1_mfyc[index_d1_mfyc,index_d1_mfyc_cols] <- l6_form_d1_mfyc[index_d1_mfyc_cols]
  }
}

final_d1_mfyc[is.na(final_d1_mfyc)] <- ""
d1_matchfirstycmatrix <- cbind(d1_teams,final_d1_mfyc,suml6_d1_mfyc,sum_d1_zero_mfyc,sum_d1_1to15_mfyc,sum_d1_15to30_mfyc,sum_d1_30to45_mfyc,sum_d1_45to60_mfyc,sum_d1_60to75_mfyc,sum_d1_75to100_mfyc,avgr_d1_mfyc,sdr_d1_mfyc)
write.xlsx(d1_matchfirstycmatrix,"Analytics/BIGFIVE/MatchFirstYCanalysis.xlsx", sheetName = "D1", append = TRUE)
#########################################################################################################################################################################################
#########################################################################################################################################################################################
#F1
F1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "F1")

f1_matchfirstyc_h <- tapply(F1_matchfirstyc$Home_first_YCTime, F1_matchfirstyc[c("HomeTeam", "Date")],mean)
f1_matchfirstyc_a <- tapply(F1_matchfirstyc$Away_first_YCTime, F1_matchfirstyc[c("AwayTeam", "Date")],mean)

f1_matchfirstyc_h[is.na(f1_matchfirstyc_h)] <- ""
f1_matchfirstyc_a[is.na(f1_matchfirstyc_a)] <- ""

for(f1_rowhyc in 1:nrow(f1_matchfirstyc_h)) {
  for(f1_colhyc in 1:ncol(f1_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(f1_rowayc in 1:nrow(f1_matchfirstyc_a)) {
      for(f1_colayc in 1:ncol(f1_matchfirstyc_a)) {
        ifelse(!f1_matchfirstyc_a[f1_rowayc,f1_colayc]=="",f1_matchfirstyc_h[f1_rowayc,f1_colayc] <- f1_matchfirstyc_a[f1_rowayc,f1_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

f1_home_games <- c()
f1_away_games <-c()

for (i_f1 in 1:length(f1_teams))
{

  f1_home_games[i_f1] <- nrow(F1_matchfirstyc[F1_matchfirstyc$HomeTeam == f1_teams[i_f1],])
  f1_away_games[i_f1]  <- nrow(F1_matchfirstyc[F1_matchfirstyc$AwayTeam == f1_teams[i_f1],])
}
f1_games_played <- f1_home_games + f1_away_games
f1_last_n_games <- f1_games_played[1]

f1_totalrounds <-  (length(f1_teams) - 1 )*2

final_f1_mfyc <- matrix(nrow = length(f1_teams),ncol = f1_totalrounds )
suml6_f1_mfyc <- c()
sum_f1_zero_mfyc <- c()
sum_f1_1to15_mfyc <- c()
sum_f1_15to30_mfyc <- c()
sum_f1_30to45_mfyc <- c()
sum_f1_45to60_mfyc <- c()
sum_f1_60to75_mfyc <- c()
sum_f1_75to100_mfyc <- c()
avgr_f1_mfyc <- c()
sdr_f1_mfyc <- c()
l6_form_f1_mfycsplitted <- c()
form_f1_mfyc <- c()
for(index_f1_mfyc in 1:length(f1_teams))
{
  for(index_f1_mfyc_cols in 1:f1_totalrounds)
  {
    index_f1_mfyc  <- row.names(f1_matchfirstyc_h) == f1_teams[index_f1_mfyc]
    form_f1_mfyc <- f1_matchfirstyc_h[index_f1_mfyc ]
    deleted_form_f1_mfyc <- form_f1_mfyc[!form_f1_mfyc[] == ""]
    l6_form_f1_mfyc <- tail(deleted_form_f1_mfyc,f1_last_n_games)
    l6_form_f1_mfyc <- as.numeric(l6_form_f1_mfyc)
    suml6_f1_mfyc[index_f1_mfyc] <- sum(l6_form_f1_mfyc)
    suml6_f1_mfyc[index_f1_mfyc] <- paste(suml6_f1_mfyc[index_f1_mfyc],sep = "")
    sum_f1_zero_mfyc[index_f1_mfyc] <- length(which(l6_form_f1_mfyc == 0))
    sum_f1_zero_mfyc[index_f1_mfyc] <- paste(sum_f1_zero_mfyc[index_f1_mfyc],sep = "")
    sum_f1_1to15_mfyc[index_f1_mfyc] <- length(which(l6_form_f1_mfyc >= 1 & l6_form_f1_mfyc <= 15))
    sum_f1_1to15_mfyc[index_f1_mfyc] <- paste(sum_f1_1to15_mfyc[index_f1_mfyc],sep = "")
    sum_f1_15to30_mfyc[index_f1_mfyc] <- length(which(l6_form_f1_mfyc >= 15 & l6_form_f1_mfyc <= 30))
    sum_f1_15to30_mfyc[index_f1_mfyc] <- paste(sum_f1_15to30_mfyc[index_f1_mfyc],sep = "")
    sum_f1_30to45_mfyc[index_f1_mfyc] <- length(which(l6_form_f1_mfyc >= 30 & l6_form_f1_mfyc <= 45))
    sum_f1_30to45_mfyc[index_f1_mfyc] <- paste(sum_f1_30to45_mfyc[index_f1_mfyc],sep = "")
    sum_f1_45to60_mfyc[index_f1_mfyc] <- length(which(l6_form_f1_mfyc >= 45 & l6_form_f1_mfyc <= 60))
    sum_f1_45to60_mfyc[index_f1_mfyc] <- paste(sum_f1_45to60_mfyc[index_f1_mfyc],sep = "")
    sum_f1_60to75_mfyc[index_f1_mfyc] <- length(which(l6_form_f1_mfyc >= 60 & l6_form_f1_mfyc <= 75))
    sum_f1_60to75_mfyc[index_f1_mfyc] <- paste(sum_f1_60to75_mfyc[index_f1_mfyc],sep = "")
    sum_f1_75to100_mfyc[index_f1_mfyc] <- length(which(l6_form_f1_mfyc >= 75 & l6_form_f1_mfyc <= 100))
    sum_f1_75to100_mfyc[index_f1_mfyc] <- paste(sum_f1_75to100_mfyc[index_f1_mfyc],sep = "")
    avgr_f1_mfyc[index_f1_mfyc] <- mean(l6_form_f1_mfyc)
    avgr_f1_mfyc[index_f1_mfyc] <- paste(avgr_f1_mfyc[index_f1_mfyc],sep = "")
    sdr_f1_mfyc[index_f1_mfyc] <- sd(l6_form_f1_mfyc)
    sdr_f1_mfyc[index_f1_mfyc] <- paste(sdr_f1_mfyc[index_f1_mfyc],sep = "")
    l6_form_f1_mfyc <- as.character(l6_form_f1_mfyc)
    #l6_form_f1_mfyc_flattened <- stri_paste(l6_form_f1_mfyc,collapse = '')
    #l6_form_f1_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_f1_mfyc_flattened),"")[[1]])
    final_f1_mfyc[index_f1_mfyc,index_f1_mfyc_cols] <- l6_form_f1_mfyc[index_f1_mfyc_cols]
  }
}

final_f1_mfyc[is.na(final_f1_mfyc)] <- ""
f1_matchfirstycmatrix <- cbind(f1_teams,final_f1_mfyc,suml6_f1_mfyc,sum_f1_zero_mfyc,sum_f1_1to15_mfyc,sum_f1_15to30_mfyc,sum_f1_30to45_mfyc,sum_f1_45to60_mfyc,sum_f1_60to75_mfyc,sum_f1_75to100_mfyc,avgr_f1_mfyc,sdr_f1_mfyc)
write.xlsx(f1_matchfirstycmatrix,"Analytics/BIGFIVE/MatchFirstYCanalysis.xlsx", sheetName = "F1", append = TRUE)
#########################################################################################################################################################################################
#########################################################################################################################################################################################
#I1
I1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "I1")

i1_matchfirstyc_h <- tapply(I1_matchfirstyc$Home_first_YCTime, I1_matchfirstyc[c("HomeTeam", "Date")],mean)
i1_matchfirstyc_a <- tapply(I1_matchfirstyc$Away_first_YCTime, I1_matchfirstyc[c("AwayTeam", "Date")],mean)

i1_matchfirstyc_h[is.na(i1_matchfirstyc_h)] <- ""
i1_matchfirstyc_a[is.na(i1_matchfirstyc_a)] <- ""

for(i1_rowhyc in 1:nrow(i1_matchfirstyc_h)) {
  for(i1_colhyc in 1:ncol(i1_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(i1_rowayc in 1:nrow(i1_matchfirstyc_a)) {
      for(i1_colayc in 1:ncol(i1_matchfirstyc_a)) {
        ifelse(!i1_matchfirstyc_a[i1_rowayc,i1_colayc]=="",i1_matchfirstyc_h[i1_rowayc,i1_colayc] <- i1_matchfirstyc_a[i1_rowayc,i1_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

i1_home_games <- c()
i1_away_games <-c()

for (i_i1 in 1:length(i1_teams))
{

  i1_home_games[i_i1] <- nrow(I1_matchfirstyc[I1_matchfirstyc$HomeTeam == i1_teams[i_i1],])
  i1_away_games[i_i1]  <- nrow(I1_matchfirstyc[I1_matchfirstyc$AwayTeam == i1_teams[i_i1],])
}
i1_games_played <- i1_home_games + i1_away_games
i1_last_n_games <- i1_games_played[1]

i1_totalrounds <-  (length(i1_teams) - 1 )*2

final_i1_mfyc <- matrix(nrow = length(i1_teams),ncol = i1_totalrounds )
suml6_i1_mfyc <- c()
sum_i1_zero_mfyc <- c()
sum_i1_1to15_mfyc <- c()
sum_i1_15to30_mfyc <- c()
sum_i1_30to45_mfyc <- c()
sum_i1_45to60_mfyc <- c()
sum_i1_60to75_mfyc <- c()
sum_i1_75to100_mfyc <- c()
avgr_i1_mfyc <- c()
sdr_i1_mfyc <- c()
l6_form_i1_mfycsplitted <- c()
form_i1_mfyc <- c()
for(index_i1_mfyc in 1:length(i1_teams))
{
  for(index_i1_mfyc_cols in 1:i1_totalrounds)
  {
    index_i1_mfyc  <- row.names(i1_matchfirstyc_h) == i1_teams[index_i1_mfyc]
    form_i1_mfyc <- i1_matchfirstyc_h[index_i1_mfyc ]
    deleted_form_i1_mfyc <- form_i1_mfyc[!form_i1_mfyc[] == ""]
    l6_form_i1_mfyc <- tail(deleted_form_i1_mfyc,i1_last_n_games)
    l6_form_i1_mfyc <- as.numeric(l6_form_i1_mfyc)
    suml6_i1_mfyc[index_i1_mfyc] <- sum(l6_form_i1_mfyc)
    suml6_i1_mfyc[index_i1_mfyc] <- paste(suml6_i1_mfyc[index_i1_mfyc],sep = "")
    sum_i1_zero_mfyc[index_i1_mfyc] <- length(which(l6_form_i1_mfyc == 0))
    sum_i1_zero_mfyc[index_i1_mfyc] <- paste(sum_i1_zero_mfyc[index_i1_mfyc],sep = "")
    sum_i1_1to15_mfyc[index_i1_mfyc] <- length(which(l6_form_i1_mfyc >= 1 & l6_form_i1_mfyc <= 15))
    sum_i1_1to15_mfyc[index_i1_mfyc] <- paste(sum_i1_1to15_mfyc[index_i1_mfyc],sep = "")
    sum_i1_15to30_mfyc[index_i1_mfyc] <- length(which(l6_form_i1_mfyc >= 15 & l6_form_i1_mfyc <= 30))
    sum_i1_15to30_mfyc[index_i1_mfyc] <- paste(sum_i1_15to30_mfyc[index_i1_mfyc],sep = "")
    sum_i1_30to45_mfyc[index_i1_mfyc] <- length(which(l6_form_i1_mfyc >= 30 & l6_form_i1_mfyc <= 45))
    sum_i1_30to45_mfyc[index_i1_mfyc] <- paste(sum_i1_30to45_mfyc[index_i1_mfyc],sep = "")
    sum_i1_45to60_mfyc[index_i1_mfyc] <- length(which(l6_form_i1_mfyc >= 45 & l6_form_i1_mfyc <= 60))
    sum_i1_45to60_mfyc[index_i1_mfyc] <- paste(sum_i1_45to60_mfyc[index_i1_mfyc],sep = "")
    sum_i1_60to75_mfyc[index_i1_mfyc] <- length(which(l6_form_i1_mfyc >= 60 & l6_form_i1_mfyc <= 75))
    sum_i1_60to75_mfyc[index_i1_mfyc] <- paste(sum_i1_60to75_mfyc[index_i1_mfyc],sep = "")
    sum_i1_75to100_mfyc[index_i1_mfyc] <- length(which(l6_form_i1_mfyc >= 75 & l6_form_i1_mfyc <= 100))
    sum_i1_75to100_mfyc[index_i1_mfyc] <- paste(sum_i1_75to100_mfyc[index_i1_mfyc],sep = "")
    avgr_i1_mfyc[index_i1_mfyc] <- mean(l6_form_i1_mfyc)
    avgr_i1_mfyc[index_i1_mfyc] <- paste(avgr_i1_mfyc[index_i1_mfyc],sep = "")
    sdr_i1_mfyc[index_i1_mfyc] <- sd(l6_form_i1_mfyc)
    sdr_i1_mfyc[index_i1_mfyc] <- paste(sdr_i1_mfyc[index_i1_mfyc],sep = "")
    l6_form_i1_mfyc <- as.character(l6_form_i1_mfyc)
    #l6_form_i1_mfyc_flattened <- stri_paste(l6_form_i1_mfyc,collapse = '')
    #l6_form_i1_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_i1_mfyc_flattened),"")[[1]])
    final_i1_mfyc[index_i1_mfyc,index_i1_mfyc_cols] <- l6_form_i1_mfyc[index_i1_mfyc_cols]
  }
}

final_i1_mfyc[is.na(final_i1_mfyc)] <- ""
i1_matchfirstycmatrix <- cbind(i1_teams,final_i1_mfyc,suml6_i1_mfyc,sum_i1_zero_mfyc,sum_i1_1to15_mfyc,sum_i1_15to30_mfyc,sum_i1_30to45_mfyc,sum_i1_45to60_mfyc,sum_i1_60to75_mfyc,sum_i1_75to100_mfyc,avgr_i1_mfyc,sdr_i1_mfyc)
write.xlsx(i1_matchfirstycmatrix,"Analytics/BIGFIVE/MatchFirstYCanalysis.xlsx", sheetName = "I1", append = TRUE)
######################################################################################################################################################################################
######################################################################################################################################################################################
################
#OTHER LEAGUES##
################
OTHERLEAGUES_analytics <- readxl::read_excel('OTHERLEAGUES20232024.xlsx')
OTHERLEAGUES_analytics <- OTHERLEAGUES_analytics[,-1]
OTHERLEAGUES_analytics <- as.data.frame(OTHERLEAGUES_analytics)

#B1
B1_matchfirstyc <- subset(OTHERLEAGUES_analytics, Div == "B1")

b1_matchfirstyc_h <- tapply(B1_matchfirstyc$Home_first_YCTime, B1_matchfirstyc[c("HomeTeam", "Date")],mean)
b1_matchfirstyc_a <- tapply(B1_matchfirstyc$Away_first_YCTime, B1_matchfirstyc[c("AwayTeam", "Date")],mean)

b1_matchfirstyc_h[is.na(b1_matchfirstyc_h)] <- ""
b1_matchfirstyc_a[is.na(b1_matchfirstyc_a)] <- ""

for(b1_rowhyc in 1:nrow(b1_matchfirstyc_h)) {
  for(b1_colhyc in 1:ncol(b1_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(b1_rowayc in 1:nrow(b1_matchfirstyc_a)) {
      for(b1_colayc in 1:ncol(b1_matchfirstyc_a)) {
        ifelse(!b1_matchfirstyc_a[b1_rowayc,b1_colayc]=="",b1_matchfirstyc_h[b1_rowayc,b1_colayc] <- b1_matchfirstyc_a[b1_rowayc,b1_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

b1_home_games <- c()
b1_away_games <-c()

for (i_b1 in 1:length(b1_teams))
{

  b1_home_games[i_b1] <- nrow(B1_matchfirstyc[B1_matchfirstyc$HomeTeam == b1_teams[i_b1],])
  b1_away_games[i_b1]  <- nrow(B1_matchfirstyc[B1_matchfirstyc$AwayTeam == b1_teams[i_b1],])
}
b1_games_played <- b1_home_games + b1_away_games
b1_last_n_games <- b1_games_played[1]

b1_totalrounds <-  (length(b1_teams) - 1 )*2

final_b1_mfyc <- matrix(nrow = length(b1_teams),ncol = b1_totalrounds )
suml6_b1_mfyc <- c()
sum_b1_zero_mfyc <- c()
sum_b1_1to15_mfyc <- c()
sum_b1_15to30_mfyc <- c()
sum_b1_30to45_mfyc <- c()
sum_b1_45to60_mfyc <- c()
sum_b1_60to75_mfyc <- c()
sum_b1_75to100_mfyc <- c()
avgr_b1_mfyc <- c()
sdr_b1_mfyc <- c()
l6_form_b1_mfycsplitted <- c()
form_b1_mfyc <- c()
for(index_b1_mfyc in 1:length(b1_teams))
{
  for(index_b1_mfyc_cols in 1:b1_totalrounds)
  {
    index_b1_mfyc  <- row.names(b1_matchfirstyc_h) == b1_teams[index_b1_mfyc]
    form_b1_mfyc <- b1_matchfirstyc_h[index_b1_mfyc ]
    deleted_form_b1_mfyc <- form_b1_mfyc[!form_b1_mfyc[] == ""]
    l6_form_b1_mfyc <- tail(deleted_form_b1_mfyc,b1_last_n_games)
    l6_form_b1_mfyc <- as.numeric(l6_form_b1_mfyc)
    suml6_b1_mfyc[index_b1_mfyc] <- sum(l6_form_b1_mfyc)
    suml6_b1_mfyc[index_b1_mfyc] <- paste(suml6_b1_mfyc[index_b1_mfyc],sep = "")
    sum_b1_zero_mfyc[index_b1_mfyc] <- length(which(l6_form_b1_mfyc == 0))
    sum_b1_zero_mfyc[index_b1_mfyc] <- paste(sum_b1_zero_mfyc[index_b1_mfyc],sep = "")
    sum_b1_1to15_mfyc[index_b1_mfyc] <- length(which(l6_form_b1_mfyc >= 1 & l6_form_b1_mfyc <= 15))
    sum_b1_1to15_mfyc[index_b1_mfyc] <- paste(sum_b1_1to15_mfyc[index_b1_mfyc],sep = "")
    sum_b1_15to30_mfyc[index_b1_mfyc] <- length(which(l6_form_b1_mfyc >= 15 & l6_form_b1_mfyc <= 30))
    sum_b1_15to30_mfyc[index_b1_mfyc] <- paste(sum_b1_15to30_mfyc[index_b1_mfyc],sep = "")
    sum_b1_30to45_mfyc[index_b1_mfyc] <- length(which(l6_form_b1_mfyc >= 30 & l6_form_b1_mfyc <= 45))
    sum_b1_30to45_mfyc[index_b1_mfyc] <- paste(sum_b1_30to45_mfyc[index_b1_mfyc],sep = "")
    sum_b1_45to60_mfyc[index_b1_mfyc] <- length(which(l6_form_b1_mfyc >= 45 & l6_form_b1_mfyc <= 60))
    sum_b1_45to60_mfyc[index_b1_mfyc] <- paste(sum_b1_45to60_mfyc[index_b1_mfyc],sep = "")
    sum_b1_60to75_mfyc[index_b1_mfyc] <- length(which(l6_form_b1_mfyc >= 60 & l6_form_b1_mfyc <= 75))
    sum_b1_60to75_mfyc[index_b1_mfyc] <- paste(sum_b1_60to75_mfyc[index_b1_mfyc],sep = "")
    sum_b1_75to100_mfyc[index_b1_mfyc] <- length(which(l6_form_b1_mfyc >= 75 & l6_form_b1_mfyc <= 100))
    sum_b1_75to100_mfyc[index_b1_mfyc] <- paste(sum_b1_75to100_mfyc[index_b1_mfyc],sep = "")
    avgr_b1_mfyc[index_b1_mfyc] <- mean(l6_form_b1_mfyc)
    avgr_b1_mfyc[index_b1_mfyc] <- paste(avgr_b1_mfyc[index_b1_mfyc],sep = "")
    sdr_b1_mfyc[index_b1_mfyc] <- sd(l6_form_b1_mfyc)
    sdr_b1_mfyc[index_b1_mfyc] <- paste(sdr_b1_mfyc[index_b1_mfyc],sep = "")
    l6_form_b1_mfyc <- as.character(l6_form_b1_mfyc)
    #l6_form_b1_mfyc_flattened <- stri_paste(l6_form_b1_mfyc,collapse = '')
    #l6_form_b1_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_b1_mfyc_flattened),"")[[1]])
    final_b1_mfyc[index_b1_mfyc,index_b1_mfyc_cols] <- l6_form_b1_mfyc[index_b1_mfyc_cols]
  }
}

final_b1_mfyc[is.na(final_b1_mfyc)] <- ""
b1_matchfirstycmatrix <- cbind(b1_teams,final_b1_mfyc,suml6_b1_mfyc,sum_b1_zero_mfyc,sum_b1_1to15_mfyc,sum_b1_15to30_mfyc,sum_b1_30to45_mfyc,sum_b1_45to60_mfyc,sum_b1_60to75_mfyc,sum_b1_75to100_mfyc,avgr_b1_mfyc,sdr_b1_mfyc)
unlink('Analytics/OTHERLEAGUES/MatchFirstYCanalysis.xlsx')
write.xlsx(b1_matchfirstycmatrix,"Analytics/OTHERLEAGUES/MatchFirstYCanalysis.xlsx", sheetName = "B1", append = TRUE)
#######################################################################################################################################################################
#######################################################################################################################################################################
#D2
D2_matchfirstyc <- subset(OTHERLEAGUES_analytics, Div == "D2")

d2_matchfirstyc_h <- tapply(D2_matchfirstyc$Home_first_YCTime, D2_matchfirstyc[c("HomeTeam", "Date")],mean)
d2_matchfirstyc_a <- tapply(D2_matchfirstyc$Away_first_YCTime, D2_matchfirstyc[c("AwayTeam", "Date")],mean)

d2_matchfirstyc_h[is.na(d2_matchfirstyc_h)] <- ""
d2_matchfirstyc_a[is.na(d2_matchfirstyc_a)] <- ""

for(d2_rowhyc in 1:nrow(d2_matchfirstyc_h)) {
  for(d2_colhyc in 1:ncol(d2_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(d2_rowayc in 1:nrow(d2_matchfirstyc_a)) {
      for(d2_colayc in 1:ncol(d2_matchfirstyc_a)) {
        ifelse(!d2_matchfirstyc_a[d2_rowayc,d2_colayc]=="",d2_matchfirstyc_h[d2_rowayc,d2_colayc] <- d2_matchfirstyc_a[d2_rowayc,d2_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

d2_home_games <- c()
d2_away_games <-c()

for (i_d2 in 1:length(d2_teams))
{

  d2_home_games[i_d2] <- nrow(D2_matchfirstyc[D2_matchfirstyc$HomeTeam == d2_teams[i_d2],])
  d2_away_games[i_d2]  <- nrow(D2_matchfirstyc[D2_matchfirstyc$AwayTeam == d2_teams[i_d2],])
}
d2_games_played <- d2_home_games + d2_away_games
d2_last_n_games <- d2_games_played[1]

d2_totalrounds <-  (length(d2_teams) - 1 )*2

final_d2_mfyc <- matrix(nrow = length(d2_teams),ncol = d2_totalrounds )
suml6_d2_mfyc <- c()
sum_d2_zero_mfyc <- c()
sum_d2_1to15_mfyc <- c()
sum_d2_15to30_mfyc <- c()
sum_d2_30to45_mfyc <- c()
sum_d2_45to60_mfyc <- c()
sum_d2_60to75_mfyc <- c()
sum_d2_75to100_mfyc <- c()
avgr_d2_mfyc <- c()
sdr_d2_mfyc <- c()
l6_form_d2_mfycsplitted <- c()
form_d2_mfyc <- c()
for(index_d2_mfyc in 1:length(d2_teams))
{
  for(index_d2_mfyc_cols in 1:d2_totalrounds)
  {
    index_d2_mfyc  <- row.names(d2_matchfirstyc_h) == d2_teams[index_d2_mfyc]
    form_d2_mfyc <- d2_matchfirstyc_h[index_d2_mfyc ]
    deleted_form_d2_mfyc <- form_d2_mfyc[!form_d2_mfyc[] == ""]
    l6_form_d2_mfyc <- tail(deleted_form_d2_mfyc,d2_last_n_games)
    l6_form_d2_mfyc <- as.numeric(l6_form_d2_mfyc)
    suml6_d2_mfyc[index_d2_mfyc] <- sum(l6_form_d2_mfyc)
    suml6_d2_mfyc[index_d2_mfyc] <- paste(suml6_d2_mfyc[index_d2_mfyc],sep = "")
    sum_d2_zero_mfyc[index_d2_mfyc] <- length(which(l6_form_d2_mfyc == 0))
    sum_d2_zero_mfyc[index_d2_mfyc] <- paste(sum_d2_zero_mfyc[index_d2_mfyc],sep = "")
    sum_d2_1to15_mfyc[index_d2_mfyc] <- length(which(l6_form_d2_mfyc >= 1 & l6_form_d2_mfyc <= 15))
    sum_d2_1to15_mfyc[index_d2_mfyc] <- paste(sum_d2_1to15_mfyc[index_d2_mfyc],sep = "")
    sum_d2_15to30_mfyc[index_d2_mfyc] <- length(which(l6_form_d2_mfyc >= 15 & l6_form_d2_mfyc <= 30))
    sum_d2_15to30_mfyc[index_d2_mfyc] <- paste(sum_d2_15to30_mfyc[index_d2_mfyc],sep = "")
    sum_d2_30to45_mfyc[index_d2_mfyc] <- length(which(l6_form_d2_mfyc >= 30 & l6_form_d2_mfyc <= 45))
    sum_d2_30to45_mfyc[index_d2_mfyc] <- paste(sum_d2_30to45_mfyc[index_d2_mfyc],sep = "")
    sum_d2_45to60_mfyc[index_d2_mfyc] <- length(which(l6_form_d2_mfyc >= 45 & l6_form_d2_mfyc <= 60))
    sum_d2_45to60_mfyc[index_d2_mfyc] <- paste(sum_d2_45to60_mfyc[index_d2_mfyc],sep = "")
    sum_d2_60to75_mfyc[index_d2_mfyc] <- length(which(l6_form_d2_mfyc >= 60 & l6_form_d2_mfyc <= 75))
    sum_d2_60to75_mfyc[index_d2_mfyc] <- paste(sum_d2_60to75_mfyc[index_d2_mfyc],sep = "")
    sum_d2_75to100_mfyc[index_d2_mfyc] <- length(which(l6_form_d2_mfyc >= 75 & l6_form_d2_mfyc <= 100))
    sum_d2_75to100_mfyc[index_d2_mfyc] <- paste(sum_d2_75to100_mfyc[index_d2_mfyc],sep = "")
    avgr_d2_mfyc[index_d2_mfyc] <- mean(l6_form_d2_mfyc)
    avgr_d2_mfyc[index_d2_mfyc] <- paste(avgr_d2_mfyc[index_d2_mfyc],sep = "")
    sdr_d2_mfyc[index_d2_mfyc] <- sd(l6_form_d2_mfyc)
    sdr_d2_mfyc[index_d2_mfyc] <- paste(sdr_d2_mfyc[index_d2_mfyc],sep = "")
    l6_form_d2_mfyc <- as.character(l6_form_d2_mfyc)
    #l6_form_d2_mfyc_flattened <- stri_paste(l6_form_d2_mfyc,collapse = '')
    #l6_form_d2_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_d2_mfyc_flattened),"")[[1]])
    final_d2_mfyc[index_d2_mfyc,index_d2_mfyc_cols] <- l6_form_d2_mfyc[index_d2_mfyc_cols]
  }
}

final_d2_mfyc[is.na(final_d2_mfyc)] <- ""
d2_matchfirstycmatrix <- cbind(d2_teams,final_d2_mfyc,suml6_d2_mfyc,sum_d2_zero_mfyc,sum_d2_1to15_mfyc,sum_d2_15to30_mfyc,sum_d2_30to45_mfyc,sum_d2_45to60_mfyc,sum_d2_60to75_mfyc,sum_d2_75to100_mfyc,avgr_d2_mfyc,sdr_d2_mfyc)
write.xlsx(d2_matchfirstycmatrix,"Analytics/OTHERLEAGUES/MatchFirstYCanalysis.xlsx", sheetName = "D2", append = TRUE)
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#E1
E1_matchfirstyc <- subset(OTHERLEAGUES_analytics, Div == "E1")

e1_matchfirstyc_h <- tapply(E1_matchfirstyc$Home_first_YCTime, E1_matchfirstyc[c("HomeTeam", "Date")],mean)
e1_matchfirstyc_a <- tapply(E1_matchfirstyc$Away_first_YCTime, E1_matchfirstyc[c("AwayTeam", "Date")],mean)

e1_matchfirstyc_h[is.na(e1_matchfirstyc_h)] <- ""
e1_matchfirstyc_a[is.na(e1_matchfirstyc_a)] <- ""

for(e1_rowhyc in 1:nrow(e1_matchfirstyc_h)) {
  for(e1_colhyc in 1:ncol(e1_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(e1_rowayc in 1:nrow(e1_matchfirstyc_a)) {
      for(e1_colayc in 1:ncol(e1_matchfirstyc_a)) {
        ifelse(!e1_matchfirstyc_a[e1_rowayc,e1_colayc]=="",e1_matchfirstyc_h[e1_rowayc,e1_colayc] <- e1_matchfirstyc_a[e1_rowayc,e1_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

e1_home_games <- c()
e1_away_games <-c()

for (i_e1 in 1:length(e1_teams))
{

  e1_home_games[i_e1] <- nrow(E1_matchfirstyc[E1_matchfirstyc$HomeTeam == e1_teams[i_e1],])
  e1_away_games[i_e1]  <- nrow(E1_matchfirstyc[E1_matchfirstyc$AwayTeam == e1_teams[i_e1],])
}
e1_games_played <- e1_home_games + e1_away_games
e1_last_n_games <- e1_games_played[1]

e1_totalrounds <-  (length(e1_teams) - 1 )*2

final_e1_mfyc <- matrix(nrow = length(e1_teams),ncol = e1_totalrounds )
suml6_e1_mfyc <- c()
sum_e1_zero_mfyc <- c()
sum_e1_1to15_mfyc <- c()
sum_e1_15to30_mfyc <- c()
sum_e1_30to45_mfyc <- c()
sum_e1_45to60_mfyc <- c()
sum_e1_60to75_mfyc <- c()
sum_e1_75to100_mfyc <- c()
avgr_e1_mfyc <- c()
sdr_e1_mfyc <- c()
l6_form_e1_mfycsplitted <- c()
form_e1_mfyc <- c()
for(index_e1_mfyc in 1:length(e1_teams))
{
  for(index_e1_mfyc_cols in 1:e1_totalrounds)
  {
    index_e1_mfyc  <- row.names(e1_matchfirstyc_h) == e1_teams[index_e1_mfyc]
    form_e1_mfyc <- e1_matchfirstyc_h[index_e1_mfyc ]
    deleted_form_e1_mfyc <- form_e1_mfyc[!form_e1_mfyc[] == ""]
    l6_form_e1_mfyc <- tail(deleted_form_e1_mfyc,e1_last_n_games)
    l6_form_e1_mfyc <- as.numeric(l6_form_e1_mfyc)
    suml6_e1_mfyc[index_e1_mfyc] <- sum(l6_form_e1_mfyc)
    suml6_e1_mfyc[index_e1_mfyc] <- paste(suml6_e1_mfyc[index_e1_mfyc],sep = "")
    sum_e1_zero_mfyc[index_e1_mfyc] <- length(which(l6_form_e1_mfyc == 0))
    sum_e1_zero_mfyc[index_e1_mfyc] <- paste(sum_e1_zero_mfyc[index_e1_mfyc],sep = "")
    sum_e1_1to15_mfyc[index_e1_mfyc] <- length(which(l6_form_e1_mfyc >= 1 & l6_form_e1_mfyc <= 15))
    sum_e1_1to15_mfyc[index_e1_mfyc] <- paste(sum_e1_1to15_mfyc[index_e1_mfyc],sep = "")
    sum_e1_15to30_mfyc[index_e1_mfyc] <- length(which(l6_form_e1_mfyc >= 15 & l6_form_e1_mfyc <= 30))
    sum_e1_15to30_mfyc[index_e1_mfyc] <- paste(sum_e1_15to30_mfyc[index_e1_mfyc],sep = "")
    sum_e1_30to45_mfyc[index_e1_mfyc] <- length(which(l6_form_e1_mfyc >= 30 & l6_form_e1_mfyc <= 45))
    sum_e1_30to45_mfyc[index_e1_mfyc] <- paste(sum_e1_30to45_mfyc[index_e1_mfyc],sep = "")
    sum_e1_45to60_mfyc[index_e1_mfyc] <- length(which(l6_form_e1_mfyc >= 45 & l6_form_e1_mfyc <= 60))
    sum_e1_45to60_mfyc[index_e1_mfyc] <- paste(sum_e1_45to60_mfyc[index_e1_mfyc],sep = "")
    sum_e1_60to75_mfyc[index_e1_mfyc] <- length(which(l6_form_e1_mfyc >= 60 & l6_form_e1_mfyc <= 75))
    sum_e1_60to75_mfyc[index_e1_mfyc] <- paste(sum_e1_60to75_mfyc[index_e1_mfyc],sep = "")
    sum_e1_75to100_mfyc[index_e1_mfyc] <- length(which(l6_form_e1_mfyc >= 75 & l6_form_e1_mfyc <= 100))
    sum_e1_75to100_mfyc[index_e1_mfyc] <- paste(sum_e1_75to100_mfyc[index_e1_mfyc],sep = "")
    avgr_e1_mfyc[index_e1_mfyc] <- mean(l6_form_e1_mfyc)
    avgr_e1_mfyc[index_e1_mfyc] <- paste(avgr_e1_mfyc[index_e1_mfyc],sep = "")
    sdr_e1_mfyc[index_e1_mfyc] <- sd(l6_form_e1_mfyc)
    sdr_e1_mfyc[index_e1_mfyc] <- paste(sdr_e1_mfyc[index_e1_mfyc],sep = "")
    l6_form_e1_mfyc <- as.character(l6_form_e1_mfyc)
    #l6_form_e1_mfyc_flattened <- stri_paste(l6_form_e1_mfyc,collapse = '')
    #l6_form_e1_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_e1_mfyc_flattened),"")[[1]])
    final_e1_mfyc[index_e1_mfyc,index_e1_mfyc_cols] <- l6_form_e1_mfyc[index_e1_mfyc_cols]
  }
}

final_e1_mfyc[is.na(final_e1_mfyc)] <- ""
e1_matchfirstycmatrix <- cbind(e1_teams,final_e1_mfyc,suml6_e1_mfyc,sum_e1_zero_mfyc,sum_e1_1to15_mfyc,sum_e1_15to30_mfyc,sum_e1_30to45_mfyc,sum_e1_45to60_mfyc,sum_e1_60to75_mfyc,sum_e1_75to100_mfyc,avgr_e1_mfyc,sdr_e1_mfyc)
write.xlsx(e1_matchfirstycmatrix,"Analytics/OTHERLEAGUES/MatchFirstYCanalysis.xlsx", sheetName = "E1", append = TRUE)
################################################################################################################################################################################
################################################################################################################################################################################
#SC0
SC0_matchfirstyc <- readxl::read_excel('../Rsoccer/SC0_SPREAD.xlsx')
SC0_matchfirstyc <- SC0_matchfirstyc[,c(-1)]

sc0_matchfirstyc_h <- tapply(SC0_matchfirstyc$Home_first_YCTime, SC0_matchfirstyc[c("HomeTeam", "Date")],mean)
sc0_matchfirstyc_a <- tapply(SC0_matchfirstyc$Away_first_YCTime, SC0_matchfirstyc[c("AwayTeam", "Date")],mean)

sc0_matchfirstyc_h[is.na(sc0_matchfirstyc_h)] <- ""
sc0_matchfirstyc_a[is.na(sc0_matchfirstyc_a)] <- ""

for(sc0_rowhyc in 1:nrow(sc0_matchfirstyc_h)) {
  for(sc0_colhyc in 1:ncol(sc0_matchfirstyc_h)) {

    # print(my_matrix[row, col])
    for(sc0_rowayc in 1:nrow(sc0_matchfirstyc_a)) {
      for(sc0_colayc in 1:ncol(sc0_matchfirstyc_a)) {
        ifelse(!sc0_matchfirstyc_a[sc0_rowayc,sc0_colayc]=="",sc0_matchfirstyc_h[sc0_rowayc,sc0_colayc] <- sc0_matchfirstyc_a[sc0_rowayc,sc0_colayc],next)
        #print(my_matrix[row, col])
      }
    }

  }
}

sc0_home_games <- c()
sc0_away_games <-c()

for (i_sc0 in 1:length(sc0_teams))
{

  sc0_home_games[i_sc0] <- nrow(SC0_matchfirstyc[SC0_matchfirstyc$HomeTeam == sc0_teams[i_sc0],])
  sc0_away_games[i_sc0]  <- nrow(SC0_matchfirstyc[SC0_matchfirstyc$AwayTeam == sc0_teams[i_sc0],])
}
sc0_games_played <- sc0_home_games + sc0_away_games
sc0_last_n_games <- sc0_games_played[1]

sc0_totalrounds <-  (length(sc0_teams) - 1 )*2

final_sc0_mfyc <- matrix(nrow = length(sc0_teams),ncol = sc0_totalrounds )
suml6_sc0_mfyc <- c()
sum_sc0_zero_mfyc <- c()
sum_sc0_1to15_mfyc <- c()
sum_sc0_15to30_mfyc <- c()
sum_sc0_30to45_mfyc <- c()
sum_sc0_45to60_mfyc <- c()
sum_sc0_60to75_mfyc <- c()
sum_sc0_75to100_mfyc <- c()
avgr_sc0_mfyc <- c()
sdr_sc0_mfyc <- c()
l6_form_sc0_mfycsplitted <- c()
form_sc0_mfyc <- c()
for(index_sc0_mfyc in 1:length(sc0_teams))
{
  for(index_sc0_mfyc_cols in 1:sc0_totalrounds)
  {
    index_sc0_mfyc  <- row.names(sc0_matchfirstyc_h) == sc0_teams[index_sc0_mfyc]
    form_sc0_mfyc <- sc0_matchfirstyc_h[index_sc0_mfyc ]
    deleted_form_sc0_mfyc <- form_sc0_mfyc[!form_sc0_mfyc[] == ""]
    l6_form_sc0_mfyc <- tail(deleted_form_sc0_mfyc,sc0_last_n_games)
    l6_form_sc0_mfyc <- as.numeric(l6_form_sc0_mfyc)
    suml6_sc0_mfyc[index_sc0_mfyc] <- sum(l6_form_sc0_mfyc)
    suml6_sc0_mfyc[index_sc0_mfyc] <- paste(suml6_sc0_mfyc[index_sc0_mfyc],sep = "")
    sum_sc0_zero_mfyc[index_sc0_mfyc] <- length(which(l6_form_sc0_mfyc == 0))
    sum_sc0_zero_mfyc[index_sc0_mfyc] <- paste(sum_sc0_zero_mfyc[index_sc0_mfyc],sep = "")
    sum_sc0_1to15_mfyc[index_sc0_mfyc] <- length(which(l6_form_sc0_mfyc >= 1 & l6_form_sc0_mfyc <= 15))
    sum_sc0_1to15_mfyc[index_sc0_mfyc] <- paste(sum_sc0_1to15_mfyc[index_sc0_mfyc],sep = "")
    sum_sc0_15to30_mfyc[index_sc0_mfyc] <- length(which(l6_form_sc0_mfyc >= 15 & l6_form_sc0_mfyc <= 30))
    sum_sc0_15to30_mfyc[index_sc0_mfyc] <- paste(sum_sc0_15to30_mfyc[index_sc0_mfyc],sep = "")
    sum_sc0_30to45_mfyc[index_sc0_mfyc] <- length(which(l6_form_sc0_mfyc >= 30 & l6_form_sc0_mfyc <= 45))
    sum_sc0_30to45_mfyc[index_sc0_mfyc] <- paste(sum_sc0_30to45_mfyc[index_sc0_mfyc],sep = "")
    sum_sc0_45to60_mfyc[index_sc0_mfyc] <- length(which(l6_form_sc0_mfyc >= 45 & l6_form_sc0_mfyc <= 60))
    sum_sc0_45to60_mfyc[index_sc0_mfyc] <- paste(sum_sc0_45to60_mfyc[index_sc0_mfyc],sep = "")
    sum_sc0_60to75_mfyc[index_sc0_mfyc] <- length(which(l6_form_sc0_mfyc >= 60 & l6_form_sc0_mfyc <= 75))
    sum_sc0_60to75_mfyc[index_sc0_mfyc] <- paste(sum_sc0_60to75_mfyc[index_sc0_mfyc],sep = "")
    sum_sc0_75to100_mfyc[index_sc0_mfyc] <- length(which(l6_form_sc0_mfyc >= 75 & l6_form_sc0_mfyc <= 100))
    sum_sc0_75to100_mfyc[index_sc0_mfyc] <- paste(sum_sc0_75to100_mfyc[index_sc0_mfyc],sep = "")
    avgr_sc0_mfyc[index_sc0_mfyc] <- mean(l6_form_sc0_mfyc)
    avgr_sc0_mfyc[index_sc0_mfyc] <- paste(avgr_sc0_mfyc[index_sc0_mfyc],sep = "")
    sdr_sc0_mfyc[index_sc0_mfyc] <- sd(l6_form_sc0_mfyc)
    sdr_sc0_mfyc[index_sc0_mfyc] <- paste(sdr_sc0_mfyc[index_sc0_mfyc],sep = "")
    l6_form_sc0_mfyc <- as.character(l6_form_sc0_mfyc)
    #l6_form_sc0_mfyc_flattened <- stri_paste(l6_form_sc0_mfyc,collapse = '')
    #l6_form_sc0_mfycsplitted <- as.numeric(strsplit(as.character(l6_form_sc0_mfyc_flattened),"")[[1]])
    final_sc0_mfyc[index_sc0_mfyc,index_sc0_mfyc_cols] <- l6_form_sc0_mfyc[index_sc0_mfyc_cols]
  }
}

final_sc0_mfyc[is.na(final_sc0_mfyc)] <- ""
sc0_matchfirstycmatrix <- cbind(sc0_teams,final_sc0_mfyc,suml6_sc0_mfyc,sum_sc0_zero_mfyc,sum_sc0_1to15_mfyc,sum_sc0_15to30_mfyc,sum_sc0_30to45_mfyc,sum_sc0_45to60_mfyc,sum_sc0_60to75_mfyc,sum_sc0_75to100_mfyc,avgr_sc0_mfyc,sdr_sc0_mfyc)
write.xlsx(sc0_matchfirstycmatrix,"Analytics/OTHERLEAGUES/MatchFirstYCanalysis.xlsx", sheetName = "SC0", append = TRUE)

