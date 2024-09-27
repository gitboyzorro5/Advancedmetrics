library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')

BIGFIVE_analytics <- readxl::read_excel('BIGFIVE20232024.xlsx')
BIGFIVE_analytics <- BIGFIVE_analytics[,-1]
BIGFIVE_analytics <- as.data.frame(BIGFIVE_analytics)

#E0
E0_matchfirstyc <- subset(BIGFIVE_analytics, Div == "E0")

e0_GP <- nrow(E0_matchfirstyc)

e0_T_HFYC <- sum(E0_matchfirstyc$Home_first_YCTime)

e0_T_AFYC <- sum(E0_matchfirstyc$Away_first_YCTime)

e0_avg_HFYC <- round(e0_T_HFYC /e0_GP, digits = 4)

e0_avg_AFYC <- round(e0_T_AFYC/e0_GP, digits = 4)

e0_home_games <- c()
e0_away_games <- c()
for (i_e0 in 1:length(e0_teams))
{

  e0_home_games[i_e0] <- nrow(E0_matchfirstyc[E0_matchfirstyc$HomeTeam == e0_teams[i_e0],])
  e0_away_games[i_e0]  <- nrow(E0_matchfirstyc[E0_matchfirstyc$AwayTeam == e0_teams[i_e0],])

}

e0_home_fhyc <- aggregate(E0_matchfirstyc$Home_first_YCTime, by = list(E0_matchfirstyc$HomeTeam), FUN = sum)
e0_away_fayc <- aggregate(E0_matchfirstyc$Away_first_YCTime, by = list(E0_matchfirstyc$AwayTeam), FUN = sum)


e0_home_fycas <- round(((e0_home_fhyc$x/e0_home_games))/e0_avg_HFYC, digits = 4)

e0_away_fycas <- round(((e0_away_fayc$x/e0_away_games))/e0_avg_AFYC, digits = 4)

e0_avg_HFYCC <- round(e0_T_AFYC /e0_GP, digits = 4)

e0_avg_AFYCC <- round(e0_T_HFYC /e0_GP, digits = 4)

e0_home_fycc <- aggregate(E0_matchfirstyc$Away_first_YCTime, by = list(E0_matchfirstyc$HomeTeam), FUN = sum)
e0_away_fycc <- aggregate(E0_matchfirstyc$Home_first_YCTime, by = list(E0_matchfirstyc$AwayTeam), FUN = sum)
e0_home_fycds <- round(((e0_home_fycc$x/e0_home_games))/e0_avg_HFYC, digits = 4)
e0_away_fycds <- round(((e0_away_fycc$x/e0_away_games))/e0_avg_AFYC, digits = 4)

e0_division <- c()
e0_division[1:length(e0_teams)] <- "E0"
e0_home_poisson_fyc <- cbind(e0_division,e0_teams,e0_avg_HFYC,e0_home_fycas,e0_home_fycds)

e0_division <- c()
e0_division[1:length(e0_teams)] <- "E0"
e0_away_poisson_fyc <- cbind(e0_division,e0_teams,e0_avg_AFYC,e0_away_fycas,e0_away_fycds)

HomeTeam_e0_yc <- rep(e0_teams, each = length(e0_teams))
AwayTeam_e0_yc <- rep(e0_teams, length(e0_teams))
E0_matchfirstyc_fixtures_yc <- cbind(HomeTeam_e0_yc,AwayTeam_e0_yc)
E0_matchfirstyc_fixtures_yc <- as.data.frame(E0_matchfirstyc_fixtures_yc)
E0_matchfirstyc_fixtures_yc <- E0_matchfirstyc_fixtures_yc[!E0_matchfirstyc_fixtures_yc$HomeTeam_e0_yc == E0_matchfirstyc_fixtures_yc$AwayTeam_e0_yc,]
rownames(E0_matchfirstyc_fixtures_yc) <- NULL
E0_matchfirstyc_fixtures_yc$Div <- "E0"
E0_matchfirstyc_fixtures_yc <- E0_matchfirstyc_fixtures_yc[,c(3,1,2)]

E0_matchfirstyc_fixtures_yc$avg_HY_e0 <- e0_avg_HFYC

E0_matchfirstyc_fixtures_yc$e0_homeyas <- rep(e0_home_fycas,each = length(e0_teams)-1)

e0_awayyds_lookup <- cbind(e0_teams,e0_away_fycds)

e0_awayyds_lookup <- as.data.frame(e0_awayyds_lookup)

colnames(e0_awayyds_lookup) <- c("AwayTeam_e0_yc","e0_awayyds")


require('RH2')
E0_matchfirstyc_fixtures_yc$e0_awayyds <- sqldf("SELECT e0_awayyds_lookup.e0_awayyds FROM e0_awayyds_lookup INNER JOIN E0_matchfirstyc_fixtures_yc ON e0_awayyds_lookup.AwayTeam_e0_yc = E0_matchfirstyc_fixtures_yc.AwayTeam_e0_yc")

E0_matchfirstyc_fixtures_yc$avg_AY_e0 <- e0_avg_AFYC

e0_awayyas_lookup <- cbind(e0_teams,e0_away_fycas)

e0_awayyas_lookup <- as.data.frame(e0_awayyas_lookup)

colnames(e0_awayyas_lookup) <- c("AwayTeam_e0_yc","e0_awayyas")

E0_matchfirstyc_fixtures_yc$e0_awayyas <- sqldf("SELECT e0_awayyas_lookup.e0_awayyas FROM e0_awayyas_lookup INNER JOIN E0_matchfirstyc_fixtures_yc ON e0_awayyas_lookup.AwayTeam_e0_yc = E0_matchfirstyc_fixtures_yc.AwayTeam_e0_yc")

E0_matchfirstyc_fixtures_yc$e0_homeyds <- rep(e0_home_fycds,each = length(e0_teams)-1)

E0_matchfirstyc_fixtures_yc$e0_awayyds <- as.numeric(unlist(E0_matchfirstyc_fixtures_yc$e0_awayyds))
#xGH
E0_matchfirstyc_fixtures_yc$e0_xHYC <- E0_matchfirstyc_fixtures_yc$avg_HY_e0 * E0_matchfirstyc_fixtures_yc$e0_homeyas * E0_matchfirstyc_fixtures_yc$e0_awayyds
#xGA

E0_matchfirstyc_fixtures_yc$e0_awayyas <- as.numeric(unlist(E0_matchfirstyc_fixtures_yc$e0_awayyas))

E0_matchfirstyc_fixtures_yc$e0_xAYC <- E0_matchfirstyc_fixtures_yc$avg_AY_e0 * E0_matchfirstyc_fixtures_yc$e0_awayyas * E0_matchfirstyc_fixtures_yc$e0_homeyds

unlink('Analytics/BIGFIVE/PoissonFirstYCanalysis.xlsx')
write.xlsx(E0_matchfirstyc_fixtures_yc,"Analytics/BIGFIVE/PoissonFirstYCanalysis.xlsx", sheetName = "E0")
###################################################################################################################################################################################
###################################################################################################################################################################################
#D1

D1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "D1")

d1_GP <- nrow(D1_matchfirstyc)

d1_T_HFYC <- sum(D1_matchfirstyc$Home_first_YCTime)

d1_T_AFYC <- sum(D1_matchfirstyc$Away_first_YCTime)

d1_avg_HFYC <- round(d1_T_HFYC /d1_GP, digits = 4)

d1_avg_AFYC <- round(d1_T_AFYC/d1_GP, digits = 4)

d1_home_games <- c()
d1_away_games <- c()
for (i_d1 in 1:length(d1_teams))
{

  d1_home_games[i_d1] <- nrow(D1_matchfirstyc[D1_matchfirstyc$HomeTeam == d1_teams[i_d1],])
  d1_away_games[i_d1]  <- nrow(D1_matchfirstyc[D1_matchfirstyc$AwayTeam == d1_teams[i_d1],])

}

d1_home_fhyc <- aggregate(D1_matchfirstyc$Home_first_YCTime, by = list(D1_matchfirstyc$HomeTeam), FUN = sum)
d1_away_fayc <- aggregate(D1_matchfirstyc$Away_first_YCTime, by = list(D1_matchfirstyc$AwayTeam), FUN = sum)


d1_home_fycas <- round(((d1_home_fhyc$x/d1_home_games))/d1_avg_HFYC, digits = 4)

d1_away_fycas <- round(((d1_away_fayc$x/d1_away_games))/d1_avg_AFYC, digits = 4)

d1_avg_HFYCC <- round(d1_T_AFYC /d1_GP, digits = 4)

d1_avg_AFYCC <- round(d1_T_HFYC /d1_GP, digits = 4)

d1_home_fycc <- aggregate(D1_matchfirstyc$Away_first_YCTime, by = list(D1_matchfirstyc$HomeTeam), FUN = sum)
d1_away_fycc <- aggregate(D1_matchfirstyc$Home_first_YCTime, by = list(D1_matchfirstyc$AwayTeam), FUN = sum)
d1_home_fycds <- round(((d1_home_fycc$x/d1_home_games))/d1_avg_HFYC, digits = 4)
d1_away_fycds <- round(((d1_away_fycc$x/d1_away_games))/d1_avg_AFYC, digits = 4)

d1_division <- c()
d1_division[1:length(d1_teams)] <- "D1"
d1_home_poisson_fyc <- cbind(d1_division,d1_teams,d1_avg_HFYC,d1_home_fycas,d1_home_fycds)

d1_division <- c()
d1_division[1:length(d1_teams)] <- "D1"
d1_away_poisson_fyc <- cbind(d1_division,d1_teams,d1_avg_AFYC,d1_away_fycas,d1_away_fycds)

HomeTeam_d1_yc <- rep(d1_teams, each = length(d1_teams))
AwayTeam_d1_yc <- rep(d1_teams, length(d1_teams))
D1_matchfirstyc_fixtures_yc <- cbind(HomeTeam_d1_yc,AwayTeam_d1_yc)
D1_matchfirstyc_fixtures_yc <- as.data.frame(D1_matchfirstyc_fixtures_yc)
D1_matchfirstyc_fixtures_yc <- D1_matchfirstyc_fixtures_yc[!D1_matchfirstyc_fixtures_yc$HomeTeam_d1_yc == D1_matchfirstyc_fixtures_yc$AwayTeam_d1_yc,]
rownames(D1_matchfirstyc_fixtures_yc) <- NULL
D1_matchfirstyc_fixtures_yc$Div <- "D1"
D1_matchfirstyc_fixtures_yc <- D1_matchfirstyc_fixtures_yc[,c(3,1,2)]

D1_matchfirstyc_fixtures_yc$avg_HY_d1 <- d1_avg_HFYC

D1_matchfirstyc_fixtures_yc$d1_homeyas <- rep(d1_home_fycas,each = length(d1_teams)-1)

d1_awayyds_lookup <- cbind(d1_teams,d1_away_fycds)

d1_awayyds_lookup <- as.data.frame(d1_awayyds_lookup)

colnames(d1_awayyds_lookup) <- c("AwayTeam_d1_yc","d1_awayyds")


require('RH2')
D1_matchfirstyc_fixtures_yc$d1_awayyds <- sqldf("SELECT d1_awayyds_lookup.d1_awayyds FROM d1_awayyds_lookup INNER JOIN D1_matchfirstyc_fixtures_yc ON d1_awayyds_lookup.AwayTeam_d1_yc = D1_matchfirstyc_fixtures_yc.AwayTeam_d1_yc")

D1_matchfirstyc_fixtures_yc$avg_AY_d1 <- d1_avg_AFYC

d1_awayyas_lookup <- cbind(d1_teams,d1_away_fycas)

d1_awayyas_lookup <- as.data.frame(d1_awayyas_lookup)

colnames(d1_awayyas_lookup) <- c("AwayTeam_d1_yc","d1_awayyas")

D1_matchfirstyc_fixtures_yc$d1_awayyas <- sqldf("SELECT d1_awayyas_lookup.d1_awayyas FROM d1_awayyas_lookup INNER JOIN D1_matchfirstyc_fixtures_yc ON d1_awayyas_lookup.AwayTeam_d1_yc = D1_matchfirstyc_fixtures_yc.AwayTeam_d1_yc")

D1_matchfirstyc_fixtures_yc$d1_homeyds <- rep(d1_home_fycds,each = length(d1_teams)-1)

D1_matchfirstyc_fixtures_yc$d1_awayyds <- as.numeric(unlist(D1_matchfirstyc_fixtures_yc$d1_awayyds))
#xGH
D1_matchfirstyc_fixtures_yc$d1_xHYC <- D1_matchfirstyc_fixtures_yc$avg_HY_d1 * D1_matchfirstyc_fixtures_yc$d1_homeyas * D1_matchfirstyc_fixtures_yc$d1_awayyds
#xGA

D1_matchfirstyc_fixtures_yc$d1_awayyas <- as.numeric(unlist(D1_matchfirstyc_fixtures_yc$d1_awayyas))

D1_matchfirstyc_fixtures_yc$d1_xAYC <- D1_matchfirstyc_fixtures_yc$avg_AY_d1 * D1_matchfirstyc_fixtures_yc$d1_awayyas * D1_matchfirstyc_fixtures_yc$d1_homeyds

write.xlsx(D1_matchfirstyc_fixtures_yc,"Analytics/BIGFIVE/PoissonFirstYCanalysis.xlsx", sheetName = "D1", append = TRUE)
######################################################################################################################################################################
######################################################################################################################################################################
#I1

I1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "I1")

i1_GP <- nrow(I1_matchfirstyc)

i1_T_HFYC <- sum(I1_matchfirstyc$Home_first_YCTime)

i1_T_AFYC <- sum(I1_matchfirstyc$Away_first_YCTime)

i1_avg_HFYC <- round(i1_T_HFYC /i1_GP, digits = 4)

i1_avg_AFYC <- round(i1_T_AFYC/i1_GP, digits = 4)

i1_home_games <- c()
i1_away_games <- c()
for (i_i1 in 1:length(i1_teams))
{

  i1_home_games[i_i1] <- nrow(I1_matchfirstyc[I1_matchfirstyc$HomeTeam == i1_teams[i_i1],])
  i1_away_games[i_i1]  <- nrow(I1_matchfirstyc[I1_matchfirstyc$AwayTeam == i1_teams[i_i1],])

}

i1_home_fhyc <- aggregate(I1_matchfirstyc$Home_first_YCTime, by = list(I1_matchfirstyc$HomeTeam), FUN = sum)
i1_away_fayc <- aggregate(I1_matchfirstyc$Away_first_YCTime, by = list(I1_matchfirstyc$AwayTeam), FUN = sum)


i1_home_fycas <- round(((i1_home_fhyc$x/i1_home_games))/i1_avg_HFYC, digits = 4)

i1_away_fycas <- round(((i1_away_fayc$x/i1_away_games))/i1_avg_AFYC, digits = 4)

i1_avg_HFYCC <- round(i1_T_AFYC /i1_GP, digits = 4)

i1_avg_AFYCC <- round(i1_T_HFYC /i1_GP, digits = 4)

i1_home_fycc <- aggregate(I1_matchfirstyc$Away_first_YCTime, by = list(I1_matchfirstyc$HomeTeam), FUN = sum)
i1_away_fycc <- aggregate(I1_matchfirstyc$Home_first_YCTime, by = list(I1_matchfirstyc$AwayTeam), FUN = sum)
i1_home_fycds <- round(((i1_home_fycc$x/i1_home_games))/i1_avg_HFYC, digits = 4)
i1_away_fycds <- round(((i1_away_fycc$x/i1_away_games))/i1_avg_AFYC, digits = 4)

i1_division <- c()
i1_division[1:length(i1_teams)] <- "I1"
i1_home_poisson_fyc <- cbind(i1_division,i1_teams,i1_avg_HFYC,i1_home_fycas,i1_home_fycds)

i1_division <- c()
i1_division[1:length(i1_teams)] <- "I1"
i1_away_poisson_fyc <- cbind(i1_division,i1_teams,i1_avg_AFYC,i1_away_fycas,i1_away_fycds)

HomeTeam_i1_yc <- rep(i1_teams, each = length(i1_teams))
AwayTeam_i1_yc <- rep(i1_teams, length(i1_teams))
I1_matchfirstyc_fixtures_yc <- cbind(HomeTeam_i1_yc,AwayTeam_i1_yc)
I1_matchfirstyc_fixtures_yc <- as.data.frame(I1_matchfirstyc_fixtures_yc)
I1_matchfirstyc_fixtures_yc <- I1_matchfirstyc_fixtures_yc[!I1_matchfirstyc_fixtures_yc$HomeTeam_i1_yc == I1_matchfirstyc_fixtures_yc$AwayTeam_i1_yc,]
rownames(I1_matchfirstyc_fixtures_yc) <- NULL
I1_matchfirstyc_fixtures_yc$Div <- "I1"
I1_matchfirstyc_fixtures_yc <- I1_matchfirstyc_fixtures_yc[,c(3,1,2)]

I1_matchfirstyc_fixtures_yc$avg_HY_i1 <- i1_avg_HFYC

I1_matchfirstyc_fixtures_yc$i1_homeyas <- rep(i1_home_fycas,each = length(i1_teams)-1)

i1_awayyds_lookup <- cbind(i1_teams,i1_away_fycds)

i1_awayyds_lookup <- as.data.frame(i1_awayyds_lookup)

colnames(i1_awayyds_lookup) <- c("AwayTeam_i1_yc","i1_awayyds")


require('RH2')
I1_matchfirstyc_fixtures_yc$i1_awayyds <- sqldf("SELECT i1_awayyds_lookup.i1_awayyds FROM i1_awayyds_lookup INNER JOIN I1_matchfirstyc_fixtures_yc ON i1_awayyds_lookup.AwayTeam_i1_yc = I1_matchfirstyc_fixtures_yc.AwayTeam_i1_yc")

I1_matchfirstyc_fixtures_yc$avg_AY_i1 <- i1_avg_AFYC

i1_awayyas_lookup <- cbind(i1_teams,i1_away_fycas)

i1_awayyas_lookup <- as.data.frame(i1_awayyas_lookup)

colnames(i1_awayyas_lookup) <- c("AwayTeam_i1_yc","i1_awayyas")

I1_matchfirstyc_fixtures_yc$i1_awayyas <- sqldf("SELECT i1_awayyas_lookup.i1_awayyas FROM i1_awayyas_lookup INNER JOIN I1_matchfirstyc_fixtures_yc ON i1_awayyas_lookup.AwayTeam_i1_yc = I1_matchfirstyc_fixtures_yc.AwayTeam_i1_yc")

I1_matchfirstyc_fixtures_yc$i1_homeyds <- rep(i1_home_fycds,each = length(i1_teams)-1)

I1_matchfirstyc_fixtures_yc$i1_awayyds <- as.numeric(unlist(I1_matchfirstyc_fixtures_yc$i1_awayyds))
#xGH
I1_matchfirstyc_fixtures_yc$i1_xHYC <- I1_matchfirstyc_fixtures_yc$avg_HY_i1 * I1_matchfirstyc_fixtures_yc$i1_homeyas * I1_matchfirstyc_fixtures_yc$i1_awayyds
#xGA

I1_matchfirstyc_fixtures_yc$i1_awayyas <- as.numeric(unlist(I1_matchfirstyc_fixtures_yc$i1_awayyas))

I1_matchfirstyc_fixtures_yc$i1_xAYC <- I1_matchfirstyc_fixtures_yc$avg_AY_i1 * I1_matchfirstyc_fixtures_yc$i1_awayyas * I1_matchfirstyc_fixtures_yc$i1_homeyds

write.xlsx(I1_matchfirstyc_fixtures_yc,"Analytics/BIGFIVE/PoissonFirstYCanalysis.xlsx", sheetName = "I1", append = TRUE)
#######################################################################################################################################################################
#######################################################################################################################################################################
#SP1
SP1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "SP1")

sp1_GP <- nrow(SP1_matchfirstyc)

sp1_T_HFYC <- sum(SP1_matchfirstyc$Home_first_YCTime)

sp1_T_AFYC <- sum(SP1_matchfirstyc$Away_first_YCTime)

sp1_avg_HFYC <- round(sp1_T_HFYC /sp1_GP, digits = 4)

sp1_avg_AFYC <- round(sp1_T_AFYC/sp1_GP, digits = 4)

sp1_home_games <- c()
sp1_away_games <- c()
for (i_sp1 in 1:length(sp1_teams))
{

  sp1_home_games[i_sp1] <- nrow(SP1_matchfirstyc[SP1_matchfirstyc$HomeTeam == sp1_teams[i_sp1],])
  sp1_away_games[i_sp1]  <- nrow(SP1_matchfirstyc[SP1_matchfirstyc$AwayTeam == sp1_teams[i_sp1],])

}

sp1_home_fhyc <- aggregate(SP1_matchfirstyc$Home_first_YCTime, by = list(SP1_matchfirstyc$HomeTeam), FUN = sum)
sp1_away_fayc <- aggregate(SP1_matchfirstyc$Away_first_YCTime, by = list(SP1_matchfirstyc$AwayTeam), FUN = sum)


sp1_home_fycas <- round(((sp1_home_fhyc$x/sp1_home_games))/sp1_avg_HFYC, digits = 4)

sp1_away_fycas <- round(((sp1_away_fayc$x/sp1_away_games))/sp1_avg_AFYC, digits = 4)

sp1_avg_HFYCC <- round(sp1_T_AFYC /sp1_GP, digits = 4)

sp1_avg_AFYCC <- round(sp1_T_HFYC /sp1_GP, digits = 4)

sp1_home_fycc <- aggregate(SP1_matchfirstyc$Away_first_YCTime, by = list(SP1_matchfirstyc$HomeTeam), FUN = sum)
sp1_away_fycc <- aggregate(SP1_matchfirstyc$Home_first_YCTime, by = list(SP1_matchfirstyc$AwayTeam), FUN = sum)
sp1_home_fycds <- round(((sp1_home_fycc$x/sp1_home_games))/sp1_avg_HFYC, digits = 4)
sp1_away_fycds <- round(((sp1_away_fycc$x/sp1_away_games))/sp1_avg_AFYC, digits = 4)

sp1_division <- c()
sp1_division[1:length(sp1_teams)] <- "SP1"
sp1_home_poisson_fyc <- cbind(sp1_division,sp1_teams,sp1_avg_HFYC,sp1_home_fycas,sp1_home_fycds)

sp1_division <- c()
sp1_division[1:length(sp1_teams)] <- "SP1"
sp1_away_poisson_fyc <- cbind(sp1_division,sp1_teams,sp1_avg_AFYC,sp1_away_fycas,sp1_away_fycds)

HomeTeam_sp1_yc <- rep(sp1_teams, each = length(sp1_teams))
AwayTeam_sp1_yc <- rep(sp1_teams, length(sp1_teams))
SP1_matchfirstyc_fixtures_yc <- cbind(HomeTeam_sp1_yc,AwayTeam_sp1_yc)
SP1_matchfirstyc_fixtures_yc <- as.data.frame(SP1_matchfirstyc_fixtures_yc)
SP1_matchfirstyc_fixtures_yc <- SP1_matchfirstyc_fixtures_yc[!SP1_matchfirstyc_fixtures_yc$HomeTeam_sp1_yc == SP1_matchfirstyc_fixtures_yc$AwayTeam_sp1_yc,]
rownames(SP1_matchfirstyc_fixtures_yc) <- NULL
SP1_matchfirstyc_fixtures_yc$Div <- "SP1"
SP1_matchfirstyc_fixtures_yc <- SP1_matchfirstyc_fixtures_yc[,c(3,1,2)]

SP1_matchfirstyc_fixtures_yc$avg_HY_sp1 <- sp1_avg_HFYC

SP1_matchfirstyc_fixtures_yc$sp1_homeyas <- rep(sp1_home_fycas,each = length(sp1_teams)-1)

sp1_awayyds_lookup <- cbind(sp1_teams,sp1_away_fycds)

sp1_awayyds_lookup <- as.data.frame(sp1_awayyds_lookup)

colnames(sp1_awayyds_lookup) <- c("AwayTeam_sp1_yc","sp1_awayyds")


require('RH2')
SP1_matchfirstyc_fixtures_yc$sp1_awayyds <- sqldf("SELECT sp1_awayyds_lookup.sp1_awayyds FROM sp1_awayyds_lookup INNER JOIN SP1_matchfirstyc_fixtures_yc ON sp1_awayyds_lookup.AwayTeam_sp1_yc = SP1_matchfirstyc_fixtures_yc.AwayTeam_sp1_yc")

SP1_matchfirstyc_fixtures_yc$avg_AY_sp1 <- sp1_avg_AFYC

sp1_awayyas_lookup <- cbind(sp1_teams,sp1_away_fycas)

sp1_awayyas_lookup <- as.data.frame(sp1_awayyas_lookup)

colnames(sp1_awayyas_lookup) <- c("AwayTeam_sp1_yc","sp1_awayyas")

SP1_matchfirstyc_fixtures_yc$sp1_awayyas <- sqldf("SELECT sp1_awayyas_lookup.sp1_awayyas FROM sp1_awayyas_lookup INNER JOIN SP1_matchfirstyc_fixtures_yc ON sp1_awayyas_lookup.AwayTeam_sp1_yc = SP1_matchfirstyc_fixtures_yc.AwayTeam_sp1_yc")

SP1_matchfirstyc_fixtures_yc$sp1_homeyds <- rep(sp1_home_fycds,each = length(sp1_teams)-1)

SP1_matchfirstyc_fixtures_yc$sp1_awayyds <- as.numeric(unlist(SP1_matchfirstyc_fixtures_yc$sp1_awayyds))
#xGH
SP1_matchfirstyc_fixtures_yc$sp1_xHYC <- SP1_matchfirstyc_fixtures_yc$avg_HY_sp1 * SP1_matchfirstyc_fixtures_yc$sp1_homeyas * SP1_matchfirstyc_fixtures_yc$sp1_awayyds
#xGA

SP1_matchfirstyc_fixtures_yc$sp1_awayyas <- as.numeric(unlist(SP1_matchfirstyc_fixtures_yc$sp1_awayyas))

SP1_matchfirstyc_fixtures_yc$sp1_xAYC <- SP1_matchfirstyc_fixtures_yc$avg_AY_sp1 * SP1_matchfirstyc_fixtures_yc$sp1_awayyas * SP1_matchfirstyc_fixtures_yc$sp1_homeyds


write.xlsx(SP1_matchfirstyc_fixtures_yc,"Analytics/BIGFIVE/PoissonFirstYCanalysis.xlsx", sheetName = "SP1", append = TRUE)
#############################################################################################################################################################################
#############################################################################################################################################################################

F1_matchfirstyc <- subset(BIGFIVE_analytics, Div == "F1")

f1_GP <- nrow(F1_matchfirstyc)

f1_T_HFYC <- sum(F1_matchfirstyc$Home_first_YCTime)

f1_T_AFYC <- sum(F1_matchfirstyc$Away_first_YCTime)

f1_avg_HFYC <- round(f1_T_HFYC /f1_GP, digits = 4)

f1_avg_AFYC <- round(f1_T_AFYC/f1_GP, digits = 4)

f1_home_games <- c()
f1_away_games <- c()
for (i_f1 in 1:length(f1_teams))
{

  f1_home_games[i_f1] <- nrow(F1_matchfirstyc[F1_matchfirstyc$HomeTeam == f1_teams[i_f1],])
  f1_away_games[i_f1]  <- nrow(F1_matchfirstyc[F1_matchfirstyc$AwayTeam == f1_teams[i_f1],])

}

f1_home_fhyc <- aggregate(F1_matchfirstyc$Home_first_YCTime, by = list(F1_matchfirstyc$HomeTeam), FUN = sum)
f1_away_fayc <- aggregate(F1_matchfirstyc$Away_first_YCTime, by = list(F1_matchfirstyc$AwayTeam), FUN = sum)


f1_home_fycas <- round(((f1_home_fhyc$x/f1_home_games))/f1_avg_HFYC, digits = 4)

f1_away_fycas <- round(((f1_away_fayc$x/f1_away_games))/f1_avg_AFYC, digits = 4)

f1_avg_HFYCC <- round(f1_T_AFYC /f1_GP, digits = 4)

f1_avg_AFYCC <- round(f1_T_HFYC /f1_GP, digits = 4)

f1_home_fycc <- aggregate(F1_matchfirstyc$Away_first_YCTime, by = list(F1_matchfirstyc$HomeTeam), FUN = sum)
f1_away_fycc <- aggregate(F1_matchfirstyc$Home_first_YCTime, by = list(F1_matchfirstyc$AwayTeam), FUN = sum)
f1_home_fycds <- round(((f1_home_fycc$x/f1_home_games))/f1_avg_HFYC, digits = 4)
f1_away_fycds <- round(((f1_away_fycc$x/f1_away_games))/f1_avg_AFYC, digits = 4)

f1_division <- c()
f1_division[1:length(f1_teams)] <- "F1"
f1_home_poisson_fyc <- cbind(f1_division,f1_teams,f1_avg_HFYC,f1_home_fycas,f1_home_fycds)

f1_division <- c()
f1_division[1:length(f1_teams)] <- "F1"
f1_away_poisson_fyc <- cbind(f1_division,f1_teams,f1_avg_AFYC,f1_away_fycas,f1_away_fycds)

HomeTeam_f1_yc <- rep(f1_teams, each = length(f1_teams))
AwayTeam_f1_yc <- rep(f1_teams, length(f1_teams))
F1_matchfirstyc_fixtures_yc <- cbind(HomeTeam_f1_yc,AwayTeam_f1_yc)
F1_matchfirstyc_fixtures_yc <- as.data.frame(F1_matchfirstyc_fixtures_yc)
F1_matchfirstyc_fixtures_yc <- F1_matchfirstyc_fixtures_yc[!F1_matchfirstyc_fixtures_yc$HomeTeam_f1_yc == F1_matchfirstyc_fixtures_yc$AwayTeam_f1_yc,]
rownames(F1_matchfirstyc_fixtures_yc) <- NULL
F1_matchfirstyc_fixtures_yc$Div <- "F1"
F1_matchfirstyc_fixtures_yc <- F1_matchfirstyc_fixtures_yc[,c(3,1,2)]

F1_matchfirstyc_fixtures_yc$avg_HY_f1 <- f1_avg_HFYC

F1_matchfirstyc_fixtures_yc$f1_homeyas <- rep(f1_home_fycas,each = length(f1_teams)-1)

f1_awayyds_lookup <- cbind(f1_teams,f1_away_fycds)

f1_awayyds_lookup <- as.data.frame(f1_awayyds_lookup)

colnames(f1_awayyds_lookup) <- c("AwayTeam_f1_yc","f1_awayyds")


require('RH2')
F1_matchfirstyc_fixtures_yc$f1_awayyds <- sqldf("SELECT f1_awayyds_lookup.f1_awayyds FROM f1_awayyds_lookup INNER JOIN F1_matchfirstyc_fixtures_yc ON f1_awayyds_lookup.AwayTeam_f1_yc = F1_matchfirstyc_fixtures_yc.AwayTeam_f1_yc")

F1_matchfirstyc_fixtures_yc$avg_AY_f1 <- f1_avg_AFYC

f1_awayyas_lookup <- cbind(f1_teams,f1_away_fycas)

f1_awayyas_lookup <- as.data.frame(f1_awayyas_lookup)

colnames(f1_awayyas_lookup) <- c("AwayTeam_f1_yc","f1_awayyas")

F1_matchfirstyc_fixtures_yc$f1_awayyas <- sqldf("SELECT f1_awayyas_lookup.f1_awayyas FROM f1_awayyas_lookup INNER JOIN F1_matchfirstyc_fixtures_yc ON f1_awayyas_lookup.AwayTeam_f1_yc = F1_matchfirstyc_fixtures_yc.AwayTeam_f1_yc")

F1_matchfirstyc_fixtures_yc$f1_homeyds <- rep(f1_home_fycds,each = length(f1_teams)-1)

F1_matchfirstyc_fixtures_yc$f1_awayyds <- as.numeric(unlist(F1_matchfirstyc_fixtures_yc$f1_awayyds))
#xGH
F1_matchfirstyc_fixtures_yc$f1_xHYC <- F1_matchfirstyc_fixtures_yc$avg_HY_f1 * F1_matchfirstyc_fixtures_yc$f1_homeyas * F1_matchfirstyc_fixtures_yc$f1_awayyds
#xGA

F1_matchfirstyc_fixtures_yc$f1_awayyas <- as.numeric(unlist(F1_matchfirstyc_fixtures_yc$f1_awayyas))

F1_matchfirstyc_fixtures_yc$f1_xAYC <- F1_matchfirstyc_fixtures_yc$avg_AY_f1 * F1_matchfirstyc_fixtures_yc$f1_awayyas * F1_matchfirstyc_fixtures_yc$f1_homeyds


write.xlsx(F1_matchfirstyc_fixtures_yc,"Analytics/BIGFIVE/PoissonFirstYCanalysis.xlsx", sheetName = "F1", append = TRUE)
####################################################################################################################################################################################
####################################################################################################################################################################################
###############
#OTHER LEAGUES#
###############
OTHERLEAGUES_analytics <- readxl::read_excel('OTHERLEAGUES20232024.xlsx')
OTHERLEAGUES_analytics <- OTHERLEAGUES_analytics[,-1]
OTHERLEAGUES_analytics <- as.data.frame(OTHERLEAGUES_analytics)

#B1
B1_matchfirstyc <- subset(OTHERLEAGUES_analytics, Div == "B1")

b1_GP <- nrow(B1_matchfirstyc)

b1_T_HFYC <- sum(B1_matchfirstyc$Home_first_YCTime)

b1_T_AFYC <- sum(B1_matchfirstyc$Away_first_YCTime)

b1_avg_HFYC <- round(b1_T_HFYC /b1_GP, digits = 4)

b1_avg_AFYC <- round(b1_T_AFYC/b1_GP, digits = 4)

b1_home_games <- c()
b1_away_games <- c()
for (i_b1 in 1:length(b1_teams))
{

  b1_home_games[i_b1] <- nrow(B1_matchfirstyc[B1_matchfirstyc$HomeTeam == b1_teams[i_b1],])
  b1_away_games[i_b1]  <- nrow(B1_matchfirstyc[B1_matchfirstyc$AwayTeam == b1_teams[i_b1],])

}

b1_home_fhyc <- aggregate(B1_matchfirstyc$Home_first_YCTime, by = list(B1_matchfirstyc$HomeTeam), FUN = sum)
b1_away_fayc <- aggregate(B1_matchfirstyc$Away_first_YCTime, by = list(B1_matchfirstyc$AwayTeam), FUN = sum)


b1_home_fycas <- round(((b1_home_fhyc$x/b1_home_games))/b1_avg_HFYC, digits = 4)

b1_away_fycas <- round(((b1_away_fayc$x/b1_away_games))/b1_avg_AFYC, digits = 4)

b1_avg_HFYCC <- round(b1_T_AFYC /b1_GP, digits = 4)

b1_avg_AFYCC <- round(b1_T_HFYC /b1_GP, digits = 4)

b1_home_fycc <- aggregate(B1_matchfirstyc$Away_first_YCTime, by = list(B1_matchfirstyc$HomeTeam), FUN = sum)
b1_away_fycc <- aggregate(B1_matchfirstyc$Home_first_YCTime, by = list(B1_matchfirstyc$AwayTeam), FUN = sum)
b1_home_fycds <- round(((b1_home_fycc$x/b1_home_games))/b1_avg_HFYC, digits = 4)
b1_away_fycds <- round(((b1_away_fycc$x/b1_away_games))/b1_avg_AFYC, digits = 4)

b1_division <- c()
b1_division[1:length(b1_teams)] <- "B1"
b1_home_poisson_fyc <- cbind(b1_division,b1_teams,b1_avg_HFYC,b1_home_fycas,b1_home_fycds)

b1_division <- c()
b1_division[1:length(b1_teams)] <- "B1"
b1_away_poisson_fyc <- cbind(b1_division,b1_teams,b1_avg_AFYC,b1_away_fycas,b1_away_fycds)

HomeTeam_b1_yc <- rep(b1_teams, each = length(b1_teams))
AwayTeam_b1_yc <- rep(b1_teams, length(b1_teams))
B1_matchfirstyc_fixtures_yc <- cbind(HomeTeam_b1_yc,AwayTeam_b1_yc)
B1_matchfirstyc_fixtures_yc <- as.data.frame(B1_matchfirstyc_fixtures_yc)
B1_matchfirstyc_fixtures_yc <- B1_matchfirstyc_fixtures_yc[!B1_matchfirstyc_fixtures_yc$HomeTeam_b1_yc == B1_matchfirstyc_fixtures_yc$AwayTeam_b1_yc,]
rownames(B1_matchfirstyc_fixtures_yc) <- NULL
B1_matchfirstyc_fixtures_yc$Div <- "B1"
B1_matchfirstyc_fixtures_yc <- B1_matchfirstyc_fixtures_yc[,c(3,1,2)]

B1_matchfirstyc_fixtures_yc$avg_HY_b1 <- b1_avg_HFYC

B1_matchfirstyc_fixtures_yc$b1_homeyas <- rep(b1_home_fycas,each = length(b1_teams)-1)

b1_awayyds_lookup <- cbind(b1_teams,b1_away_fycds)

b1_awayyds_lookup <- as.data.frame(b1_awayyds_lookup)

colnames(b1_awayyds_lookup) <- c("AwayTeam_b1_yc","b1_awayyds")


require('RH2')
B1_matchfirstyc_fixtures_yc$b1_awayyds <- sqldf("SELECT b1_awayyds_lookup.b1_awayyds FROM b1_awayyds_lookup INNER JOIN B1_matchfirstyc_fixtures_yc ON b1_awayyds_lookup.AwayTeam_b1_yc = B1_matchfirstyc_fixtures_yc.AwayTeam_b1_yc")

B1_matchfirstyc_fixtures_yc$avg_AY_b1 <- b1_avg_AFYC

b1_awayyas_lookup <- cbind(b1_teams,b1_away_fycas)

b1_awayyas_lookup <- as.data.frame(b1_awayyas_lookup)

colnames(b1_awayyas_lookup) <- c("AwayTeam_b1_yc","b1_awayyas")

B1_matchfirstyc_fixtures_yc$b1_awayyas <- sqldf("SELECT b1_awayyas_lookup.b1_awayyas FROM b1_awayyas_lookup INNER JOIN B1_matchfirstyc_fixtures_yc ON b1_awayyas_lookup.AwayTeam_b1_yc = B1_matchfirstyc_fixtures_yc.AwayTeam_b1_yc")

B1_matchfirstyc_fixtures_yc$b1_homeyds <- rep(b1_home_fycds,each = length(b1_teams)-1)

B1_matchfirstyc_fixtures_yc$b1_awayyds <- as.numeric(unlist(B1_matchfirstyc_fixtures_yc$b1_awayyds))
#xGH
B1_matchfirstyc_fixtures_yc$b1_xHYC <- B1_matchfirstyc_fixtures_yc$avg_HY_b1 * B1_matchfirstyc_fixtures_yc$b1_homeyas * B1_matchfirstyc_fixtures_yc$b1_awayyds
#xGA

B1_matchfirstyc_fixtures_yc$b1_awayyas <- as.numeric(unlist(B1_matchfirstyc_fixtures_yc$b1_awayyas))

B1_matchfirstyc_fixtures_yc$b1_xAYC <- B1_matchfirstyc_fixtures_yc$avg_AY_b1 * B1_matchfirstyc_fixtures_yc$b1_awayyas * B1_matchfirstyc_fixtures_yc$b1_homeyds

unlink('Analytics/OTHERLEAGUES/PoissonFirstYCanalysis.xlsx')
write.xlsx(B1_matchfirstyc_fixtures_yc,"Analytics/OTHERLEAGUES/PoissonFirstYCanalysis.xlsx", sheetName = "B1")
######################################################################################################################################################################
######################################################################################################################################################################
#D2
D2_matchfirstyc <- subset(OTHERLEAGUES_analytics, Div == "D2")

d2_GP <- nrow(D2_matchfirstyc)

d2_T_HFYC <- sum(D2_matchfirstyc$Home_first_YCTime)

d2_T_AFYC <- sum(D2_matchfirstyc$Away_first_YCTime)

d2_avg_HFYC <- round(d2_T_HFYC /d2_GP, digits = 4)

d2_avg_AFYC <- round(d2_T_AFYC/d2_GP, digits = 4)

d2_home_games <- c()
d2_away_games <- c()
for (i_d2 in 1:length(d2_teams))
{

  d2_home_games[i_d2] <- nrow(D2_matchfirstyc[D2_matchfirstyc$HomeTeam == d2_teams[i_d2],])
  d2_away_games[i_d2]  <- nrow(D2_matchfirstyc[D2_matchfirstyc$AwayTeam == d2_teams[i_d2],])

}

d2_home_fhyc <- aggregate(D2_matchfirstyc$Home_first_YCTime, by = list(D2_matchfirstyc$HomeTeam), FUN = sum)
d2_away_fayc <- aggregate(D2_matchfirstyc$Away_first_YCTime, by = list(D2_matchfirstyc$AwayTeam), FUN = sum)


d2_home_fycas <- round(((d2_home_fhyc$x/d2_home_games))/d2_avg_HFYC, digits = 4)

d2_away_fycas <- round(((d2_away_fayc$x/d2_away_games))/d2_avg_AFYC, digits = 4)

d2_avg_HFYCC <- round(d2_T_AFYC /d2_GP, digits = 4)

d2_avg_AFYCC <- round(d2_T_HFYC /d2_GP, digits = 4)

d2_home_fycc <- aggregate(D2_matchfirstyc$Away_first_YCTime, by = list(D2_matchfirstyc$HomeTeam), FUN = sum)
d2_away_fycc <- aggregate(D2_matchfirstyc$Home_first_YCTime, by = list(D2_matchfirstyc$AwayTeam), FUN = sum)
d2_home_fycds <- round(((d2_home_fycc$x/d2_home_games))/d2_avg_HFYC, digits = 4)
d2_away_fycds <- round(((d2_away_fycc$x/d2_away_games))/d2_avg_AFYC, digits = 4)

d2_division <- c()
d2_division[1:length(d2_teams)] <- "D2"
d2_home_poisson_fyc <- cbind(d2_division,d2_teams,d2_avg_HFYC,d2_home_fycas,d2_home_fycds)

d2_division <- c()
d2_division[1:length(d2_teams)] <- "D2"
d2_away_poisson_fyc <- cbind(d2_division,d2_teams,d2_avg_AFYC,d2_away_fycas,d2_away_fycds)

HomeTeam_d2_yc <- rep(d2_teams, each = length(d2_teams))
AwayTeam_d2_yc <- rep(d2_teams, length(d2_teams))
D2_matchfirstyc_fixtures_yc <- cbind(HomeTeam_d2_yc,AwayTeam_d2_yc)
D2_matchfirstyc_fixtures_yc <- as.data.frame(D2_matchfirstyc_fixtures_yc)
D2_matchfirstyc_fixtures_yc <- D2_matchfirstyc_fixtures_yc[!D2_matchfirstyc_fixtures_yc$HomeTeam_d2_yc == D2_matchfirstyc_fixtures_yc$AwayTeam_d2_yc,]
rownames(D2_matchfirstyc_fixtures_yc) <- NULL
D2_matchfirstyc_fixtures_yc$Div <- "D2"
D2_matchfirstyc_fixtures_yc <- D2_matchfirstyc_fixtures_yc[,c(3,1,2)]

D2_matchfirstyc_fixtures_yc$avg_HY_d2 <- d2_avg_HFYC

D2_matchfirstyc_fixtures_yc$d2_homeyas <- rep(d2_home_fycas,each = length(d2_teams)-1)

d2_awayyds_lookup <- cbind(d2_teams,d2_away_fycds)

d2_awayyds_lookup <- as.data.frame(d2_awayyds_lookup)

colnames(d2_awayyds_lookup) <- c("AwayTeam_d2_yc","d2_awayyds")


require('RH2')
D2_matchfirstyc_fixtures_yc$d2_awayyds <- sqldf("SELECT d2_awayyds_lookup.d2_awayyds FROM d2_awayyds_lookup INNER JOIN D2_matchfirstyc_fixtures_yc ON d2_awayyds_lookup.AwayTeam_d2_yc = D2_matchfirstyc_fixtures_yc.AwayTeam_d2_yc")

D2_matchfirstyc_fixtures_yc$avg_AY_d2 <- d2_avg_AFYC

d2_awayyas_lookup <- cbind(d2_teams,d2_away_fycas)

d2_awayyas_lookup <- as.data.frame(d2_awayyas_lookup)

colnames(d2_awayyas_lookup) <- c("AwayTeam_d2_yc","d2_awayyas")

D2_matchfirstyc_fixtures_yc$d2_awayyas <- sqldf("SELECT d2_awayyas_lookup.d2_awayyas FROM d2_awayyas_lookup INNER JOIN D2_matchfirstyc_fixtures_yc ON d2_awayyas_lookup.AwayTeam_d2_yc = D2_matchfirstyc_fixtures_yc.AwayTeam_d2_yc")

D2_matchfirstyc_fixtures_yc$d2_homeyds <- rep(d2_home_fycds,each = length(d2_teams)-1)

D2_matchfirstyc_fixtures_yc$d2_awayyds <- as.numeric(unlist(D2_matchfirstyc_fixtures_yc$d2_awayyds))
#xGH
D2_matchfirstyc_fixtures_yc$d2_xHYC <- D2_matchfirstyc_fixtures_yc$avg_HY_d2 * D2_matchfirstyc_fixtures_yc$d2_homeyas * D2_matchfirstyc_fixtures_yc$d2_awayyds
#xGA

D2_matchfirstyc_fixtures_yc$d2_awayyas <- as.numeric(unlist(D2_matchfirstyc_fixtures_yc$d2_awayyas))

D2_matchfirstyc_fixtures_yc$d2_xAYC <- D2_matchfirstyc_fixtures_yc$avg_AY_d2 * D2_matchfirstyc_fixtures_yc$d2_awayyas * D2_matchfirstyc_fixtures_yc$d2_homeyds

write.xlsx(D2_matchfirstyc_fixtures_yc,"Analytics/OTHERLEAGUES/PoissonFirstYCanalysis.xlsx", sheetName = "D2", append = TRUE)
###########################################################################################################################################################################
###########################################################################################################################################################################
#E1
E1_matchfirstyc <- subset(OTHERLEAGUES_analytics, Div == "E1")

e1_GP <- nrow(E1_matchfirstyc)

e1_T_HFYC <- sum(E1_matchfirstyc$Home_first_YCTime)

e1_T_AFYC <- sum(E1_matchfirstyc$Away_first_YCTime)

e1_avg_HFYC <- round(e1_T_HFYC /e1_GP, digits = 4)

e1_avg_AFYC <- round(e1_T_AFYC/e1_GP, digits = 4)

e1_home_games <- c()
e1_away_games <- c()
for (i_e1 in 1:length(e1_teams))
{

  e1_home_games[i_e1] <- nrow(E1_matchfirstyc[E1_matchfirstyc$HomeTeam == e1_teams[i_e1],])
  e1_away_games[i_e1]  <- nrow(E1_matchfirstyc[E1_matchfirstyc$AwayTeam == e1_teams[i_e1],])

}

e1_home_fhyc <- aggregate(E1_matchfirstyc$Home_first_YCTime, by = list(E1_matchfirstyc$HomeTeam), FUN = sum)
e1_away_fayc <- aggregate(E1_matchfirstyc$Away_first_YCTime, by = list(E1_matchfirstyc$AwayTeam), FUN = sum)


e1_home_fycas <- round(((e1_home_fhyc$x/e1_home_games))/e1_avg_HFYC, digits = 4)

e1_away_fycas <- round(((e1_away_fayc$x/e1_away_games))/e1_avg_AFYC, digits = 4)

e1_avg_HFYCC <- round(e1_T_AFYC /e1_GP, digits = 4)

e1_avg_AFYCC <- round(e1_T_HFYC /e1_GP, digits = 4)

e1_home_fycc <- aggregate(E1_matchfirstyc$Away_first_YCTime, by = list(E1_matchfirstyc$HomeTeam), FUN = sum)
e1_away_fycc <- aggregate(E1_matchfirstyc$Home_first_YCTime, by = list(E1_matchfirstyc$AwayTeam), FUN = sum)
e1_home_fycds <- round(((e1_home_fycc$x/e1_home_games))/e1_avg_HFYC, digits = 4)
e1_away_fycds <- round(((e1_away_fycc$x/e1_away_games))/e1_avg_AFYC, digits = 4)

e1_division <- c()
e1_division[1:length(e1_teams)] <- "E1"
e1_home_poisson_fyc <- cbind(e1_division,e1_teams,e1_avg_HFYC,e1_home_fycas,e1_home_fycds)

e1_division <- c()
e1_division[1:length(e1_teams)] <- "E1"
e1_away_poisson_fyc <- cbind(e1_division,e1_teams,e1_avg_AFYC,e1_away_fycas,e1_away_fycds)

HomeTeam_e1_yc <- rep(e1_teams, each = length(e1_teams))
AwayTeam_e1_yc <- rep(e1_teams, length(e1_teams))
E1_matchfirstyc_fixtures_yc <- cbind(HomeTeam_e1_yc,AwayTeam_e1_yc)
E1_matchfirstyc_fixtures_yc <- as.data.frame(E1_matchfirstyc_fixtures_yc)
E1_matchfirstyc_fixtures_yc <- E1_matchfirstyc_fixtures_yc[!E1_matchfirstyc_fixtures_yc$HomeTeam_e1_yc == E1_matchfirstyc_fixtures_yc$AwayTeam_e1_yc,]
rownames(E1_matchfirstyc_fixtures_yc) <- NULL
E1_matchfirstyc_fixtures_yc$Div <- "E1"
E1_matchfirstyc_fixtures_yc <- E1_matchfirstyc_fixtures_yc[,c(3,1,2)]

E1_matchfirstyc_fixtures_yc$avg_HY_e1 <- e1_avg_HFYC

E1_matchfirstyc_fixtures_yc$e1_homeyas <- rep(e1_home_fycas,each = length(e1_teams)-1)

e1_awayyds_lookup <- cbind(e1_teams,e1_away_fycds)

e1_awayyds_lookup <- as.data.frame(e1_awayyds_lookup)

colnames(e1_awayyds_lookup) <- c("AwayTeam_e1_yc","e1_awayyds")


require('RH2')
E1_matchfirstyc_fixtures_yc$e1_awayyds <- sqldf("SELECT e1_awayyds_lookup.e1_awayyds FROM e1_awayyds_lookup INNER JOIN E1_matchfirstyc_fixtures_yc ON e1_awayyds_lookup.AwayTeam_e1_yc = E1_matchfirstyc_fixtures_yc.AwayTeam_e1_yc")

E1_matchfirstyc_fixtures_yc$avg_AY_e1 <- e1_avg_AFYC

e1_awayyas_lookup <- cbind(e1_teams,e1_away_fycas)

e1_awayyas_lookup <- as.data.frame(e1_awayyas_lookup)

colnames(e1_awayyas_lookup) <- c("AwayTeam_e1_yc","e1_awayyas")

E1_matchfirstyc_fixtures_yc$e1_awayyas <- sqldf("SELECT e1_awayyas_lookup.e1_awayyas FROM e1_awayyas_lookup INNER JOIN E1_matchfirstyc_fixtures_yc ON e1_awayyas_lookup.AwayTeam_e1_yc = E1_matchfirstyc_fixtures_yc.AwayTeam_e1_yc")

E1_matchfirstyc_fixtures_yc$e1_homeyds <- rep(e1_home_fycds,each = length(e1_teams)-1)

E1_matchfirstyc_fixtures_yc$e1_awayyds <- as.numeric(unlist(E1_matchfirstyc_fixtures_yc$e1_awayyds))
#xGH
E1_matchfirstyc_fixtures_yc$e1_xHYC <- E1_matchfirstyc_fixtures_yc$avg_HY_e1 * E1_matchfirstyc_fixtures_yc$e1_homeyas * E1_matchfirstyc_fixtures_yc$e1_awayyds
#xGA

E1_matchfirstyc_fixtures_yc$e1_awayyas <- as.numeric(unlist(E1_matchfirstyc_fixtures_yc$e1_awayyas))

E1_matchfirstyc_fixtures_yc$e1_xAYC <- E1_matchfirstyc_fixtures_yc$avg_AY_e1 * E1_matchfirstyc_fixtures_yc$e1_awayyas * E1_matchfirstyc_fixtures_yc$e1_homeyds

write.xlsx(E1_matchfirstyc_fixtures_yc,"Analytics/OTHERLEAGUES/PoissonFirstYCanalysis.xlsx", sheetName = "E1", append = TRUE)
##############################################################################################################################################################################
##############################################################################################################################################################################
#SC2

SC0_matchfirstyc <- readxl::read_excel('../Rsoccer/SC0_SPREAD.xlsx')
SC0_matchfirstyc <- SC0_matchfirstyc[,c(-1)]


sc0_GP <- nrow(SC0_matchfirstyc)

sc0_T_HFYC <- sum(SC0_matchfirstyc$Home_first_YCTime)

sc0_T_AFYC <- sum(SC0_matchfirstyc$Away_first_YCTime)

sc0_avg_HFYC <- round(sc0_T_HFYC /sc0_GP, digits = 4)

sc0_avg_AFYC <- round(sc0_T_AFYC/sc0_GP, digits = 4)

sc0_home_games <- c()
sc0_away_games <- c()
for (i_sc0 in 1:length(sc0_teams))
{

  sc0_home_games[i_sc0] <- nrow(SC0_matchfirstyc[SC0_matchfirstyc$HomeTeam == sc0_teams[i_sc0],])
  sc0_away_games[i_sc0]  <- nrow(SC0_matchfirstyc[SC0_matchfirstyc$AwayTeam == sc0_teams[i_sc0],])

}

sc0_home_fhyc <- aggregate(SC0_matchfirstyc$Home_first_YCTime, by = list(SC0_matchfirstyc$HomeTeam), FUN = sum)
sc0_away_fayc <- aggregate(SC0_matchfirstyc$Away_first_YCTime, by = list(SC0_matchfirstyc$AwayTeam), FUN = sum)


sc0_home_fycas <- round(((sc0_home_fhyc$x/sc0_home_games))/sc0_avg_HFYC, digits = 4)

sc0_away_fycas <- round(((sc0_away_fayc$x/sc0_away_games))/sc0_avg_AFYC, digits = 4)

sc0_avg_HFYCC <- round(sc0_T_AFYC /sc0_GP, digits = 4)

sc0_avg_AFYCC <- round(sc0_T_HFYC /sc0_GP, digits = 4)

sc0_home_fycc <- aggregate(SC0_matchfirstyc$Away_first_YCTime, by = list(SC0_matchfirstyc$HomeTeam), FUN = sum)
sc0_away_fycc <- aggregate(SC0_matchfirstyc$Home_first_YCTime, by = list(SC0_matchfirstyc$AwayTeam), FUN = sum)
sc0_home_fycds <- round(((sc0_home_fycc$x/sc0_home_games))/sc0_avg_HFYC, digits = 4)
sc0_away_fycds <- round(((sc0_away_fycc$x/sc0_away_games))/sc0_avg_AFYC, digits = 4)

sc0_division <- c()
sc0_division[1:length(sc0_teams)] <- "SC0"
sc0_home_poisson_fyc <- cbind(sc0_division,sc0_teams,sc0_avg_HFYC,sc0_home_fycas,sc0_home_fycds)

sc0_division <- c()
sc0_division[1:length(sc0_teams)] <- "SC0"
sc0_away_poisson_fyc <- cbind(sc0_division,sc0_teams,sc0_avg_AFYC,sc0_away_fycas,sc0_away_fycds)

HomeTeam_sc0_yc <- rep(sc0_teams, each = length(sc0_teams))
AwayTeam_sc0_yc <- rep(sc0_teams, length(sc0_teams))
SC0_matchfirstyc_fixtures_yc <- cbind(HomeTeam_sc0_yc,AwayTeam_sc0_yc)
SC0_matchfirstyc_fixtures_yc <- as.data.frame(SC0_matchfirstyc_fixtures_yc)
SC0_matchfirstyc_fixtures_yc <- SC0_matchfirstyc_fixtures_yc[!SC0_matchfirstyc_fixtures_yc$HomeTeam_sc0_yc == SC0_matchfirstyc_fixtures_yc$AwayTeam_sc0_yc,]
rownames(SC0_matchfirstyc_fixtures_yc) <- NULL
SC0_matchfirstyc_fixtures_yc$Div <- "SC0"
SC0_matchfirstyc_fixtures_yc <- SC0_matchfirstyc_fixtures_yc[,c(3,1,2)]

SC0_matchfirstyc_fixtures_yc$avg_HY_sc0 <- sc0_avg_HFYC

SC0_matchfirstyc_fixtures_yc$sc0_homeyas <- rep(sc0_home_fycas,each = length(sc0_teams)-1)

sc0_awayyds_lookup <- cbind(sc0_teams,sc0_away_fycds)

sc0_awayyds_lookup <- as.data.frame(sc0_awayyds_lookup)

colnames(sc0_awayyds_lookup) <- c("AwayTeam_sc0_yc","sc0_awayyds")


require('RH2')
SC0_matchfirstyc_fixtures_yc$sc0_awayyds <- sqldf("SELECT sc0_awayyds_lookup.sc0_awayyds FROM sc0_awayyds_lookup INNER JOIN SC0_matchfirstyc_fixtures_yc ON sc0_awayyds_lookup.AwayTeam_sc0_yc = SC0_matchfirstyc_fixtures_yc.AwayTeam_sc0_yc")

SC0_matchfirstyc_fixtures_yc$avg_AY_sc0 <- sc0_avg_AFYC

sc0_awayyas_lookup <- cbind(sc0_teams,sc0_away_fycas)

sc0_awayyas_lookup <- as.data.frame(sc0_awayyas_lookup)

colnames(sc0_awayyas_lookup) <- c("AwayTeam_sc0_yc","sc0_awayyas")

SC0_matchfirstyc_fixtures_yc$sc0_awayyas <- sqldf("SELECT sc0_awayyas_lookup.sc0_awayyas FROM sc0_awayyas_lookup INNER JOIN SC0_matchfirstyc_fixtures_yc ON sc0_awayyas_lookup.AwayTeam_sc0_yc = SC0_matchfirstyc_fixtures_yc.AwayTeam_sc0_yc")

SC0_matchfirstyc_fixtures_yc$sc0_homeyds <- rep(sc0_home_fycds,each = length(sc0_teams)-1)

SC0_matchfirstyc_fixtures_yc$sc0_awayyds <- as.numeric(unlist(SC0_matchfirstyc_fixtures_yc$sc0_awayyds))
#xGH
SC0_matchfirstyc_fixtures_yc$sc0_xHYC <- SC0_matchfirstyc_fixtures_yc$avg_HY_sc0 * SC0_matchfirstyc_fixtures_yc$sc0_homeyas * SC0_matchfirstyc_fixtures_yc$sc0_awayyds
#xGA

SC0_matchfirstyc_fixtures_yc$sc0_awayyas <- as.numeric(unlist(SC0_matchfirstyc_fixtures_yc$sc0_awayyas))

SC0_matchfirstyc_fixtures_yc$sc0_xAYC <- SC0_matchfirstyc_fixtures_yc$avg_AY_sc0 * SC0_matchfirstyc_fixtures_yc$sc0_awayyas * SC0_matchfirstyc_fixtures_yc$sc0_homeyds

write.xlsx(SC0_matchfirstyc_fixtures_yc,"Analytics/OTHERLEAGUES/PoissonFirstYCanalysis.xlsx", sheetName = "SC0",append = TRUE)
