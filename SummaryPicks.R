library('xlsx')
#E0
advstatsn <- 6
E0_advstats <- readxl::read_excel('EPL_FINALSPREAD.xlsx')
E0_advstats <- E0_advstats[,-1]

unlink('Summaries/E0/*')
for(e0_sn in 1:19){
  df <- tail(E0_advstats[E0_advstats$HomeTeam == final_doublefixture_e0[e0_sn,1] | E0_advstats$AwayTeam == final_doublefixture_e0[e0_sn,1] ,],advstatsn)

  df2 <- tail(E0_advstats[E0_advstats$HomeTeam == final_doublefixture_e0[e0_sn + 1,1] | E0_advstats$AwayTeam == final_doublefixture_e0[e0_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\E0"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_e0[e0_sn,1],final_doublefixture_e0[e0_sn + 1,1],"adv.xlsx",sep = "_")))

}
#
#D1
advstatsn <- 6
D1_advstats <- readxl::read_excel('BUNDES_FINALSPREAD.xlsx')
D1_advstats <- D1_advstats[,-1]
unlink('Summaries/D1/*')
for(d1_sn in 1:17){
  df <- tail(D1_advstats[D1_advstats$HomeTeam == final_doublefixture_d1[d1_sn,1] | D1_advstats$AwayTeam == final_doublefixture_d1[d1_sn,1] ,],advstatsn)

  df2 <- tail(D1_advstats[D1_advstats$HomeTeam == final_doublefixture_d1[d1_sn + 1,1] | D1_advstats$AwayTeam == final_doublefixture_d1[d1_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\D1"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_d1[d1_sn,1],final_doublefixture_d1[d1_sn + 1,1],"adv.xlsx",sep = "_")))

}
##
#I1
advstatsn <- 6
I1_advstats <- readxl::read_excel('SERIEA_FINALSPREAD.xlsx')
I1_advstats <- I1_advstats[,-1]
unlink('Summaries/I1/*')
for(i1_sn in 1:19){
  df <- tail(I1_advstats[I1_advstats$HomeTeam == final_doublefixture_i1[i1_sn,1] | I1_advstats$AwayTeam == final_doublefixture_i1[i1_sn,1] ,],advstatsn)

  df2 <- tail(I1_advstats[I1_advstats$HomeTeam == final_doublefixture_i1[i1_sn + 1,1] | I1_advstats$AwayTeam == final_doublefixture_i1[i1_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\I1"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_i1[i1_sn,1],final_doublefixture_i1[i1_sn + 1,1],"adv.xlsx",sep = "_")))

}
#
#SP1
#SP1
advstatsn <- 6
SP1_advstats <- readxl::read_excel('LALIGA_FINALSPREAD.xlsx')
SP1_advstats <- SP1_advstats[,-1]
unlink('Summaries/SP1/*')
for(sp1_sn in 1:19){
  df <- tail(SP1_advstats[SP1_advstats$HomeTeam == final_doublefixture_sp1[sp1_sn,1] | SP1_advstats$AwayTeam == final_doublefixture_sp1[sp1_sn,1] ,],advstatsn)

  df2 <- tail(SP1_advstats[SP1_advstats$HomeTeam == final_doublefixture_sp1[sp1_sn + 1,1] | SP1_advstats$AwayTeam == final_doublefixture_sp1[sp1_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\SP1"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_sp1[sp1_sn,1],final_doublefixture_sp1[sp1_sn + 1,1],"adv.xlsx",sep = "_")))

}
#
#F1
#E0
advstatsn <- 6
F1_advstats <- readxl::read_excel('LIGUEONE_FINALSPREAD.xlsx')
F1_advstats <- F1_advstats[,-1]
unlink('Summaries/F1/*')
for(f1_sn in 1:19){
  df <- tail(F1_advstats[F1_advstats$HomeTeam == final_doublefixture_f1[f1_sn,1] | F1_advstats$AwayTeam == final_doublefixture_f1[f1_sn,1] ,],advstatsn)

  df2 <- tail(F1_advstats[F1_advstats$HomeTeam == final_doublefixture_f1[f1_sn + 1,1] | F1_advstats$AwayTeam == final_doublefixture_f1[f1_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\F1"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_f1[f1_sn,1],final_doublefixture_f1[f1_sn + 1,1],"adv.xlsx",sep = "_")))

}


##################################################
#different leagues union
##################################################

advstatsn <- 6
D1_advstats <- readxl::read_excel('BUNDES_SPREAD.xlsx')
D1_advstats <- D1_advstats[,-1]
I1_advstats <- readxl::read_excel('SERIEA_SPREAD.xlsx')
I1_advstats <- I1_advstats[,-1]
E0_advstats <- readxl::read_excel('EPL_SPREAD.xlsx')
E0_advstats <- E0_advstats[,-1]

UCL_advstats <- rbind(D1_advstats,I1_advstats,E0_advstats)

df <- tail(UCL_advstats[UCL_advstats$HomeTeam == "Man United" | UCL_advstats$AwayTeam == "Man United",],advstatsn)

df2 <- tail(UCL_advstats[UCL_advstats$HomeTeam == "Man City" | UCL_advstats$AwayTeam == "Man City",],advstatsn)

temp_analysis <- rbind(df,df2)

temp_analysis <- as.data.frame(temp_analysis)
temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67)])
temp_sliced <- tail(temp_analysis,1)
temp_sliced <- temp_sliced[1:38]

temp_analyis_combined <- c(temp_sliced,temp_colmeans)
temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

write.xlsx(temp_analysis,"Temp/manunitedVcity.xlsx")










