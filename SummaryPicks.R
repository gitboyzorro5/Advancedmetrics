library('xlsx')
#E0
advstatsn <- 6
E0_advstats <- readxl::read_excel('EPL_SPREAD.xlsx')
E0_advstats <- E0_advstats[,-1]
unlink('Summaries/E0/*')
for(e0_sn in 1:19){
  df <- tail(E0_advstats[E0_advstats$HomeTeam == final_doublefixture_e0[e0_sn,1] | E0_advstats$AwayTeam == final_doublefixture_e0[e0_sn,1] ,],advstatsn)

  df2 <- tail(E0_advstats[E0_advstats$HomeTeam == final_doublefixture_e0[e0_sn + 1,1] | E0_advstats$AwayTeam == final_doublefixture_e0[e0_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:38]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\E0"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_e0[e0_sn,1],final_doublefixture_e0[e0_sn + 1,1],"adv.xlsx",sep = "_")))

}
