library('xlsx')
#E0

unlink('Squads/E0/*')
for(e0_sn in 1:19){
  df <- eplsquad[eplsquad$Squad == final_doublefixture_e0[e0_sn,1],]

  df2 <- eplsquad[eplsquad$Squad == final_doublefixture_e0[e0_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\E0"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_e0[e0_sn,1],final_doublefixture_e0[e0_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads",append = TRUE)

}

#D1
unlink('Squads/D1/*')
for(d1_sn in 1:17){
  df <- bundesligasquad[bundesligasquad$Squad == final_doublefixture_d1[d1_sn,1],]

  df2 <- bundesligasquad[bundesligasquad$Squad == final_doublefixture_d1[d1_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\D1"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_d1[d1_sn,1],final_doublefixture_d1[d1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads",append = TRUE)

}

#I1
unlink('Squads/I1/*')
for(i1_sn in 1:19){
  df <- serieasquad[serieasquad$Squad == final_doublefixture_i1[i1_sn,1],]

  df2 <- serieasquad[serieasquad$Squad == final_doublefixture_i1[i1_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\I1"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_i1[i1_sn,1],final_doublefixture_i1[i1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads",append = TRUE)

}

#SP1

unlink('Squads/SP1/*')
for(sp1_sn in 1:19){
  df <- laligasquad[laligasquad$Squad == final_doublefixture_sp1[sp1_sn,1],]

  df2 <- laligasquad[laligasquad$Squad == final_doublefixture_sp1[sp1_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\SP1"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_sp1[sp1_sn,1],final_doublefixture_sp1[sp1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads",append = TRUE)

}

#F1
unlink('Squads/F1/*')
for(f1_sn in 1:17){
  df <- ligueonesquad[ligueonesquad$Squad == final_doublefixture_f1[f1_sn,1],]

  df2 <- ligueonesquad[ligueonesquad$Squad == final_doublefixture_f1[f1_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\F1"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_f1[f1_sn,1],final_doublefixture_f1[f1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads",append = TRUE)

}
########################################################################################
########################################################################################
#write the second part of the squad
#E0
for(e0_sn in 1:19){
  ef <- eplsquad2[eplsquad2$Squad == final_doublefixture_e0[e0_sn,1],]

  ef2 <- eplsquad2[eplsquad2$Squad == final_doublefixture_e0[e0_sn + 1,1],]

  temp_players2 <- rbind(ef,ef2)
  temp_players2 <- temp_players2[order(temp_players2$Gls, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\E0"
  write.xlsx(temp_players2,file.path(path,paste(final_doublefixture_e0[e0_sn,1],final_doublefixture_e0[e0_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squad2",append = TRUE)

}
########################################
#D1
for(d1_sn in 1:17){
  ef <- bundesligasquad2[bundesligasquad2$Squad == final_doublefixture_d1[d1_sn,1],]

  ef2 <- bundesligasquad2[bundesligasquad2$Squad == final_doublefixture_d1[d1_sn + 1,1],]

  temp_players2 <- rbind(ef,ef2)
  temp_players2 <- temp_players2[order(temp_players2$Gls, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\D1"
  write.xlsx(temp_players2,file.path(path,paste(final_doublefixture_d1[d1_sn,1],final_doublefixture_d1[d1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads2",append = TRUE)

}
########################################
#I1
for(i1_sn in 1:19){
  ef <- serieasquad2[serieasquad2$Squad == final_doublefixture_i1[i1_sn,1],]

  ef2 <- serieasquad2[serieasquad2$Squad == final_doublefixture_i1[i1_sn + 1,1],]

  temp_players2 <- rbind(ef,ef2)
  temp_players2 <- temp_players2[order(temp_players2$Gls, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\I1"
  write.xlsx(temp_players2,file.path(path,paste(final_doublefixture_i1[i1_sn,1],final_doublefixture_i1[i1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads2",append = TRUE)

}
#######################################
#SP1
for(sp1_sn in 1:19){
  ef <- laligasquad2[laligasquad2$Squad == final_doublefixture_sp1[sp1_sn,1],]

  ef2 <- laligasquad2[laligasquad2$Squad == final_doublefixture_sp1[sp1_sn + 1,1],]

  temp_players2 <- rbind(ef,ef2)
  temp_players2 <- temp_players2[order(temp_players2$Gls, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\SP1"
  write.xlsx(temp_players2,file.path(path,paste(final_doublefixture_sp1[sp1_sn,1],final_doublefixture_sp1[sp1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads2",append = TRUE)

}

#######################################
#F1
for(f1_sn in 1:17){
  ef <- ligueonesquad2[ligueonesquad2$Squad == final_doublefixture_f1[f1_sn,1],]

  ef2 <- ligueonesquad2[ligueonesquad2$Squad == final_doublefixture_f1[f1_sn + 1,1],]

  temp_players2 <- rbind(ef,ef2)
  temp_players2 <- temp_players2[order(temp_players2$Gls, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Summaries\\F1"
  write.xlsx(temp_players2,file.path(path,paste(final_doublefixture_f1[f1_sn,1],final_doublefixture_f1[f1_sn + 1,1],"adv.xlsx",sep = "_")),sheetName = "squads2",append = TRUE)

}
###############################################################################################################################################################################
###############################################################################################################################################################################
#B1
unlink('Squads/B1/*')
for(b1_sn in 1:15){
  df <- b1squad2[b1squad2$Squad == final_doublefixture_b1[b1_sn,1],]

  df2 <- b1squad2[b1squad2$Squad == final_doublefixture_b1[b1_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Squads\\B1"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_b1[b1_sn,1],final_doublefixture_b1[b1_sn + 1,1],"squad.xlsx",sep = "_")))

}
##############################################################################################################################################
#D2
unlink('Squads/D2/*')
for(d2_sn in 1:17){
  df <- d2squad2[d2squad2$Squad == final_doublefixture_d2[d2_sn,1],]

  df2 <- d2squad2[d2squad2$Squad == final_doublefixture_d2[d2_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Squads\\D2"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_d2[d2_sn,1],final_doublefixture_d2[d2_sn + 1,1],"squad.xlsx",sep = "_")))

}
################################################################################################################################################
#SC0
unlink('Squads/SC0/*')
for(sc0_sn in 1:11){
  df <- sc0squad2[sc0squad2$Squad == final_doublefixture_sc0[sc0_sn,1],]

  df2 <- sc0squad2[sc0squad2$Squad == final_doublefixture_sc0[sc0_sn + 1,1],]

  temp_players <- rbind(df,df2)
  temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]

  path = "C:\\Users\\Kovan\\Advancedmetrics\\Squads\\SC0"
  write.xlsx(temp_players,file.path(path,paste(final_doublefixture_sc0[sc0_sn,1],final_doublefixture_sc0[sc0_sn + 1,1],"squad.xlsx",sep = "_")))

}













unique(sort(playerdata$Squad))
###############individual team name#######################################
# library(xlsx)
# df <- playerdata[playerdata$Squad == "Manchester City",]
# df2 <- playerdata[playerdata$Squad == "Brighton",]
#
# ef <- playerdata2[playerdata2$Squad == "Manchester City",]
# ef2 <- playerdata2[playerdata2$Squad == "Brighton",]
#
# temp_players <- rbind(df,df2)
# temp_players <- temp_players[order(temp_players$YCrd, decreasing = T),]
#
# etemp_players <- rbind(ef,ef2)
# etemp_players <- etemp_players[order(etemp_players$Gls, decreasing = T),]
#
#
# write.xlsx(temp_players,'Temp/brightoncity.xlsx')
# write.xlsx(etemp_players,'Temp/brightoncity.xlsx',sheetName = "sheet2",append = TRUE)











