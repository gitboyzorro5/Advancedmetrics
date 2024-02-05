
b1_df_picks <- 1
d1_df_picks <- 1
d2_df_picks <- 1
e0_df_picks <- 1
e1_df_picks <- 1
e2_df_picks <- 1
e3_df_picks <- 1
ec_df_picks <- 1
f1_df_picks <- 1
f2_df_picks <- 1
i1_df_picks <- 1
i2_df_picks <- 1
g1_df_picks <- 1
n1_df_picks <- 1
p1_df_picks <- 1
sp1_df_picks <- 1
sp2_df_picks <- 1
sc0_df_picks <- 1
sc1_df_picks <- 1
sc2_df_picks <- 1
sc3_df_picks <- 1
t1_df_picks  <- 1


#############################################################
myfixturesb1 <- subset(myfixtures,Div == "B1")
#############################################################
final_doublefixture_b1 <- c()
final_first_column_b1 <- c()
final_second_column_b1 <- c()

for(dbfixture_b1 in 1:nrow(myfixturesb1))
{
  test_fixture_b1 <- myfixturesb1[dbfixture_b1,]
  test_fixture_inv_b1 <- test_fixture_b1[,c(1,3,2,4)]


  final_test_fixture_b1 <- test_fixture_b1[rep(seq_len(nrow(test_fixture_b1)), each = b1_df_picks),]
  final_test_fixture_inv_b1 <- test_fixture_inv_b1[rep(seq_len(nrow(test_fixture_inv_b1)), each = b1_df_picks),]


  final_test_fixture_b1 <- final_test_fixture_b1[,c(2,3)]
  final_test_fixture_inv_b1 <- final_test_fixture_inv_b1[,c(2,3)]

  final_first_column_b1 <- c(final_test_fixture_b1[,c(1)],final_test_fixture_inv_b1[,c(1)])
  final_second_column_b1 <- c(final_test_fixture_b1[,c(2)],final_test_fixture_inv_b1[,c(2)])

  mid_doublefixture_b1 <- cbind(final_first_column_b1,final_second_column_b1)

  final_doublefixture_b1 <- rbind(final_doublefixture_b1,mid_doublefixture_b1)
  final_doublefixture_b1

}
unlink('finaldoublefixtureb1.csv')
write.csv(final_doublefixture_b1,"finaldoublefixtureb1.csv")
############################################################################################################################
#############################################################
myfixturesd1 <- subset(myfixtures,Div == "D1")
#############################################################
final_doublefixture_d1 <- c()
final_first_column_d1 <- c()
final_second_column_d1 <- c()

for(dbfixture_d1 in 1:nrow(myfixturesd1))
{
  test_fixture_d1 <- myfixturesd1[dbfixture_d1,]
  test_fixture_inv_d1 <- test_fixture_d1[,c(1,3,2,4)]
myfixturesd1

  final_test_fixture_d1 <- test_fixture_d1[rep(seq_len(nrow(test_fixture_d1)), each = d1_df_picks),]
  final_test_fixture_inv_d1 <- test_fixture_inv_d1[rep(seq_len(nrow(test_fixture_inv_d1)), each = d1_df_picks),]


  final_test_fixture_d1 <- final_test_fixture_d1[,c(2,3)]
  final_test_fixture_inv_d1 <- final_test_fixture_inv_d1[,c(2,3)]

  final_first_column_d1 <- c(final_test_fixture_d1[,c(1)],final_test_fixture_inv_d1[,c(1)])
  final_second_column_d1 <- c(final_test_fixture_d1[,c(2)],final_test_fixture_inv_d1[,c(2)])

  mid_doublefixture_d1 <- cbind(final_first_column_d1,final_second_column_d1)

  final_doublefixture_d1 <- rbind(final_doublefixture_d1,mid_doublefixture_d1)


}
unlink('finaldoublefixtured1.csv')
write.csv(final_doublefixture_d1,"finaldoublefixtured1.csv")
#########################################################################################################################
#############################################################
myfixturesd2 <- subset(myfixtures,Div == "D2")
#############################################################
final_doublefixture_d2 <- c()
final_first_column_d2 <- c()
final_second_column_d2 <- c()

for(dbfixture_d2 in 1:nrow(myfixturesd2))
{
  test_fixture_d2 <- myfixturesd2[dbfixture_d2,]
  test_fixture_inv_d2 <- test_fixture_d2[,c(1,3,2,4)]


  final_test_fixture_d2 <- test_fixture_d2[rep(seq_len(nrow(test_fixture_d2)), each = d2_df_picks),]
  final_test_fixture_inv_d2 <- test_fixture_inv_d2[rep(seq_len(nrow(test_fixture_inv_d2)), each = d2_df_picks),]


  final_test_fixture_d2 <- final_test_fixture_d2[,c(2,3)]
  final_test_fixture_inv_d2 <- final_test_fixture_inv_d2[,c(2,3)]

  final_first_column_d2 <- c(final_test_fixture_d2[,c(1)],final_test_fixture_inv_d2[,c(1)])
  final_second_column_d2 <- c(final_test_fixture_d2[,c(2)],final_test_fixture_inv_d2[,c(2)])

  mid_doublefixture_d2 <- cbind(final_first_column_d2,final_second_column_d2)

  final_doublefixture_d2 <- rbind(final_doublefixture_d2,mid_doublefixture_d2)


}
unlink('finaldoublefixtured2.csv')
write.csv(final_doublefixture_d2,"finaldoublefixtured2.csv")
#####################################################################################################################
#############################################################
myfixturese0 <- subset(myfixtures,Div == "E0")
#############################################################
final_doublefixture_e0 <- c()
final_first_column_e0 <- c()
final_second_column_e0 <- c()

for(dbfixture_e0 in 1:nrow(myfixturese0))
{
  test_fixture_e0 <- myfixturese0[dbfixture_e0,]
  test_fixture_inv_e0 <- test_fixture_e0[,c(1,3,2,4)]


  final_test_fixture_e0 <- test_fixture_e0[rep(seq_len(nrow(test_fixture_e0)), each = e0_df_picks),]
  final_test_fixture_inv_e0 <- test_fixture_inv_e0[rep(seq_len(nrow(test_fixture_inv_e0)), each = e0_df_picks),]


  final_test_fixture_e0 <- final_test_fixture_e0[,c(2,3)]
  final_test_fixture_inv_e0 <- final_test_fixture_inv_e0[,c(2,3)]

  final_first_column_e0 <- c(final_test_fixture_e0[,c(1)],final_test_fixture_inv_e0[,c(1)])
  final_second_column_e0 <- c(final_test_fixture_e0[,c(2)],final_test_fixture_inv_e0[,c(2)])

  mid_doublefixture_e0 <- cbind(final_first_column_e0,final_second_column_e0)

  final_doublefixture_e0 <- rbind(final_doublefixture_e0,mid_doublefixture_e0)


}
unlink('finaldoublefixturee0.csv')
write.csv(final_doublefixture_e0,"finaldoublefixturee0.csv")
#############################################################################################################
myfixturese1 <- subset(myfixtures,Div == "E1")
#############################################################
final_doublefixture_e1 <- c()
final_first_column_e1 <- c()
final_second_column_e1 <- c()

for(dbfixture_e1 in 1:nrow(myfixturese1))
{
  test_fixture_e1 <- myfixturese1[dbfixture_e1,]
  test_fixture_inv_e1 <- test_fixture_e1[,c(1,3,2,4)]


  final_test_fixture_e1 <- test_fixture_e1[rep(seq_len(nrow(test_fixture_e1)), each = e1_df_picks),]
  final_test_fixture_inv_e1 <- test_fixture_inv_e1[rep(seq_len(nrow(test_fixture_inv_e1)), each = e1_df_picks),]


  final_test_fixture_e1 <- final_test_fixture_e1[,c(2,3)]
  final_test_fixture_inv_e1 <- final_test_fixture_inv_e1[,c(2,3)]

  final_first_column_e1 <- c(final_test_fixture_e1[,c(1)],final_test_fixture_inv_e1[,c(1)])
  final_second_column_e1 <- c(final_test_fixture_e1[,c(2)],final_test_fixture_inv_e1[,c(2)])

  mid_doublefixture_e1 <- cbind(final_first_column_e1,final_second_column_e1)

  final_doublefixture_e1 <- rbind(final_doublefixture_e1,mid_doublefixture_e1)


}
unlink('finaldoublefixturee1.csv')
write.csv(final_doublefixture_e1,"finaldoublefixturee1.csv")
##################################################################################################################
myfixturese2 <- subset(myfixtures,Div == "E2")
#############################################################
final_doublefixture_e2 <- c()
final_first_column_e2 <- c()
final_second_column_e2 <- c()

for(dbfixture_e2 in 1:nrow(myfixturese2))
{
  test_fixture_e2 <- myfixturese2[dbfixture_e2,]
  test_fixture_inv_e2 <- test_fixture_e2[,c(1,3,2,4)]


  final_test_fixture_e2 <- test_fixture_e2[rep(seq_len(nrow(test_fixture_e2)), each = e2_df_picks),]
  final_test_fixture_inv_e2 <- test_fixture_inv_e2[rep(seq_len(nrow(test_fixture_inv_e2)), each = e2_df_picks),]


  final_test_fixture_e2 <- final_test_fixture_e2[,c(2,3)]
  final_test_fixture_inv_e2 <- final_test_fixture_inv_e2[,c(2,3)]

  final_first_column_e2 <- c(final_test_fixture_e2[,c(1)],final_test_fixture_inv_e2[,c(1)])
  final_second_column_e2 <- c(final_test_fixture_e2[,c(2)],final_test_fixture_inv_e2[,c(2)])

  mid_doublefixture_e2 <- cbind(final_first_column_e2,final_second_column_e2)

  final_doublefixture_e2 <- rbind(final_doublefixture_e2,mid_doublefixture_e2)


}
unlink('finaldoublefixturee2.csv')
write.csv(final_doublefixture_e2,"finaldoublefixturee2.csv")
###########################################################################################################
myfixturese3 <- subset(myfixtures,Div == "E3")
#############################################################
final_doublefixture_e3 <- c()
final_first_column_e3 <- c()
final_second_column_e3 <- c()

for(dbfixture_e3 in 1:nrow(myfixturese3))
{
  test_fixture_e3 <- myfixturese3[dbfixture_e3,]
  test_fixture_inv_e3 <- test_fixture_e3[,c(1,3,2,4)]


  final_test_fixture_e3 <- test_fixture_e3[rep(seq_len(nrow(test_fixture_e3)), each = e3_df_picks),]
  final_test_fixture_inv_e3 <- test_fixture_inv_e3[rep(seq_len(nrow(test_fixture_inv_e3)), each = e3_df_picks),]


  final_test_fixture_e3 <- final_test_fixture_e3[,c(2,3)]
  final_test_fixture_inv_e3 <- final_test_fixture_inv_e3[,c(2,3)]

  final_first_column_e3 <- c(final_test_fixture_e3[,c(1)],final_test_fixture_inv_e3[,c(1)])
  final_second_column_e3 <- c(final_test_fixture_e3[,c(2)],final_test_fixture_inv_e3[,c(2)])

  mid_doublefixture_e3 <- cbind(final_first_column_e3,final_second_column_e3)

  final_doublefixture_e3 <- rbind(final_doublefixture_e3,mid_doublefixture_e3)


}
unlink('finaldoublefixturee3.csv')
write.csv(final_doublefixture_e3,"finaldoublefixturee3.csv")
##########################################################################################################
myfixturesec <- subset(myfixtures,Div == "EC")
#############################################################
final_doublefixture_ec <- c()
final_first_column_ec <- c()
final_second_column_ec <- c()

for(dbfixture_ec in 1:nrow(myfixturesec))
{
  test_fixture_ec <- myfixturesec[dbfixture_ec,]
  test_fixture_inv_ec <- test_fixture_ec[,c(1,3,2,4)]


  final_test_fixture_ec <- test_fixture_ec[rep(seq_len(nrow(test_fixture_ec)), each = ec_df_picks),]
  final_test_fixture_inv_ec <- test_fixture_inv_ec[rep(seq_len(nrow(test_fixture_inv_ec)), each = ec_df_picks),]


  final_test_fixture_ec <- final_test_fixture_ec[,c(2,3)]
  final_test_fixture_inv_ec <- final_test_fixture_inv_ec[,c(2,3)]

  final_first_column_ec <- c(final_test_fixture_ec[,c(1)],final_test_fixture_inv_ec[,c(1)])
  final_second_column_ec <- c(final_test_fixture_ec[,c(2)],final_test_fixture_inv_ec[,c(2)])

  mid_doublefixture_ec <- cbind(final_first_column_ec,final_second_column_ec)

  final_doublefixture_ec <- rbind(final_doublefixture_ec,mid_doublefixture_ec)


}
unlink('finaldoublefixtureec.csv')
write.csv(final_doublefixture_ec,"finaldoublefixtureec.csv")
##################################################################################################
myfixturesf1 <- subset(myfixtures,Div == "F1")
#############################################################
final_doublefixture_f1 <- c()
final_first_column_f1 <- c()
final_second_column_f1 <- c()

for(dbfixture_f1 in 1:nrow(myfixturesf1))
{
  test_fixture_f1 <- myfixturesf1[dbfixture_f1,]
  test_fixture_inv_f1 <- test_fixture_f1[,c(1,3,2,4)]


  final_test_fixture_f1 <- test_fixture_f1[rep(seq_len(nrow(test_fixture_f1)), each = f1_df_picks),]
  final_test_fixture_inv_f1 <- test_fixture_inv_f1[rep(seq_len(nrow(test_fixture_inv_f1)), each = f1_df_picks),]


  final_test_fixture_f1 <- final_test_fixture_f1[,c(2,3)]
  final_test_fixture_inv_f1 <- final_test_fixture_inv_f1[,c(2,3)]

  final_first_column_f1 <- c(final_test_fixture_f1[,c(1)],final_test_fixture_inv_f1[,c(1)])
  final_second_column_f1 <- c(final_test_fixture_f1[,c(2)],final_test_fixture_inv_f1[,c(2)])

  mid_doublefixture_f1 <- cbind(final_first_column_f1,final_second_column_f1)

  final_doublefixture_f1 <- rbind(final_doublefixture_f1,mid_doublefixture_f1)


}
unlink('finaldoublefixturef1.csv')
write.csv(final_doublefixture_f1,"finaldoublefixturef1.csv")
################################################################################################################
myfixturesf2 <- subset(myfixtures,Div == "F2")
#############################################################
final_doublefixture_f2 <- c()
final_first_column_f2 <- c()
final_second_column_f2 <- c()

for(dbfixture_f2 in 1:nrow(myfixturesf2))
{
  test_fixture_f2 <- myfixturesf2[dbfixture_f2,]
  test_fixture_inv_f2 <- test_fixture_f2[,c(1,3,2,4)]


  final_test_fixture_f2 <- test_fixture_f2[rep(seq_len(nrow(test_fixture_f2)), each = f2_df_picks),]
  final_test_fixture_inv_f2 <- test_fixture_inv_f2[rep(seq_len(nrow(test_fixture_inv_f2)), each = f2_df_picks),]


  final_test_fixture_f2 <- final_test_fixture_f2[,c(2,3)]
  final_test_fixture_inv_f2 <- final_test_fixture_inv_f2[,c(2,3)]

  final_first_column_f2 <- c(final_test_fixture_f2[,c(1)],final_test_fixture_inv_f2[,c(1)])
  final_second_column_f2 <- c(final_test_fixture_f2[,c(2)],final_test_fixture_inv_f2[,c(2)])

  mid_doublefixture_f2 <- cbind(final_first_column_f2,final_second_column_f2)

  final_doublefixture_f2 <- rbind(final_doublefixture_f2,mid_doublefixture_f2)


}
unlink('finaldoublefixturef2.csv')
write.csv(final_doublefixture_f2,"finaldoublefixturef2.csv")

####################################################################################################################
myfixturesg1 <- subset(myfixtures,Div == "G1")
#############################################################
final_doublefixture_g1 <- c()
final_first_column_g1 <- c()
final_second_column_g1 <- c()

for(dbfixture_g1 in 1:nrow(myfixturesg1))
{
  test_fixture_g1 <- myfixturesg1[dbfixture_g1,]
  test_fixture_inv_g1 <- test_fixture_g1[,c(1,3,2,4)]


  final_test_fixture_g1 <- test_fixture_g1[rep(seq_len(nrow(test_fixture_g1)), each = g1_df_picks),]
  final_test_fixture_inv_g1 <- test_fixture_inv_g1[rep(seq_len(nrow(test_fixture_inv_g1)), each = g1_df_picks),]


  final_test_fixture_g1 <- final_test_fixture_g1[,c(2,3)]
  final_test_fixture_inv_g1 <- final_test_fixture_inv_g1[,c(2,3)]

  final_first_column_g1 <- c(final_test_fixture_g1[,c(1)],final_test_fixture_inv_g1[,c(1)])
  final_second_column_g1 <- c(final_test_fixture_g1[,c(2)],final_test_fixture_inv_g1[,c(2)])

  mid_doublefixture_g1 <- cbind(final_first_column_g1,final_second_column_g1)

  final_doublefixture_g1 <- rbind(final_doublefixture_g1,mid_doublefixture_g1)


}
unlink('finaldoublefixtureg1.csv')
write.csv(final_doublefixture_g1,"finaldoublefixtureg1.csv")
####################################################################################################################

##################################################################################################
myfixturesi1 <- subset(myfixtures,Div == "I1")
#############################################################
final_doublefixture_i1 <- c()
final_first_column_i1 <- c()
final_second_column_i1 <- c()

for(dbfixture_i1 in 1:nrow(myfixturesi1))
{
  test_fixture_i1 <- myfixturesi1[dbfixture_i1,]
  test_fixture_inv_i1 <- test_fixture_i1[,c(1,3,2,4)]


  final_test_fixture_i1 <- test_fixture_i1[rep(seq_len(nrow(test_fixture_i1)), each = i1_df_picks),]
  final_test_fixture_inv_i1 <- test_fixture_inv_i1[rep(seq_len(nrow(test_fixture_inv_i1)), each = i1_df_picks),]


  final_test_fixture_i1 <- final_test_fixture_i1[,c(2,3)]
  final_test_fixture_inv_i1 <- final_test_fixture_inv_i1[,c(2,3)]

  final_first_column_i1 <- c(final_test_fixture_i1[,c(1)],final_test_fixture_inv_i1[,c(1)])
  final_second_column_i1 <- c(final_test_fixture_i1[,c(2)],final_test_fixture_inv_i1[,c(2)])

  mid_doublefixture_i1 <- cbind(final_first_column_i1,final_second_column_i1)

  final_doublefixture_i1 <- rbind(final_doublefixture_i1,mid_doublefixture_i1)


}
unlink('finaldoublefixturei1.csv')
write.csv(final_doublefixture_i1,"finaldoublefixturei1.csv")
#########################################################################################################

#########################################################################################################
myfixturesi2 <- subset(myfixtures,Div == "I2")
#############################################################
final_doublefixture_i2 <- c()
final_first_column_i2 <- c()
final_second_column_i2 <- c()

for(dbfixture_i2 in 1:nrow(myfixturesi2))
{
  test_fixture_i2 <- myfixturesi2[dbfixture_i2,]
  test_fixture_inv_i2 <- test_fixture_i2[,c(1,3,2,4)]


  final_test_fixture_i2 <- test_fixture_i2[rep(seq_len(nrow(test_fixture_i2)), each = i2_df_picks),]
  final_test_fixture_inv_i2 <- test_fixture_inv_i2[rep(seq_len(nrow(test_fixture_inv_i2)), each = i2_df_picks),]


  final_test_fixture_i2 <- final_test_fixture_i2[,c(2,3)]
  final_test_fixture_inv_i2 <- final_test_fixture_inv_i2[,c(2,3)]

  final_first_column_i2 <- c(final_test_fixture_i2[,c(1)],final_test_fixture_inv_i2[,c(1)])
  final_second_column_i2 <- c(final_test_fixture_i2[,c(2)],final_test_fixture_inv_i2[,c(2)])

  mid_doublefixture_i2 <- cbind(final_first_column_i2,final_second_column_i2)

  final_doublefixture_i2 <- rbind(final_doublefixture_i2,mid_doublefixture_i2)


}
unlink('finaldoublefixturei2.csv')
write.csv(final_doublefixture_i2,"finaldoublefixturei2.csv")
#########################################################################################################
myfixturesn1 <- subset(myfixtures,Div == "N1")
#############################################################
final_doublefixture_n1 <- c()
final_first_column_n1 <- c()
final_second_column_n1 <- c()

for(dbfixture_n1 in 1:nrow(myfixturesn1))
{
  test_fixture_n1 <- myfixturesn1[dbfixture_n1,]
  test_fixture_inv_n1 <- test_fixture_n1[,c(1,3,2,4)]


  final_test_fixture_n1 <- test_fixture_n1[rep(seq_len(nrow(test_fixture_n1)), each = n1_df_picks),]
  final_test_fixture_inv_n1 <- test_fixture_inv_n1[rep(seq_len(nrow(test_fixture_inv_n1)), each = n1_df_picks),]


  final_test_fixture_n1 <- final_test_fixture_n1[,c(2,3)]
  final_test_fixture_inv_n1 <- final_test_fixture_inv_n1[,c(2,3)]

  final_first_column_n1 <- c(final_test_fixture_n1[,c(1)],final_test_fixture_inv_n1[,c(1)])
  final_second_column_n1 <- c(final_test_fixture_n1[,c(2)],final_test_fixture_inv_n1[,c(2)])

  mid_doublefixture_n1 <- cbind(final_first_column_n1,final_second_column_n1)

  final_doublefixture_n1 <- rbind(final_doublefixture_n1,mid_doublefixture_n1)


}
unlink('finaldoublefixturen1.csv')
write.csv(final_doublefixture_n1,"finaldoublefixturen1.csv")
###############################################################################################################
myfixturesp1 <- subset(myfixtures,Div == "P1")
#############################################################
final_doublefixture_p1 <- c()
final_first_column_p1 <- c()
final_second_column_p1 <- c()

for(dbfixture_p1 in 1:nrow(myfixturesp1))
{
  test_fixture_p1 <- myfixturesp1[dbfixture_p1,]
  test_fixture_inv_p1 <- test_fixture_p1[,c(1,3,2,4)]


  final_test_fixture_p1 <- test_fixture_p1[rep(seq_len(nrow(test_fixture_p1)), each = p1_df_picks),]
  final_test_fixture_inv_p1 <- test_fixture_inv_p1[rep(seq_len(nrow(test_fixture_inv_p1)), each = p1_df_picks),]


  final_test_fixture_p1 <- final_test_fixture_p1[,c(2,3)]
  final_test_fixture_inv_p1 <- final_test_fixture_inv_p1[,c(2,3)]

  final_first_column_p1 <- c(final_test_fixture_p1[,c(1)],final_test_fixture_inv_p1[,c(1)])
  final_second_column_p1 <- c(final_test_fixture_p1[,c(2)],final_test_fixture_inv_p1[,c(2)])

  mid_doublefixture_p1 <- cbind(final_first_column_p1,final_second_column_p1)

  final_doublefixture_p1 <- rbind(final_doublefixture_p1,mid_doublefixture_p1)


}
unlink('finaldoublefixturep1.csv')
write.csv(final_doublefixture_p1,"finaldoublefixturep1.csv")
#####################################################################################################
myfixturessc0 <- subset(myfixtures,Div == "SC0")
#############################################################
final_doublefixture_sc0 <- c()
final_first_column_sc0 <- c()
final_second_column_sc0 <- c()

for(dbfixture_sc0 in 1:nrow(myfixturessc0))
{
  test_fixture_sc0 <- myfixturessc0[dbfixture_sc0,]
  test_fixture_inv_sc0 <- test_fixture_sc0[,c(1,3,2,4)]


  final_test_fixture_sc0 <- test_fixture_sc0[rep(seq_len(nrow(test_fixture_sc0)), each = sc0_df_picks),]
  final_test_fixture_inv_sc0 <- test_fixture_inv_sc0[rep(seq_len(nrow(test_fixture_inv_sc0)), each = sc0_df_picks),]


  final_test_fixture_sc0 <- final_test_fixture_sc0[,c(2,3)]
  final_test_fixture_inv_sc0 <- final_test_fixture_inv_sc0[,c(2,3)]

  final_first_column_sc0 <- c(final_test_fixture_sc0[,c(1)],final_test_fixture_inv_sc0[,c(1)])
  final_second_column_sc0 <- c(final_test_fixture_sc0[,c(2)],final_test_fixture_inv_sc0[,c(2)])

  mid_doublefixture_sc0 <- cbind(final_first_column_sc0,final_second_column_sc0)

  final_doublefixture_sc0 <- rbind(final_doublefixture_sc0,mid_doublefixture_sc0)


}
unlink('finaldoublefixturesc0.csv')
write.csv(final_doublefixture_sc0,"finaldoublefixturesc0.csv")
##############################################################################################################
myfixturessc1 <- subset(myfixtures,Div == "SC1")
#############################################################
final_doublefixture_sc1 <- c()
final_first_column_sc1 <- c()
final_second_column_sc1 <- c()

for(dbfixture_sc1 in 1:nrow(myfixturessc1))
{
  test_fixture_sc1 <- myfixturessc1[dbfixture_sc1,]
  test_fixture_inv_sc1 <- test_fixture_sc1[,c(1,3,2,4)]


  final_test_fixture_sc1 <- test_fixture_sc1[rep(seq_len(nrow(test_fixture_sc1)), each = sc1_df_picks),]
  final_test_fixture_inv_sc1 <- test_fixture_inv_sc1[rep(seq_len(nrow(test_fixture_inv_sc1)), each = sc1_df_picks),]


  final_test_fixture_sc1 <- final_test_fixture_sc1[,c(2,3)]
  final_test_fixture_inv_sc1 <- final_test_fixture_inv_sc1[,c(2,3)]

  final_first_column_sc1 <- c(final_test_fixture_sc1[,c(1)],final_test_fixture_inv_sc1[,c(1)])
  final_second_column_sc1 <- c(final_test_fixture_sc1[,c(2)],final_test_fixture_inv_sc1[,c(2)])

  mid_doublefixture_sc1 <- cbind(final_first_column_sc1,final_second_column_sc1)

  final_doublefixture_sc1 <- rbind(final_doublefixture_sc1,mid_doublefixture_sc1)


}
unlink('finaldoublefixturesc1.csv')
write.csv(final_doublefixture_sc1,"finaldoublefixturesc1.csv")
##################################################################################################
myfixturessc2 <- subset(myfixtures,Div == "SC2")
#############################################################
final_doublefixture_sc2 <- c()
final_first_column_sc2 <- c()
final_second_column_sc2 <- c()

for(dbfixture_sc2 in 1:nrow(myfixturessc2))
{
  test_fixture_sc2 <- myfixturessc2[dbfixture_sc2,]
  test_fixture_inv_sc2 <- test_fixture_sc2[,c(1,3,2,4)]


  final_test_fixture_sc2 <- test_fixture_sc2[rep(seq_len(nrow(test_fixture_sc2)), each = sc2_df_picks),]
  final_test_fixture_inv_sc2 <- test_fixture_inv_sc2[rep(seq_len(nrow(test_fixture_inv_sc2)), each = sc2_df_picks),]


  final_test_fixture_sc2 <- final_test_fixture_sc2[,c(2,3)]
  final_test_fixture_inv_sc2 <- final_test_fixture_inv_sc2[,c(2,3)]

  final_first_column_sc2 <- c(final_test_fixture_sc2[,c(1)],final_test_fixture_inv_sc2[,c(1)])
  final_second_column_sc2 <- c(final_test_fixture_sc2[,c(2)],final_test_fixture_inv_sc2[,c(2)])

  mid_doublefixture_sc2 <- cbind(final_first_column_sc2,final_second_column_sc2)

  final_doublefixture_sc2 <- rbind(final_doublefixture_sc2,mid_doublefixture_sc2)


}
unlink('finaldoublefixturesc2.csv')
write.csv(final_doublefixture_sc2,"finaldoublefixturesc2.csv")
######################################################################################################################
myfixturessc3 <- subset(myfixtures,Div == "SC3")
#############################################################
final_doublefixture_sc3 <- c()
final_first_column_sc3 <- c()
final_second_column_sc3 <- c()

for(dbfixture_sc3 in 1:nrow(myfixturessc3))
{
  test_fixture_sc3 <- myfixturessc3[dbfixture_sc3,]
  test_fixture_inv_sc3 <- test_fixture_sc3[,c(1,3,2,4)]


  final_test_fixture_sc3 <- test_fixture_sc3[rep(seq_len(nrow(test_fixture_sc3)), each = sc3_df_picks),]
  final_test_fixture_inv_sc3 <- test_fixture_inv_sc3[rep(seq_len(nrow(test_fixture_inv_sc3)), each = sc3_df_picks),]


  final_test_fixture_sc3 <- final_test_fixture_sc3[,c(2,3)]
  final_test_fixture_inv_sc3 <- final_test_fixture_inv_sc3[,c(2,3)]

  final_first_column_sc3 <- c(final_test_fixture_sc3[,c(1)],final_test_fixture_inv_sc3[,c(1)])
  final_second_column_sc3 <- c(final_test_fixture_sc3[,c(2)],final_test_fixture_inv_sc3[,c(2)])

  mid_doublefixture_sc3 <- cbind(final_first_column_sc3,final_second_column_sc3)

  final_doublefixture_sc3 <- rbind(final_doublefixture_sc3,mid_doublefixture_sc3)


}
unlink('finaldoublefixturesc3.csv')
write.csv(final_doublefixture_sc3,"finaldoublefixturesc3.csv")
######################################################################################################################

##################################################################################################
myfixturessp1 <- subset(myfixtures,Div == "SP1")
##################################################################################################
final_doublefixture_sp1 <- c()
final_first_column_sp1 <- c()
final_second_column_sp1 <- c()

for(dbfixture_sp1 in 1:nrow(myfixturessp1))
{
  test_fixture_sp1 <- myfixturessp1[dbfixture_sp1,]
  test_fixture_inv_sp1 <- test_fixture_sp1[,c(1,3,2,4)]


  final_test_fixture_sp1 <- test_fixture_sp1[rep(seq_len(nrow(test_fixture_sp1)), each = sp1_df_picks),]
  final_test_fixture_inv_sp1 <- test_fixture_inv_sp1[rep(seq_len(nrow(test_fixture_inv_sp1)), each = sp1_df_picks),]


  final_test_fixture_sp1 <- final_test_fixture_sp1[,c(2,3)]
  final_test_fixture_inv_sp1 <- final_test_fixture_inv_sp1[,c(2,3)]

  final_first_column_sp1 <- c(final_test_fixture_sp1[,c(1)],final_test_fixture_inv_sp1[,c(1)])
  final_second_column_sp1 <- c(final_test_fixture_sp1[,c(2)],final_test_fixture_inv_sp1[,c(2)])

  mid_doublefixture_sp1 <- cbind(final_first_column_sp1,final_second_column_sp1)

  final_doublefixture_sp1 <- rbind(final_doublefixture_sp1,mid_doublefixture_sp1)


}
unlink('finaldoublefixturesp1.csv')
write.csv(final_doublefixture_sp1,"finaldoublefixturesp1.csv")
######################################################################################################
myfixturessp2 <- subset(myfixtures,Div == "SP2")
#############################################################
final_doublefixture_sp2 <- c()
final_first_column_sp2 <- c()
final_second_column_sp2 <- c()

for(dbfixture_sp2 in 1:nrow(myfixturessp2))
{
  test_fixture_sp2 <- myfixturessp2[dbfixture_sp2,]
  test_fixture_inv_sp2 <- test_fixture_sp2[,c(1,3,2,4)]


  final_test_fixture_sp2 <- test_fixture_sp2[rep(seq_len(nrow(test_fixture_sp2)), each = sp2_df_picks),]
  final_test_fixture_inv_sp2 <- test_fixture_inv_sp2[rep(seq_len(nrow(test_fixture_inv_sp2)), each = sp2_df_picks),]


  final_test_fixture_sp2 <- final_test_fixture_sp2[,c(2,3)]
  final_test_fixture_inv_sp2 <- final_test_fixture_inv_sp2[,c(2,3)]

  final_first_column_sp2 <- c(final_test_fixture_sp2[,c(1)],final_test_fixture_inv_sp2[,c(1)])
  final_second_column_sp2 <- c(final_test_fixture_sp2[,c(2)],final_test_fixture_inv_sp2[,c(2)])

  mid_doublefixture_sp2 <- cbind(final_first_column_sp2,final_second_column_sp2)

  final_doublefixture_sp2 <- rbind(final_doublefixture_sp2,mid_doublefixture_sp2)


}
unlink('finaldoublefixturesp2.csv')
write.csv(final_doublefixture_sp2,"finaldoublefixturesp2.csv")
##########################################################################################################
myfixturest1 <- subset(myfixtures,Div == "T1")
#############################################################
final_doublefixture_t1 <- c()
final_first_column_t1 <- c()
final_second_column_t1 <- c()

for(dbfixture_t1 in 1:nrow(myfixturest1))
{
  test_fixture_t1 <- myfixturest1[dbfixture_t1,]
  test_fixture_inv_t1 <- test_fixture_t1[,c(1,3,2,4)]


  final_test_fixture_t1 <- test_fixture_t1[rep(seq_len(nrow(test_fixture_t1)), each = t1_df_picks),]
  final_test_fixture_inv_t1 <- test_fixture_inv_t1[rep(seq_len(nrow(test_fixture_inv_t1)), each = t1_df_picks),]


  final_test_fixture_t1 <- final_test_fixture_t1[,c(2,3)]
  final_test_fixture_inv_t1 <- final_test_fixture_inv_t1[,c(2,3)]

  final_first_column_t1 <- c(final_test_fixture_t1[,c(1)],final_test_fixture_inv_t1[,c(1)])
  final_second_column_t1 <- c(final_test_fixture_t1[,c(2)],final_test_fixture_inv_t1[,c(2)])

  mid_doublefixture_t1 <- cbind(final_first_column_t1,final_second_column_t1)

  final_doublefixture_t1 <- rbind(final_doublefixture_t1,mid_doublefixture_t1)


}

unlink('finaldoublefixturet1.csv')
write.csv(final_doublefixture_t1,"finaldoublefixturet1.csv")
##########################################################################################################













