#by gitboyzorro5
#create divisions and order by date
library('lubridate')
allteams20232024 <- read.csv('allteams20232024SOT.csv')

#change date strings to Date objects
allteams20232024$Date <- dmy(allteams20232024$Date)
allteams20232024 <- allteams20232024[order(as.Date(allteams20232024$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteams20232024$TG <- allteams20232024$FTHG + allteams20232024$FTAG
allteams20232024$TC <- allteams20232024$HCO + allteams20232024$ACO
allteams20232024$TF <- allteams20232024$HF + allteams20232024$AF
allteams20232024$COSC <- paste(allteams20232024$HCO,allteams20232024$ACO,sep = "-")
allteams20232024$OV15 <- ifelse(allteams20232024$TG >= 2,"Y","N")
allteams20232024$OV25 <- ifelse(allteams20232024$TG >= 3,"Y","N")
allteams20232024$OV35 <- ifelse(allteams20232024$TG >= 4,"Y","N")
allteams20232024$TY <- allteams20232024$HY + allteams20232024$AY
allteams20232024$TR <- allteams20232024$HR + allteams20232024$AR

#create divisions subsets
B1 <- subset(allteams20232024, Div == "B1")
D1 <- subset(allteams20232024, Div == "D1")
D2 <- subset(allteams20232024, Div == "D2")
E0 <- subset(allteams20232024, Div == "E0")
E1 <- subset(allteams20232024, Div == "E1")
E2 <- subset(allteams20232024, Div == "E2")
E3 <- subset(allteams20232024, Div == "E3")
EC <- subset(allteams20232024, Div == "EC")
F1 <- subset(allteams20232024, Div == "F1")
F2 <- subset(allteams20232024, Div == "F2")
G1 <- subset(allteams20232024, Div == "G1")
I1 <- subset(allteams20232024, Div == "I1")
I2 <- subset(allteams20232024, Div == "I2")
N1 <- subset(allteams20232024, Div == "N1")
P1 <- subset(allteams20232024, Div == "P1")
SC0 <- subset(allteams20232024, Div == "SC0")
SC1 <- subset(allteams20232024, Div == "SC1")
SC2 <- subset(allteams20232024, Div == "SC2")
SC3 <- subset(allteams20232024, Div == "SC3")
SP1 <- subset(allteams20232024, Div == "SP1")
SP2 <- subset(allteams20232024, Div == "SP2")
T1 <- subset(allteams20232024, Div == "T1")

#################################################################
b1_teams <- sort(unique(B1$HomeTeam))
d1_teams <- sort(unique(D1$HomeTeam))
d2_teams <- sort(unique(D2$HomeTeam))
e0_teams <- sort(unique(E0$HomeTeam))
e1_teams <- sort(unique(E1$HomeTeam))
e2_teams <- sort(unique(E2$HomeTeam))
e3_teams <- sort(unique(E3$HomeTeam))
ec_teams <- sort(unique(EC$HomeTeam))
f1_teams <- sort(unique(F1$HomeTeam))
f2_teams <- sort(unique(F2$HomeTeam))
g1_teams <- sort(unique(G1$HomeTeam))
i1_teams <- sort(unique(I1$HomeTeam))
i2_teams <- sort(unique(I2$HomeTeam))
n1_teams <- sort(unique(N1$HomeTeam))
p1_teams <- sort(unique(P1$HomeTeam))
sc0_teams <- sort(unique(SC0$HomeTeam))
sc1_teams <- sort(unique(SC1$HomeTeam))
sc2_teams <- sort(unique(SC2$HomeTeam))
sc3_teams <- sort(unique(SC3$HomeTeam))
sp1_teams <- sort(unique(SP1$HomeTeam))
sp2_teams <- sort(unique(SP2$HomeTeam))
t1_teams <- sort(unique(T1$HomeTeam))


