#by gitboyzorro5
#create divisions and order by date
library('lubridate')
allteams20242025 <- read.csv('allteams20242025SOT.csv')

#change date strings to Date objects
allteams20242025$Date <- dmy(allteams20242025$Date)
allteams20242025 <- allteams20242025[order(as.Date(allteams20242025$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteams20242025$TG <- allteams20242025$FTHG + allteams20242025$FTAG
allteams20242025$TC <- allteams20242025$HCO + allteams20242025$ACO
allteams20242025$TF <- allteams20242025$HF + allteams20242025$AF
allteams20242025$COSC <- paste(allteams20242025$HCO,allteams20242025$ACO,sep = "-")
allteams20242025$OV15 <- ifelse(allteams20242025$TG >= 2,"Y","N")
allteams20242025$OV25 <- ifelse(allteams20242025$TG >= 3,"Y","N")
allteams20242025$OV35 <- ifelse(allteams20242025$TG >= 4,"Y","N")
allteams20242025$TY <- allteams20242025$HY + allteams20242025$AY
allteams20242025$TR <- allteams20242025$HR + allteams20242025$AR

#create divisions subsets
B1 <- subset(allteams20242025, Div == "B1")
D1 <- subset(allteams20242025, Div == "D1")
D2 <- subset(allteams20242025, Div == "D2")
E0 <- subset(allteams20242025, Div == "E0")
E1 <- subset(allteams20242025, Div == "E1")
E2 <- subset(allteams20242025, Div == "E2")
E3 <- subset(allteams20242025, Div == "E3")
EC <- subset(allteams20242025, Div == "EC")
F1 <- subset(allteams20242025, Div == "F1")
F2 <- subset(allteams20242025, Div == "F2")
G1 <- subset(allteams20242025, Div == "G1")
I1 <- subset(allteams20242025, Div == "I1")
I2 <- subset(allteams20242025, Div == "I2")
N1 <- subset(allteams20242025, Div == "N1")
P1 <- subset(allteams20242025, Div == "P1")
SC0 <- subset(allteams20242025, Div == "SC0")
SC1 <- subset(allteams20242025, Div == "SC1")
SC2 <- subset(allteams20242025, Div == "SC2")
SC3 <- subset(allteams20242025, Div == "SC3")
SP1 <- subset(allteams20242025, Div == "SP1")
SP2 <- subset(allteams20242025, Div == "SP2")
T1 <- subset(allteams20242025, Div == "T1")

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
View(allteams20242025)
