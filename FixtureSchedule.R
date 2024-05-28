library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('lubridate')
unlink('myfixtures.csv')
#read the data
B1_schedule20232024 <- read.csv('../Downloads/B1_schedule20232024.csv')
D1_schedule20232024 <- read.csv('../Downloads/D1_schedule20232024.csv')
D2_schedule20232024 <- read.csv('../Downloads/D2_schedule20232024.csv')
E0_schedule20232024 <- read.csv('../Downloads/E0_schedule20232024.csv')
E1_schedule20232024 <- read.csv('../Downloads/E1_schedule20232024.csv')
E2_schedule20232024 <- read.csv('../Downloads/E2_schedule20232024.csv')
E3_schedule20232024 <- read.csv('../Downloads/E3_schedule20232024.csv')
EC_schedule20232024 <- read.csv('../Downloads/EC_schedule20232024.csv')
F1_schedule20232024 <- read.csv('../Downloads/F1_schedule20232024.csv')
F2_schedule20232024 <- read.csv('../Downloads/F2_schedule20232024.csv')
G1_schedule20232024 <- read.csv('../Downloads/G1_schedule20232024.csv')
#UCL_schedule20232024 <- read.csv('Downloads/UCL_schedule20232024.csv')
I1_schedule20232024 <- read.csv('../Downloads/I1_schedule20232024.csv')
I2_schedule20232024 <- read.csv('../Downloads/I2_schedule20232024.csv')
N1_schedule20232024 <- read.csv('../Downloads/N1_schedule20232024.csv')
P1_schedule20232024 <- read.csv('../Downloads/P1_schedule20232024.csv')
SP1_schedule20232024 <- read.csv('../Downloads/SP1_schedule20232024.csv')
SC0_schedule20232024 <- read.csv('../Downloads/SC0_schedule20232024.csv')
SC1_schedule20232024 <- read.csv('../Downloads/SC1_schedule20232024.csv')
SC2_schedule20232024 <- read.csv('../Downloads/SC2_schedule20232024.csv')
SC3_schedule20232024 <- read.csv('../Downloads/SC3_schedule20232024.csv')
SP2_schedule20232024 <- read.csv('../Downloads/SP2_schedule20232024.csv')
T1_schedule20232024 <- read.csv('../Downloads/T1_schedule20232024.csv')
#UEL_schedule20232024 <- read.csv('../Downloads/UEL_schedule20232024.csv')
#parse the dates
B1_schedule20232024$Date <- dmy(B1_schedule20232024$Date)
D1_schedule20232024$Date <- dmy(D1_schedule20232024$Date)
D2_schedule20232024$Date <- dmy(D2_schedule20232024$Date)
E0_schedule20232024$Date <- dmy(E0_schedule20232024$Date)
E1_schedule20232024$Date <- dmy(E1_schedule20232024$Date)
E2_schedule20232024$Date <- dmy(E2_schedule20232024$Date)
E3_schedule20232024$Date <- dmy(E3_schedule20232024$Date)
EC_schedule20232024$Date <- dmy(EC_schedule20232024$Date)
F1_schedule20232024$Date <- dmy(F1_schedule20232024$Date)
F2_schedule20232024$Date <- dmy(F2_schedule20232024$Date)
G1_schedule20232024$Date <- dmy(G1_schedule20232024$Date)
#UCL_schedule20232024$Date_ucl <- mdy(UCL_schedule20232024$Date_ucl)
I1_schedule20232024$Date <- dmy(I1_schedule20232024$Date)
I2_schedule20232024$Date <- dmy(I2_schedule20232024$Date)
N1_schedule20232024$Date <- dmy(N1_schedule20232024$Date)
P1_schedule20232024$Date <- dmy(P1_schedule20232024$Date)
SC0_schedule20232024$Date <- dmy(SC0_schedule20232024$Date)
SC1_schedule20232024$Date <- dmy(SC1_schedule20232024$Date)
SC2_schedule20232024$Date <- dmy(SC2_schedule20232024$Date)
SC3_schedule20232024$Date <- dmy(SC3_schedule20232024$Date)
SP1_schedule20232024$Date <- dmy(SP1_schedule20232024$Date)
SP2_schedule20232024$Date <- dmy(SP2_schedule20232024$Date)
T1_schedule20232024$Date <- dmy(T1_schedule20232024$Date)
#UEL_schedule20232024$Date_uel <- dmy(UEL_schedule20232024$Date_uel)
#insert divisions
B1_schedule20232024$Div <- "B1"
D1_schedule20232024$Div <- "D1"
D2_schedule20232024$Div <- "D2"
E0_schedule20232024$Div <- "E0"
E1_schedule20232024$Div <- "E1"
E2_schedule20232024$Div <- "E2"
E3_schedule20232024$Div <- "E3"
EC_schedule20232024$Div <- "EC"
F1_schedule20232024$Div <- "F1"
F2_schedule20232024$Div <- "F2"
G1_schedule20232024$Div <- "G1"
I1_schedule20232024$Div <- "I1"
I2_schedule20232024$Div <- "I2"
N1_schedule20232024$Div <- "N1"
P1_schedule20232024$Div <- "P1"
SC0_schedule20232024$Div <- "SC0"
SC1_schedule20232024$Div <- "SC1"
SC2_schedule20232024$Div <- "SC2"
SC3_schedule20232024$Div <- "SC3"
SP1_schedule20232024$Div <- "SP1"
SP2_schedule20232024$Div <- "SP2"
T1_schedule20232024$Div <- "T1"
#bind the fixtures

all_schedule20232024 <- rbind(B1_schedule20232024,D2_schedule20232024,D1_schedule20232024,E0_schedule20232024,E1_schedule20232024,E2_schedule20232024,E3_schedule20232024,EC_schedule20232024,F1_schedule20232024,F2_schedule20232024,G1_schedule20232024,I1_schedule20232024,I2_schedule20232024,N1_schedule20232024,P1_schedule20232024,SC0_schedule20232024,SC1_schedule20232024,SC2_schedule20232024,SC3_schedule20232024,SP1_schedule20232024,SP2_schedule20232024,T1_schedule20232024)

all_schedule20232024 <- all_schedule20232024[,c(7,5,6,3)]

myfixtures <- all_schedule20232024[all_schedule20232024$Date >= '2024-05-17' & all_schedule20232024$Date <= '2024-05-20',]

write.csv(myfixtures,'myfixtures.csv')

View(myfixtures)
