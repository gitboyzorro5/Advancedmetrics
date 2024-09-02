library(tidyverse,quietly=TRUE)
library(rvest,quietly=TRUE)
library(janitor,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(prismatic,quietly=TRUE)
library(broom,quietly=TRUE)
library(purrr,quietly=TRUE)
library(mgsub)


urls = c('https://fbref.com/en/comps/Big5/stats/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/shooting/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/passing/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/passing_types/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/defense/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/gca/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/possession/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/playingtime/players/Big-5-European-Leagues-Stats',
         'https://fbref.com/en/comps/Big5/misc/players/Big-5-European-Leagues-Stats')

stats=read_html(urls[1],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

shooting=read_html(urls[2],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

passing=read_html(urls[3],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

passing_types=read_html(urls[4],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

defence=read_html(urls[5],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

gca=read_html(urls[6],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

possession=read_html(urls[7],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

playingtime=read_html(urls[8],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

misc=read_html(urls[9],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

### Cleaning data - fbref has data entries that are the header repeated for readibility on site - this code removes non player rows all in one
list=list(stats,shooting,passing,passing_types,defence,gca,possession,misc)
# Changes first row to header, then removes duplicate rows of header that fbref uses to display table
list=lapply(list,function(df){
  names(df)=as.matrix(df[1,])
  df=df[-1,]
  df$Rk=as.numeric(df$Rk)
  df=df[!is.na(df$Rk),]
})

### Renaming headings to more clear names and preparing data for merging, removing final column (hyperlink column on fbref), changing characters to integers

# Remove matches column
#list=lapply(list,function(df){
#  df=df[1:length(df)-1]
#})

# Cleans the stats dataframe - comes with some addition stats we dont want
#list[[1]]=subset(list[[1]],select=-(9:11))

# converts data into numeric form
for (i in 1:8){
  list[[i]][,9:length(list[[i]])]=sapply(list[[i]][,9:length(list[[i]])],as.numeric)
}


# NAMING COLUMNS TO AVOID DUPLICATES FOR MERGING
names(list[[1]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','MP','Starts','Min','90s','Gls','Ast','npG','PK','PKA','YCrd','RCrd','Gls p90','Ast p90','GandA p90','npG p90','npGandA p90','xG','npxG','xA','npxG+xA','xG p90','xA p90','xG+xA p90','npxG p90','npxG+xA p90')

names(list[[2]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','Gls','Sh','SoT','SoT%','Sh p90','SoT p90','Gls pSh','Gls pSoT','Sh Dist','Sh FK','PK','PKA','xG','npxG','npxG pSh','G-xG','npG-xG')

names(list[[3]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','PassCmp','PassA','Pass%','PassTotDist','PassProgDist','ShortPassCmp','ShortPassA','ShortPass%','MedPassCmp','MedPassA','MedPass%','LongPassCmp','LongPassA','LongPass%','Ast','xA','A-xA','Key Passes','F3 Passes','PA Passes','PA Crosses','Prog Passes')

names(list[[4]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','PassA','Live Passes','Dead Passes','Passes FK','Thruballs','Pressed Passes','Switches','Crs','CK','InCK','OutCK','StCK','Ground Passes','Low Passes','High Passes','LF Passes','RF Passes','Head Passes','Throwins','OthBP Passes','PassCmp','PassOff','PassOut','PassInt','PassBlockedbyOpp')

names(list[[5]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','TacklesA','TacklesW','TklDef3rd','TklMid3rd','TklAtt3rd','1v1W','1v1A','1v1%','Past','Pressures','Succ_Press','Succ_Press%','PressDef3rd','PressMid3rd','PressAtt3rd','Blocks','ShBlocked','ShSv','PassBlocked','Int','Tkl+Int','Clr','Err')

names(list[[6]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','SCA','SCA90','SCAPassLive','SCAPassDead','SCADrib','SCASh','SCAFld','SCADef','GCA','GCA90','GCAPassLive','GCAPassDead','GCADrib','GCASh','GCAFld','GCADef')

names(list[[7]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','Touches','Touches DefPA','Touches Def3','Touches Mid3','Touches Att3','Touches AttPA','Touches Live','DribCmp','DribA','Drib%','PlayersPast','Megs','Carries','CarryTotDist','CarryProgDist','Prog Carries','F3 Carries','PA Carries','Mis','Dis','RecA','RecCmp','Rec%','Prog Rec')

names(list[[8]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','YCrd','RCrd','2YCrd','Fls','Fld','Off','Crs','Int','TacklesW','PKwon','PKcon','OG','Recov','AerW','AerL','Aer%')

### Clearing duplicate rows - I chose to keep row where player has higher mins for simplicity. You could pick another method to merge rows.
# order by mins, filter duplicates, order by RK
for (i in 1:8){
  list[[i]]=list[[i]][order(list[[i]]$`90s`,decreasing=TRUE),]
  list[[i]]=list[[i]][!duplicated(list[[i]]$Player),]
  list[[i]]=list[[i]][order(list[[i]]$Rk,decreasing=FALSE),]
}

####Merging data into one final data frame - first remove Rk because these are not the same across the data frames (fbref is weird about their Rk columns across multiple tables)

# # Removing Rk
# list=lapply(list,function(df){
#   df=df[,2:length(df)]
# })
#
# # merging dataframes into data21
# data21 = full_join(list[[1]],list[[2]])
# for (i in 3:8){
#   data21 = full_join(data21,list[[i]])
# }
#
# head(data21)
#


playerdata <- as.data.frame(list[[8]])
playerdata2 <-  as.data.frame(list[[1]])
colnames(playerdata2)[18] = "Rcd"
colnames(playerdata2)[19] = "Ycd"

playerdata <- playerdata[1:(length(playerdata)-1)]
playerdata2 <- playerdata2[1:(length(playerdata2)-5)]

eplsquad <- playerdata[playerdata$Comp == "eng Premier League",]
eplsquad2 <- playerdata2[playerdata2$Comp == "eng Premier League",]

bundesligasquad <- playerdata[playerdata$Comp == "de Bundesliga",]
bundesligasquad2 <- playerdata2[playerdata2$Comp == "de Bundesliga",]

serieasquad <- playerdata[playerdata$Comp == "it Serie A",]
serieasquad2 <- playerdata2[playerdata2$Comp == "it Serie A",]

laligasquad <- playerdata[playerdata$Comp == "es La Liga",]
laligasquad2 <- playerdata2[playerdata2$Comp == "es La Liga",]

ligueonesquad <- playerdata[playerdata$Comp == "fr Ligue 1",]
ligueonesquad2 <- playerdata2[playerdata2$Comp == "fr Ligue 1",]
sort(unique(ligueonesquad2$Squad))
f1_teams
eplsquad$Squad <- mgsub(eplsquad$Squad,c("Leicester City","Manchester City","Manchester Utd","Newcastle Utd","Nott'ham Forest","Ipswich Town"),c("Leicester","Man City","Man United","Newcastle","Nottm Forest","Ipswich"))
eplsquad2$Squad <- mgsub(eplsquad2$Squad,c("Leicester City","Manchester City","Manchester Utd","Newcastle Utd","Nott'ham Forest","Ipswich Town"),c("Leicester","Man City","Man United","Newcastle","Nottm Forest","Ipswich"))

bundesligasquad$Squad <- mgsub(bundesligasquad$Squad,c("Mainz 05","Eint Frankfurt","St. Pauli","M'Gladbach","Gladbach"),c("Mainz","Ein Frankfurt","St Pauli","Mgladbach","Mgladbach"))
bundesligasquad2$Squad <- mgsub(bundesligasquad2$Squad,c("Mainz 05","Eint Frankfurt","St. Pauli","M'Gladbach","Gladbach"),c("Mainz","Ein Frankfurt","St Pauli","Mgladbach","Mgladbach"))

serieasquad$Squad <- mgsub(serieasquad$Squad,c("Hellas Verona"),c("Verona"))
serieasquad2$Squad <- mgsub(serieasquad2$Squad,c("Hellas Verona"),c("Verona"))

laligasquad$Squad <- mgsub(laligasquad$Squad,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Rayo Vallecano","Real Sociedad","Leganés"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Vallecano","Sociedad","Leganes"))
laligasquad2$Squad <- mgsub(laligasquad2$Squad,c("Alavés","Almería","Athletic Club","Atlético Madrid","Cádiz","Celta Vigo","Rayo Vallecano","Real Sociedad","Leganés"),c("Alaves","Almeria","Ath Bilbao","Ath Madrid","Cadiz","Celta","Vallecano","Sociedad","Leganes"))

ligueonesquad$Squad <- mgsub(ligueonesquad$Squad,c("Paris S-G","Saint-Étienne"),c("Paris SG","St Etienne"))
ligueonesquad2$Squad <- mgsub(ligueonesquad2$Squad,c("Paris S-G","Saint-Étienne"),c("Paris SG","St Etienne"))



Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
unlink("eplsquad.xlsx")
unlink("eplsquad2.xlsx")
unlink("bundesligasquad.xlsx")
unlink("bundesligasquad2.xlsx")
unlink("serieasquad.xlsx")
unlink("serieasquad2.xlsx")
unlink("laligasquad.xlsx")
unlink("laligasquad2.xlsx")
unlink("ligueonequad.xlsx")
unlink("ligueonesquad2.xlsx")

library('xlsx')
options(java.parameters = "-Xmx4g")

write.xlsx(eplsquad,"eplsquad.xlsx", sheetName = "data")
write.xlsx(eplsquad2,"eplsquad2.xlsx" ,sheetName = "data2")

write.xlsx(bundesligasquad,"bundesligasquad.xlsx", sheetName = "data")
write.xlsx(bundesligasquad2,"bundesligasquad2.xlsx" ,sheetName = "data2")

write.xlsx(serieasquad,"serieasquad.xlsx", sheetName = "data")
write.xlsx(serieasquad2,"serieasquad2.xlsx" ,sheetName = "data2")

write.xlsx(laligasquad,"laligasquad.xlsx", sheetName = "data")
write.xlsx(laligasquad2,"laligasquad2.xlsx" ,sheetName = "data2")

write.xlsx(ligueonesquad,"ligueonequad.xlsx", sheetName = "data")
write.xlsx(ligueonesquad2,"ligueonesquad2.xlsx" ,sheetName = "data2")
