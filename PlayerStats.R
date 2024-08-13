library(tidyverse,quietly=TRUE)
library(rvest,quietly=TRUE)
library(janitor,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(prismatic,quietly=TRUE)
library(broom,quietly=TRUE)
library(purrr,quietly=TRUE)
library(mgsub)
library(stringr)
library(httr)

#B1PLAYERDATA
urls = c('https://fbref.com/en/comps/37/stats/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/shooting/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/passing/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/passing_types/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/defense/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/gca/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/possession/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/playingtime/Belgian-Pro-League-Stats',
         'https://fbref.com/en/comps/37/misc/Belgian-Pro-League-Stats')

html_resp <- GET(urls[1])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
stats = html %>% html_element("table#stats_standard") %>% html_table()

html_resp <- GET(urls[2])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
shooting = html %>% html_element("table#stats_shooting") %>% html_table()

html_resp <- GET(urls[3])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
passing = html %>% html_element("table#stats_passing") %>% html_table()

html_resp <- GET(urls[4])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
passing_types = html %>% html_element("table#stats_passing_types") %>% html_table()

html_resp <- GET(urls[5])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
defence = html %>% html_element("table#stats_defense") %>% html_table()

html_resp <- GET(urls[6])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
gca = html %>% html_element("table#stats_gca") %>% html_table()

html_resp <- GET(urls[7])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
possession = html %>% html_element("table#stats_possession") %>% html_table

html_resp <- GET(urls[8])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
playingtime = html %>% html_element("table#stats_playing_time") %>% html_table

html_resp <- GET(urls[9])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
misc = html %>% html_element("table#stats_misc") %>% html_table


### Cleaning data - fbref has data entries that are the header repeated for readibility on site - this code removes non player rows all in one
list=list(stats,shooting,passing,passing_types,defence,gca,possession,misc)
# Changes first row to header, then removes duplicate rows of header that fbref uses to display table
list=lapply(list,function(df){
  names(df)=as.matrix(df[1,])
  df=df[-1,]
  df$Rk=as.numeric(df$Rk)
  df=df[!is.na(df$Rk),]
})
#View(list[[1]])
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
names(list[[1]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','MP','Starts','Min','Gls','Ast','GlspAst','npG','PK','PKA','YCrd','RCrd','Gls p90','Ast p90','GandA p90','npG p90','npGandA p90','xG','npxG','xA','npxG+xA','xG p90','xA p90','xG+xA p90','npxG p90','npxG+xA p90')

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
b1playerdata <- as.data.frame(list[[8]])
b1playerdata2 <-  as.data.frame(list[[1]])

#colnames(b1playerdata2)[18] = "Rcd"
#colnames(b1playerdata2)[19] = "Ycd"

b1playerdata <- b1playerdata[1:(length(b1playerdata)-1)]
b1playerdata2 <- b1playerdata2[1:(length(b1playerdata2)-5)]

b1squad <- b1playerdata
b1squad2 <- b1playerdata2

b1squad$Squad <- mgsub(b1squad$Squad,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG","Beerschot"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise","Beerschot VA"))
b1squad2$Squad <- mgsub(b1squad2$Squad,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG","Beerschot"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise","Beerschot VA"))

Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
unlink("b1squad.xlsx")
unlink("b1squad2.xlsx")
library('xlsx')
options(java.parameters = "-Xmx4g")
write.xlsx(b1squad,"b1squad.xlsx", sheetName = "data")
write.xlsx(b1squad2,"b1squad2.xlsx" ,sheetName = "data2")
#######################################################################################################################################################################################
########################################################################################################################################################################################
#D2PLAYERDATA
urls = c('https://fbref.com/en/comps/33/stats/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/shooting/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/passing/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/passing_types/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/defense/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/gca/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/possession/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/playingtime/2-Bundesliga-Stats',
         'https://fbref.com/en/comps/33/misc/2-Bundesliga-Stats')

html_resp <- GET(urls[1])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
stats = html %>% html_element("table#stats_standard") %>% html_table()

html_resp <- GET(urls[2])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
shooting = html %>% html_element("table#stats_shooting") %>% html_table()

html_resp <- GET(urls[3])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
passing = html %>% html_element("table#stats_passing") %>% html_table()

html_resp <- GET(urls[4])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
passing_types = html %>% html_element("table#stats_passing_types") %>% html_table()

html_resp <- GET(urls[5])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
defence = html %>% html_element("table#stats_defense") %>% html_table()

html_resp <- GET(urls[6])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
gca = html %>% html_element("table#stats_gca") %>% html_table()

html_resp <- GET(urls[7])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
possession = html %>% html_element("table#stats_possession") %>% html_table

html_resp <- GET(urls[8])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
playingtime = html %>% html_element("table#stats_playing_time") %>% html_table

html_resp <- GET(urls[9])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
misc = html %>% html_element("table#stats_misc") %>% html_table


### Cleaning data - fbref has data entries that are the header repeated for readibility on site - this code removes non player rows all in one
list=list(stats,shooting,passing,passing_types,defence,gca,possession,misc)
# Changes first row to header, then removes duplicate rows of header that fbref uses to display table
list=lapply(list,function(df){
  names(df)=as.matrix(df[1,])
  df=df[-1,]
  df$Rk=as.numeric(df$Rk)
  df=df[!is.na(df$Rk),]
})
#View(list[[8]])
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
names(list[[1]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','MP','Starts','Min','Gls','Ast','GlspAst','npG','PK','PKA','YCrd','RCrd','Gls p90','Ast p90','GandA p90','npG p90','npGandA p90','xG','npxG','xA','npxG+xA','xG p90','xA p90','xG+xA p90','npxG p90','npxG+xA p90')

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
d2playerdata <- as.data.frame(list[[8]])
d2playerdata2 <-  as.data.frame(list[[1]])

#colnames(d2playerdata2)[18] = "Rcd"
#colnames(d2playerdata2)[19] = "Ycd"

d2playerdata <- d2playerdata[1:(length(d2playerdata)-1)]
d2playerdata2 <- d2playerdata2[1:(length(d2playerdata2)-5)]

d2squad <- d2playerdata
d2squad2 <- d2playerdata2

d2squad$Squad <- mgsub(d2squad$Squad,c("Darmstadt 98","Düsseldorf","Greuther Fürth","Hamburger SV","Hannover 96","Hertha BSC","Jahn R'burg","Köln","Nürnberg","Paderborn 07"),c("Darmstadt","Fortuna Dusseldorf","Greuther Furth","Hamburg","Hannover","Hertha","Regensburg","FC Koln","Nurnberg","Paderborn"))
d2squad2$Squad <- mgsub(d2squad2$Squad,c("Darmstadt 98","Düsseldorf","Greuther Fürth","Hamburger SV","Hannover 96","Hertha BSC","Jahn R'burg","Köln","Nürnberg","Paderborn 07"),c("Darmstadt","Fortuna Dusseldorf","Greuther Furth","Hamburg","Hannover","Hertha","Regensburg","FC Koln","Nurnberg","Paderborn"))

Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
unlink("d2squad.xlsx")
unlink("d2squad2.xlsx")
library('xlsx')
options(java.parameters = "-Xmx4g")
write.xlsx(d2squad,"d2squad.xlsx", sheetName = "data")
write.xlsx(d2squad2,"d2squad2.xlsx" ,sheetName = "data2")
########################################################################################################################################################################################
########################################################################################################################################################################################\
#SC0PLAYERDATA
urls = c('https://fbref.com/en/comps/40/stats/Scottish-Premiership-Stats',
         'https://fbref.com/en/comps/40/shooting/Scottish-Premiership-Stats',
         'https://fbref.com/en/comps/40/playingtime/Scottish-Premiership-Stats',
         'https://fbref.com/en/comps/40/misc/Scottish-Premiership-Stats')

html_resp <- GET(urls[1])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
stats = html %>% html_element("table#stats_standard") %>% html_table()

html_resp <- GET(urls[2])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
shooting = html %>% html_element("table#stats_shooting") %>% html_table()

html_resp <- GET(urls[3])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
playingtime = html %>% html_element("table#stats_playing_time") %>% html_table

html_resp <- GET(urls[4])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
misc = html %>% html_element("table#stats_misc") %>% html_table


### Cleaning data - fbref has data entries that are the header repeated for readibility on site - this code removes non player rows all in one
list=list(stats,shooting,misc)
# Changes first row to header, then removes duplicate rows of header that fbref uses to display table
list=lapply(list,function(df){
  names(df)=as.matrix(df[1,])
  df=df[-1,]
  df$Rk=as.numeric(df$Rk)
  df=df[!is.na(df$Rk),]
})
#View(list[[1]])
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
names(list[[1]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','MP','Starts','Min','Gls','Ast','GlspAst','npG','PK','PKA','YCrd','RCrd','Gls p90','Ast p90','GandA p90','npG p90','npGandA p90','xG','npxG','xA','npxG+xA','xG p90','xA p90','xG+xA p90','npxG p90','npxG+xA p90')

names(list[[2]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','Gls','Sh','SoT','SoT%','Sh p90','SoT p90','Gls pSh','Gls pSoT','Sh Dist','Sh FK','PK','PKA','xG','npxG','npxG pSh','G-xG','npG-xG')

#names(list[[3]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','PassCmp','PassA','Pass%','PassTotDist','PassProgDist','ShortPassCmp','ShortPassA','ShortPass%','MedPassCmp','MedPassA','MedPass%','LongPassCmp','LongPassA','LongPass%','Ast','xA','A-xA','Key Passes','F3 Passes','PA Passes','PA Crosses','Prog Passes')

#names(list[[4]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','PassA','Live Passes','Dead Passes','Passes FK','Thruballs','Pressed Passes','Switches','Crs','CK','InCK','OutCK','StCK','Ground Passes','Low Passes','High Passes','LF Passes','RF Passes','Head Passes','Throwins','OthBP Passes','PassCmp','PassOff','PassOut','PassInt','PassBlockedbyOpp')

#names(list[[5]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','TacklesA','TacklesW','TklDef3rd','TklMid3rd','TklAtt3rd','1v1W','1v1A','1v1%','Past','Pressures','Succ_Press','Succ_Press%','PressDef3rd','PressMid3rd','PressAtt3rd','Blocks','ShBlocked','ShSv','PassBlocked','Int','Tkl+Int','Clr','Err')

#names(list[[6]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','SCA','SCA90','SCAPassLive','SCAPassDead','SCADrib','SCASh','SCAFld','SCADef','GCA','GCA90','GCAPassLive','GCAPassDead','GCADrib','GCASh','GCAFld','GCADef')

#names(list[[7]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','Touches','Touches DefPA','Touches Def3','Touches Mid3','Touches Att3','Touches AttPA','Touches Live','DribCmp','DribA','Drib%','PlayersPast','Megs','Carries','CarryTotDist','CarryProgDist','Prog Carries','F3 Carries','PA Carries','Mis','Dis','RecA','RecCmp','Rec%','Prog Rec')

names(list[[3]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','90s','YCrd','RCrd','2YCrd','Fls','Fld','Off','Crs','Int','TacklesW','PKwon','PKcon','OG','Recov','AerW','AerL','Aer%')

### Clearing duplicate rows - I chose to keep row where player has higher mins for simplicity. You could pick another method to merge rows.
# order by mins, filter duplicates, order by RK
for (i in 1:3){
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
sc0playerdata <- as.data.frame(list[[3]])
sc0playerdata2 <-  as.data.frame(list[[1]])

#colnames(sc0playerdata2)[18] = "Rcd"
#colnames(sc0playerdata2)[19] = "Ycd"

sc0playerdata <- sc0playerdata[1:(length(sc0playerdata)-1)]
sc0playerdata2 <- sc0playerdata2[1:(length(sc0playerdata2)-5)]

sc0squad <- sc0playerdata
sc0squad2 <- sc0playerdata2

#sc0squad$Squad <- mgsub(sc0squad$Squad,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG","Beerschot"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise","Beerschot VA"))
#sc0squad2$Squad <- mgsub(sc0squad2$Squad,c("OH Leuven","Sint-Truiden","Standard Liège","Union SG","Beerschot"),c("Oud-Heverlee Leuven","St Truiden","Standard","St. Gilloise","Beerschot VA"))

Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
unlink("sc0squad.xlsx")
unlink("sc0squad2.xlsx")
library('xlsx')
options(java.parameters = "-Xmx4g")
write.xlsx(sc0squad,"sc0squad.xlsx", sheetName = "data")
write.xlsx(sc0squad2,"sc0squad2.xlsx" ,sheetName = "data2")
################################################################################################################################################################################
################################################################################################################################################################################
#E1PLAYERDATA
urls = c('https://fbref.com/en/comps/10/2023-2024/stats/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/shooting/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/passing/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/passing_types/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/defense/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/gca/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/possession/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/playingtime/2023-2024-Championship-Stats',
         'https://fbref.com/en/comps/10/2023-2024/misc/2023-2024-Championship-Stats')

html_resp <- GET(urls[1])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
stats = html %>% html_element("table#stats_standard") %>% html_table()

html_resp <- GET(urls[2])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
shooting = html %>% html_element("table#stats_shooting") %>% html_table()

html_resp <- GET(urls[3])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
passing = html %>% html_element("table#stats_passing") %>% html_table()

html_resp <- GET(urls[4])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
passing_types = html %>% html_element("table#stats_passing_types") %>% html_table()

html_resp <- GET(urls[5])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
defence = html %>% html_element("table#stats_defense") %>% html_table()

html_resp <- GET(urls[6])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
gca = html %>% html_element("table#stats_gca") %>% html_table()

html_resp <- GET(urls[7])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
possession = html %>% html_element("table#stats_possession") %>% html_table

html_resp <- GET(urls[8])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
playingtime = html %>% html_element("table#stats_playing_time") %>% html_table

html_resp <- GET(urls[9])
html <- content(html_resp, as = "text") %>% stringr::str_remove_all("(<!--|-->)") %>% read_html()
misc = html %>% html_element("table#stats_misc") %>% html_table


### Cleaning data - fbref has data entries that are the header repeated for readibility on site - this code removes non player rows all in one
list=list(stats,shooting,passing,passing_types,defence,gca,possession,misc)
# Changes first row to header, then removes duplicate rows of header that fbref uses to display table
list=lapply(list,function(df){
  names(df)=as.matrix(df[1,])
  df=df[-1,]
  df$Rk=as.numeric(df$Rk)
  df=df[!is.na(df$Rk),]
})
#View(list)
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
names(list[[1]])=c('Rk','Player','Nation','Pos','Squad','Comp','Age','Born','MP','Starts','Min','Gls','Ast','GlspAst','npG','PK','PKA','YCrd','RCrd','Gls p90','Ast p90','GandA p90','npG p90','npGandA p90','xG','npxG','xA','npxG+xA','xG p90','xA p90','xG+xA p90','npxG p90','npxG+xA p90')

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
e1playerdata <- as.data.frame(list[[8]])
e1playerdata2 <-  as.data.frame(list[[1]])

#colnames(e1playerdata2)[18] = "Rcd"
#colnames(e1playerdata2)[19] = "Ycd"

e1playerdata <- e1playerdata[1:(length(e1playerdata)-1)]
e1playerdata2 <- e1playerdata2[1:(length(e1playerdata2)-5)]

e1squad <- e1playerdata
e1squad2 <- e1playerdata2

e1squad$Squad <- mgsub(e1squad$Squad,c("Cardiff City","Coventry City","Derby County","Hull City","Leeds United","Luton Town","Norwich City","Oxford United","Plymouth Argyle","Sheffield Utd","Stoke City","Swansea City"),c("Cardiff","Coventry","Derby","Hull","Leeds","Luton","Norwich","Oxford","Plymouth","Sheffield United","Stoke","Swansea"))
e1squad2$Squad <- mgsub(e1squad2$Squad,c("Cardiff City","Coventry City","Derby County","Hull City","Leeds United","Luton Town","Norwich City","Oxford United","Plymouth Argyle","Sheffield Utd","Stoke City","Swansea City"),c("Cardiff","Coventry","Derby","Hull","Leeds","Luton","Norwich","Oxford","Plymouth","Sheffield United","Stoke","Swansea"))

Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
unlink("e1squad.xlsx")
unlink("e1squad2.xlsx")
library('xlsx')
options(java.parameters = "-Xmx4g")
write.xlsx(e1squad,"e1squad.xlsx", sheetName = "data")
write.xlsx(e1squad2,"e1squad2.xlsx" ,sheetName = "data2")
