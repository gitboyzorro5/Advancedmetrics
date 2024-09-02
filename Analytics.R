library('xlsx')
library('sqldf')
require('RH2')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

####################################################################
#E0
E0_analytics <- readxl::read_excel('EPL_FINALSPREAD.xlsx')
E0_analytics <- E0_analytics[,-1]
E0_analytics <- as.data.frame(E0_analytics)

#Goalmins
E0HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(epl_goalmins) as TGM FROM E0_analytics GROUP BY HomeTeam ORDER BY SUM(epl_goalmins) DESC")
E0AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(epl_goalmins) as TGM FROM E0_analytics GROUP BY AwayTeam ORDER BY SUM(epl_goalmins) DESC")
E0Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(epl_goalmins) as TGM FROM E0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(epl_goalmins) as TGM FROM E0_analytics GROUP BY AwayTeam")
E0Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/38 from E0Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/E0/Goalmins_e0.xlsx')
write.xlsx(E0HomeTeam_goalmins,'Analytics/E0/Goalmins_e0.xlsx', sheetName = "HomeGoalmins")
write.xlsx(E0AwayTeam_goalmins,'Analytics/E0/Goalmins_e0.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(E0Team_goalmins,'Analytics/E0/Goalmins_e0.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
E0HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM E0_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
E0AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM E0_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
E0Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM E0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM E0_analytics GROUP BY AwayTeam")
E0Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/38 from E0Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/E0/Bookings_e0.xlsx')
write.xlsx(E0HomeTeam_Bookings,'Analytics/E0/Bookings_e0.xlsx', sheetName = "HomeBookings")
write.xlsx(E0AwayTeam_Bookings,'Analytics/E0/Bookings_e0.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(E0Team_Bookings,'Analytics/E0/Bookings_e0.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
E0HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM E0_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
E0AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM E0_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
E0Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM E0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM E0_analytics GROUP BY AwayTeam")
E0Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/38 from E0Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/E0/CrossBookings_e0.xlsx')
write.xlsx(E0HomeTeam_CrossBookings,'Analytics/E0/CrossBookings_e0.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(E0AwayTeam_CrossBookings,'Analytics/E0/CrossBookings_e0.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(E0Team_CrossBookings,'Analytics/E0/CrossBookings_e0.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#shirtsXbookings
E0HomeTeam_ShirtsXbookings <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM E0_analytics GROUP BY HomeTeam ORDER BY SUM(ShirtsXbookings) DESC")
E0AwayTeam_ShirtsXbookings <- sqldf("SELECT AwayTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM E0_analytics GROUP BY AwayTeam ORDER BY SUM(ShirtsXbookings) DESC")
E0Team_ShirtsXbookingshomeaway <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM E0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(ShirtsXbookings) as TShirtsXbookings FROM E0_analytics GROUP BY AwayTeam")
E0Team_ShirtsXbookings <- sqldf("SELECT HomeTeam,SUM(TShirtsXbookings),SUM(TShirtsXbookings)/38 from E0Team_ShirtsXbookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TShirtsXbookings) DESC")
unlink('Analytics/E0/ShirtsXbookings_e0.xlsx')
write.xlsx(E0HomeTeam_ShirtsXbookings,'Analytics/E0/ShirtsXbookings_e0.xlsx', sheetName = "HomeShirtsXbookings")
write.xlsx(E0AwayTeam_ShirtsXbookings,'Analytics/E0/ShirtsXbookings_e0.xlsx', sheetName = "AwayShirtsXbookings", append = TRUE)
write.xlsx(E0Team_ShirtsXbookings,'Analytics/E0/ShirtsXbookings_e0.xlsx', sheetName = "TotalShirtsXbookings" , append = TRUE)
#TGMXcorners
E0HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM E0_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
E0AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM E0_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
E0Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM E0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM E0_analytics GROUP BY AwayTeam")
E0Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/38 from E0Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/E0/TGMXcorners_e0.xlsx')
write.xlsx(E0HomeTeam_TGMXcorners,'Analytics/E0/TGMXcorners_e0.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(E0AwayTeam_TGMXcorners,'Analytics/E0/TGMXcorners_e0.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(E0Team_TGMXcorners,'Analytics/E0/TGMXcorners_e0.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
E0HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM E0_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
E0AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM E0_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
E0Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM E0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM E0_analytics GROUP BY AwayTeam")
E0Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/38 from E0Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/E0/MultiBookings_e0.xlsx')
write.xlsx(E0HomeTeam_MultiBookings,'Analytics/E0/MultiBookings_e0.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(E0AwayTeam_MultiBookings,'Analytics/E0/MultiBookings_e0.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(E0Team_MultiBookings,'Analytics/E0/MultiBookings_e0.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#Shirts
E0HomeTeam_epl_shirts <- sqldf("SELECT HomeTeam,sum(epl_shirts) as Tepl_shirts FROM E0_analytics GROUP BY HomeTeam ORDER BY SUM(epl_shirts) DESC")
E0AwayTeam_epl_shirts <- sqldf("SELECT AwayTeam,sum(epl_shirts) as Tepl_shirts FROM E0_analytics GROUP BY AwayTeam ORDER BY SUM(epl_shirts) DESC")
E0Team_epl_shirtshomeaway <- sqldf("SELECT HomeTeam,sum(epl_shirts) as Tepl_shirts FROM E0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(epl_shirts) as Tepl_shirts FROM E0_analytics GROUP BY AwayTeam")
E0Team_epl_shirts <- sqldf("SELECT HomeTeam,SUM(Tepl_shirts),SUM(Tepl_shirts)/38 from E0Team_epl_shirtshomeaway GROUP BY HomeTeam ORDER BY SUM(Tepl_shirts) DESC")
unlink('Analytics/E0/epl_shirts_e0.xlsx')
write.xlsx(E0HomeTeam_epl_shirts,'Analytics/E0/epl_shirts_e0.xlsx', sheetName = "Homeepl_shirts")
write.xlsx(E0AwayTeam_epl_shirts,'Analytics/E0/epl_shirts_e0.xlsx', sheetName = "Awayepl_shirts", append = TRUE)
write.xlsx(E0Team_epl_shirts,'Analytics/E0/epl_shirts_e0.xlsx', sheetName = "Totalepl_shirts" , append = TRUE)
#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM E0_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM E0_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM E0_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM E0_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM E0_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM E0_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/E0/RefereeFirstCard_e0.xlsx')
write.xlsx(one_and15,'Analytics/E0/RefereeFirstCard_e0.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/E0/RefereeFirstCard_e0.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/E0/RefereeFirstCard_e0.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/E0/RefereeFirstCard_e0.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/E0/RefereeFirstCard_e0.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/E0/RefereeFirstCard_e0.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E0_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E0_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E0_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E0_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E0_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM E0_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E0_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E0_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E0_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E0_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E0_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM E0_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/E0/TeamFirstCard_e0.xlsx')

write.xlsx(Hone_and15,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/E0/TeamFirstCard_e0.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
#####################################################################################################################################################################
#####################################################################################################################################################################
#D1
D1_analytics <- readxl::read_excel('BUNDES_FINALSPREAD.xlsx')
D1_analytics <- D1_analytics[,-1]
D1_analytics <- as.data.frame(D1_analytics)
#Goalmins
sqldf("SELECT AVG(bundesliga_goalmins) FROM D1_analytics")
D1HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(bundesliga_goalmins) as TGM FROM D1_analytics GROUP BY HomeTeam ORDER BY SUM(bundesliga_goalmins) DESC")
D1AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(bundesliga_goalmins) as TGM FROM D1_analytics GROUP BY AwayTeam ORDER BY SUM(bundesliga_goalmins) DESC")
D1Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(bundesliga_goalmins) as TGM FROM D1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(bundesliga_goalmins) as TGM FROM D1_analytics GROUP BY AwayTeam")
D1Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/34 from D1Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/D1/Goalmins_d1.xlsx')
write.xlsx(D1HomeTeam_goalmins,'Analytics/D1/Goalmins_d1.xlsx', sheetName = "HomeGoalmins")
write.xlsx(D1AwayTeam_goalmins,'Analytics/D1/Goalmins_d1.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(D1Team_goalmins,'Analytics/D1/Goalmins_d1.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
D1HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM D1_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
D1AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM D1_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
D1Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM D1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM D1_analytics GROUP BY AwayTeam")
D1Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/34 from D1Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/D1/Bookings_d1.xlsx')
write.xlsx(D1HomeTeam_Bookings,'Analytics/D1/Bookings_d1.xlsx', sheetName = "HomeBookings")
write.xlsx(D1AwayTeam_Bookings,'Analytics/D1/Bookings_d1.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(D1Team_Bookings,'Analytics/D1/Bookings_d1.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
D1HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM D1_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
D1AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM D1_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
D1Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM D1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM D1_analytics GROUP BY AwayTeam")
D1Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/34 from D1Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/D1/CrossBookings_d1.xlsx')
write.xlsx(D1HomeTeam_CrossBookings,'Analytics/D1/CrossBookings_d1.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(D1AwayTeam_CrossBookings,'Analytics/D1/CrossBookings_d1.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(D1Team_CrossBookings,'Analytics/D1/CrossBookings_d1.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#shirtsXbookings
D1HomeTeam_ShirtsXbookings <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM D1_analytics GROUP BY HomeTeam ORDER BY SUM(ShirtsXbookings) DESC")
D1AwayTeam_ShirtsXbookings <- sqldf("SELECT AwayTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM D1_analytics GROUP BY AwayTeam ORDER BY SUM(ShirtsXbookings) DESC")
D1Team_ShirtsXbookingshomeaway <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM D1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(ShirtsXbookings) as TShirtsXbookings FROM D1_analytics GROUP BY AwayTeam")
D1Team_ShirtsXbookings <- sqldf("SELECT HomeTeam,SUM(TShirtsXbookings),SUM(TShirtsXbookings)/34 from D1Team_ShirtsXbookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TShirtsXbookings) DESC")
unlink('Analytics/D1/ShirtsXbookings_d1.xlsx')
write.xlsx(D1HomeTeam_ShirtsXbookings,'Analytics/D1/ShirtsXbookings_d1.xlsx', sheetName = "HomeShirtsXbookings")
write.xlsx(D1AwayTeam_ShirtsXbookings,'Analytics/D1/ShirtsXbookings_d1.xlsx', sheetName = "AwayShirtsXbookings", append = TRUE)
write.xlsx(D1Team_ShirtsXbookings,'Analytics/D1/ShirtsXbookings_d1.xlsx', sheetName = "TotalShirtsXbookings" , append = TRUE)
#TGMXcorners
D1HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM D1_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
D1AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM D1_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
D1Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM D1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM D1_analytics GROUP BY AwayTeam")
D1Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/34 from D1Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/D1/TGMXcorners_d1.xlsx')
write.xlsx(D1HomeTeam_TGMXcorners,'Analytics/D1/TGMXcorners_d1.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(D1AwayTeam_TGMXcorners,'Analytics/D1/TGMXcorners_d1.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(D1Team_TGMXcorners,'Analytics/D1/TGMXcorners_d1.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
D1HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM D1_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
D1AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM D1_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
D1Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM D1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM D1_analytics GROUP BY AwayTeam")
D1Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/34 from D1Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/D1/MultiBookings_d1.xlsx')
write.xlsx(D1HomeTeam_MultiBookings,'Analytics/D1/MultiBookings_d1.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(D1AwayTeam_MultiBookings,'Analytics/D1/MultiBookings_d1.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(D1Team_MultiBookings,'Analytics/D1/MultiBookings_d1.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#Shirts
D1HomeTeam_bundesliga_shirts <- sqldf("SELECT HomeTeam,sum(bundesliga_shirts) as Tbundesliga_shirts FROM D1_analytics GROUP BY HomeTeam ORDER BY SUM(bundesliga_shirts) DESC")
D1AwayTeam_bundesliga_shirts <- sqldf("SELECT AwayTeam,sum(bundesliga_shirts) as Tbundesliga_shirts FROM D1_analytics GROUP BY AwayTeam ORDER BY SUM(bundesliga_shirts) DESC")
D1Team_bundesliga_shirtshomeaway <- sqldf("SELECT HomeTeam,sum(bundesliga_shirts) as Tbundesliga_shirts FROM D1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(bundesliga_shirts) as Tbundesliga_shirts FROM D1_analytics GROUP BY AwayTeam")
D1Team_bundesliga_shirts <- sqldf("SELECT HomeTeam,SUM(Tbundesliga_shirts),SUM(Tbundesliga_shirts)/34 from D1Team_bundesliga_shirtshomeaway GROUP BY HomeTeam ORDER BY SUM(Tbundesliga_shirts) DESC")
unlink('Analytics/D1/bundesliga_shirts_d1.xlsx')
write.xlsx(D1HomeTeam_bundesliga_shirts,'Analytics/D1/bundesliga_shirts_d1.xlsx', sheetName = "Homebundesliga_shirts")
write.xlsx(D1AwayTeam_bundesliga_shirts,'Analytics/D1/bundesliga_shirts_d1.xlsx', sheetName = "Awaybundesliga_shirts", append = TRUE)
write.xlsx(D1Team_bundesliga_shirts,'Analytics/D1/bundesliga_shirts_d1.xlsx', sheetName = "Totalbundesliga_shirts" , append = TRUE)
#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM D1_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM D1_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM D1_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM D1_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM D1_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM D1_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/D1/RefereeFirstCard_d1.xlsx')
write.xlsx(one_and15,'Analytics/D1/RefereeFirstCard_d1.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/D1/RefereeFirstCard_d1.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/D1/RefereeFirstCard_d1.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/D1/RefereeFirstCard_d1.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/D1/RefereeFirstCard_d1.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/D1/RefereeFirstCard_d1.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D1_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D1_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D1_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D1_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D1_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM D1_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D1_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D1_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D1_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D1_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D1_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM D1_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/D1/TeamFirstCard_d1.xlsx')

write.xlsx(Hone_and15,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/D1/TeamFirstCard_d1.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
################################################################################################################################################################
#################################################################################################################################################################
#I1
I1_analytics <- readxl::read_excel('SERIEA_FINALSPREAD.xlsx')
I1_analytics <- I1_analytics[,-1]
I1_analytics <- as.data.frame(I1_analytics)
#Goalmins
I1HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(seriea_goalmins) as TGM FROM I1_analytics GROUP BY HomeTeam ORDER BY SUM(seriea_goalmins) DESC")
I1AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(seriea_goalmins) as TGM FROM I1_analytics GROUP BY AwayTeam ORDER BY SUM(seriea_goalmins) DESC")
I1Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(seriea_goalmins) as TGM FROM I1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(seriea_goalmins) as TGM FROM I1_analytics GROUP BY AwayTeam")
I1Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/38 from I1Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/I1/Goalmins_d1.xlsx')
write.xlsx(I1HomeTeam_goalmins,'Analytics/I1/Goalmins_i1.xlsx', sheetName = "HomeGoalmins")
write.xlsx(I1AwayTeam_goalmins,'Analytics/I1/Goalmins_i1.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(I1Team_goalmins,'Analytics/I1/Goalmins_i1.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
I1HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM I1_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
I1AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM I1_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
I1Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM I1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM I1_analytics GROUP BY AwayTeam")
I1Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/38 from I1Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/I1/Bookings_i1.xlsx')
write.xlsx(I1HomeTeam_Bookings,'Analytics/I1/Bookings_i1.xlsx', sheetName = "HomeBookings")
write.xlsx(I1AwayTeam_Bookings,'Analytics/I1/Bookings_i1.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(I1Team_Bookings,'Analytics/I1/Bookings_i1.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
I1HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM I1_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
I1AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM I1_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
I1Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM I1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM I1_analytics GROUP BY AwayTeam")
I1Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/38 from I1Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/I1/CrossBookings_i1.xlsx')
write.xlsx(I1HomeTeam_CrossBookings,'Analytics/I1/CrossBookings_i1.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(I1AwayTeam_CrossBookings,'Analytics/I1/CrossBookings_i1.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(I1Team_CrossBookings,'Analytics/I1/CrossBookings_i1.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#shirtsXbookings
I1HomeTeam_ShirtsXbookings <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM I1_analytics GROUP BY HomeTeam ORDER BY SUM(ShirtsXbookings) DESC")
I1AwayTeam_ShirtsXbookings <- sqldf("SELECT AwayTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM I1_analytics GROUP BY AwayTeam ORDER BY SUM(ShirtsXbookings) DESC")
I1Team_ShirtsXbookingshomeaway <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM I1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(ShirtsXbookings) as TShirtsXbookings FROM I1_analytics GROUP BY AwayTeam")
I1Team_ShirtsXbookings <- sqldf("SELECT HomeTeam,SUM(TShirtsXbookings),SUM(TShirtsXbookings)/38 from I1Team_ShirtsXbookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TShirtsXbookings) DESC")
unlink('Analytics/I1/ShirtsXbookings_i1.xlsx')
write.xlsx(I1HomeTeam_ShirtsXbookings,'Analytics/I1/ShirtsXbookings_i1.xlsx', sheetName = "HomeShirtsXbookings")
write.xlsx(I1AwayTeam_ShirtsXbookings,'Analytics/I1/ShirtsXbookings_i1.xlsx', sheetName = "AwayShirtsXbookings", append = TRUE)
write.xlsx(I1Team_ShirtsXbookings,'Analytics/I1/ShirtsXbookings_i1.xlsx', sheetName = "TotalShirtsXbookings" , append = TRUE)
#TGMXcorners
I1HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM I1_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
I1AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM I1_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
I1Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM I1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM I1_analytics GROUP BY AwayTeam")
I1Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/38 from I1Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/I1/TGMXcorners_i1.xlsx')
write.xlsx(I1HomeTeam_TGMXcorners,'Analytics/I1/TGMXcorners_i1.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(I1AwayTeam_TGMXcorners,'Analytics/I1/TGMXcorners_i1.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(I1Team_TGMXcorners,'Analytics/I1/TGMXcorners_i1.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
I1HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM I1_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
I1AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM I1_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
I1Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM I1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM I1_analytics GROUP BY AwayTeam")
I1Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/38 from I1Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/I1/MultiBookings_i1.xlsx')
write.xlsx(I1HomeTeam_MultiBookings,'Analytics/I1/MultiBookings_i1.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(I1AwayTeam_MultiBookings,'Analytics/I1/MultiBookings_i1.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(I1Team_MultiBookings,'Analytics/I1/MultiBookings_i1.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#Shirts
I1HomeTeam_seriea_shirts <- sqldf("SELECT HomeTeam,sum(seriea_shirts) as Tseriea_shirts FROM I1_analytics GROUP BY HomeTeam ORDER BY SUM(seriea_shirts) DESC")
I1AwayTeam_seriea_shirts <- sqldf("SELECT AwayTeam,sum(seriea_shirts) as Tseriea_shirts FROM I1_analytics GROUP BY AwayTeam ORDER BY SUM(seriea_shirts) DESC")
I1Team_seriea_shirtshomeaway <- sqldf("SELECT HomeTeam,sum(seriea_shirts) as Tseriea_shirts FROM I1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(seriea_shirts) as Tseriea_shirts FROM I1_analytics GROUP BY AwayTeam")
I1Team_seriea_shirts <- sqldf("SELECT HomeTeam,SUM(Tseriea_shirts),SUM(Tseriea_shirts)/38 from I1Team_seriea_shirtshomeaway GROUP BY HomeTeam ORDER BY SUM(Tseriea_shirts) DESC")
unlink('Analytics/I1/seriea_shirts_i1.xlsx')
write.xlsx(I1HomeTeam_seriea_shirts,'Analytics/I1/seriea_shirts_i1.xlsx', sheetName = "Homeseriea_shirts")
write.xlsx(I1AwayTeam_seriea_shirts,'Analytics/I1/seriea_shirts_i1.xlsx', sheetName = "Awayseriea_shirts", append = TRUE)
write.xlsx(I1Team_seriea_shirts,'Analytics/I1/seriea_shirts_i1.xlsx', sheetName = "Totalseriea_shirts" , append = TRUE)
#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM I1_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM I1_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM I1_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM I1_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM I1_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM I1_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/I1/RefereeFirstCard_i1.xlsx')
write.xlsx(one_and15,'Analytics/I1/RefereeFirstCard_i1.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/I1/RefereeFirstCard_i1.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/I1/RefereeFirstCard_i1.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/I1/RefereeFirstCard_i1.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/I1/RefereeFirstCard_i1.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/I1/RefereeFirstCard_i1.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM I1_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT HomeTeam,COUNT(*) FROM I1_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM I1_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM I1_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM I1_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM I1_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM I1_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT AwayTeam,COUNT(*) FROM I1_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM I1_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM I1_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM I1_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM I1_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/I1/TeamFirstCard_i1.xlsx')

write.xlsx(Hone_and15,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/I1/TeamFirstCard_i1.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
##############################################################################################################################################################
##############################################################################################################################################################
#SP1
SP1_analytics <- readxl::read_excel('LALIGA_FINALSPREAD.xlsx')
SP1_analytics <- SP1_analytics[,-1]
SP1_analytics <- as.data.frame(SP1_analytics)
#Goalmins
SP1HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(laliga_goalmins) as TGM FROM SP1_analytics GROUP BY HomeTeam ORDER BY SUM(laliga_goalmins) DESC")
SP1AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(laliga_goalmins) as TGM FROM SP1_analytics GROUP BY AwayTeam ORDER BY SUM(laliga_goalmins) DESC")
SP1Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(laliga_goalmins) as TGM FROM SP1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(laliga_goalmins) as TGM FROM SP1_analytics GROUP BY AwayTeam")
SP1Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/38 from SP1Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/SP1/Goalmins_sp1.xlsx')
write.xlsx(SP1HomeTeam_goalmins,'Analytics/SP1/Goalmins_sp1.xlsx', sheetName = "HomeGoalmins")
write.xlsx(SP1AwayTeam_goalmins,'Analytics/SP1/Goalmins_sp1.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(SP1Team_goalmins,'Analytics/SP1/Goalmins_sp1.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
SP1HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM SP1_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
SP1AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM SP1_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
SP1Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM SP1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM SP1_analytics GROUP BY AwayTeam")
SP1Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/38 from SP1Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/SP1/Bookings_sp1.xlsx')
write.xlsx(SP1HomeTeam_Bookings,'Analytics/SP1/Bookings_sp1.xlsx', sheetName = "HomeBookings")
write.xlsx(SP1AwayTeam_Bookings,'Analytics/SP1/Bookings_sp1.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(SP1Team_Bookings,'Analytics/SP1/Bookings_sp1.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
SP1HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM SP1_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
SP1AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM SP1_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
SP1Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM SP1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM SP1_analytics GROUP BY AwayTeam")
SP1Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/38 from SP1Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/SP1/CrossBookings_sp1.xlsx')
write.xlsx(SP1HomeTeam_CrossBookings,'Analytics/SP1/CrossBookings_sp1.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(SP1AwayTeam_CrossBookings,'Analytics/SP1/CrossBookings_sp1.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(SP1Team_CrossBookings,'Analytics/SP1/CrossBookings_sp1.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#shirtsXbookings
SP1HomeTeam_ShirtsXbookings <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM SP1_analytics GROUP BY HomeTeam ORDER BY SUM(ShirtsXbookings) DESC")
SP1AwayTeam_ShirtsXbookings <- sqldf("SELECT AwayTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM SP1_analytics GROUP BY AwayTeam ORDER BY SUM(ShirtsXbookings) DESC")
SP1Team_ShirtsXbookingshomeaway <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM SP1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(ShirtsXbookings) as TShirtsXbookings FROM SP1_analytics GROUP BY AwayTeam")
SP1Team_ShirtsXbookings <- sqldf("SELECT HomeTeam,SUM(TShirtsXbookings),SUM(TShirtsXbookings)/38 from SP1Team_ShirtsXbookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TShirtsXbookings) DESC")
unlink('Analytics/SP1/ShirtsXbookings_sp1.xlsx')
write.xlsx(SP1HomeTeam_ShirtsXbookings,'Analytics/SP1/ShirtsXbookings_sp1.xlsx', sheetName = "HomeShirtsXbookings")
write.xlsx(SP1AwayTeam_ShirtsXbookings,'Analytics/SP1/ShirtsXbookings_sp1.xlsx', sheetName = "AwayShirtsXbookings", append = TRUE)
write.xlsx(SP1Team_ShirtsXbookings,'Analytics/SP1/ShirtsXbookings_sp1.xlsx', sheetName = "TotalShirtsXbookings" , append = TRUE)
#TGMXcorners
SP1HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM SP1_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
SP1AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM SP1_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
SP1Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM SP1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM SP1_analytics GROUP BY AwayTeam")
SP1Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/38 from SP1Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/SP1/TGMXcorners_sp1.xlsx')
write.xlsx(SP1HomeTeam_TGMXcorners,'Analytics/SP1/TGMXcorners_sp1.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(SP1AwayTeam_TGMXcorners,'Analytics/SP1/TGMXcorners_sp1.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(SP1Team_TGMXcorners,'Analytics/SP1/TGMXcorners_sp1.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
SP1HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM SP1_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
SP1AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM SP1_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
SP1Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM SP1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM SP1_analytics GROUP BY AwayTeam")
SP1Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/38 from SP1Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/SP1/MultiBookings_sp1.xlsx')
write.xlsx(SP1HomeTeam_MultiBookings,'Analytics/SP1/MultiBookings_sp1.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(SP1AwayTeam_MultiBookings,'Analytics/SP1/MultiBookings_sp1.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(SP1Team_MultiBookings,'Analytics/SP1/MultiBookings_sp1.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#Shirts
SP1HomeTeam_laliga_shirts <- sqldf("SELECT HomeTeam,sum(laliga_shirts) as Tlaliga_shirts FROM SP1_analytics GROUP BY HomeTeam ORDER BY SUM(laliga_shirts) DESC")
SP1AwayTeam_laliga_shirts <- sqldf("SELECT AwayTeam,sum(laliga_shirts) as Tlaliga_shirts FROM SP1_analytics GROUP BY AwayTeam ORDER BY SUM(laliga_shirts) DESC")
SP1Team_laliga_shirtshomeaway <- sqldf("SELECT HomeTeam,sum(laliga_shirts) as Tlaliga_shirts FROM SP1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(laliga_shirts) as Tlaliga_shirts FROM SP1_analytics GROUP BY AwayTeam")
SP1Team_laliga_shirts <- sqldf("SELECT HomeTeam,SUM(Tlaliga_shirts),SUM(Tlaliga_shirts)/38 from SP1Team_laliga_shirtshomeaway GROUP BY HomeTeam ORDER BY SUM(Tlaliga_shirts) DESC")
unlink('Analytics/SP1/laliga_shirts_sp1.xlsx')
write.xlsx(SP1HomeTeam_laliga_shirts,'Analytics/SP1/laliga_shirts_sp1.xlsx', sheetName = "Homelaliga_shirts")
write.xlsx(SP1AwayTeam_laliga_shirts,'Analytics/SP1/laliga_shirts_sp1.xlsx', sheetName = "Awaylaliga_shirts", append = TRUE)
write.xlsx(SP1Team_laliga_shirts,'Analytics/SP1/laliga_shirts_sp1.xlsx', sheetName = "Totallaliga_shirts" , append = TRUE)
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM SP1_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM SP1_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM SP1_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM SP1_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM SP1_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM SP1_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/SP1/RefereeFirstCard_sp1.xlsx')
write.xlsx(one_and15,'Analytics/SP1/RefereeFirstCard_sp1.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/SP1/RefereeFirstCard_sp1.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/SP1/RefereeFirstCard_sp1.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/SP1/RefereeFirstCard_sp1.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/SP1/RefereeFirstCard_sp1.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/SP1/RefereeFirstCard_sp1.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SP1_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SP1_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SP1_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SP1_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SP1_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM SP1_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SP1_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SP1_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SP1_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SP1_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SP1_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM SP1_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/SP1/TeamFirstCard_sp1.xlsx')

write.xlsx(Hone_and15,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/SP1/TeamFirstCard_sp1.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
##################################################################################################################################################################################
##################################################################################################################################################################################
#F1
F1_analytics <- readxl::read_excel('LIGUEONE_FINALSPREAD.xlsx')
F1_analytics <- F1_analytics[,-1]
F1_analytics <- as.data.frame(F1_analytics)
#Goalmins
F1HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(ligueone_goalmins) as TGM FROM F1_analytics GROUP BY HomeTeam ORDER BY SUM(ligueone_goalmins) DESC")
F1AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(ligueone_goalmins) as TGM FROM F1_analytics GROUP BY AwayTeam ORDER BY SUM(ligueone_goalmins) DESC")
F1Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(ligueone_goalmins) as TGM FROM F1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(ligueone_goalmins) as TGM FROM F1_analytics GROUP BY AwayTeam")
F1Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/34 from F1Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/F1/Goalmins_f1.xlsx')
write.xlsx(F1HomeTeam_goalmins,'Analytics/F1/Goalmins_f1.xlsx', sheetName = "HomeGoalmins")
write.xlsx(F1AwayTeam_goalmins,'Analytics/F1/Goalmins_f1.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(F1Team_goalmins,'Analytics/F1/Goalmins_f1.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
F1HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM F1_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
F1AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM F1_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
F1Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM F1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM F1_analytics GROUP BY AwayTeam")
F1Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/34 from F1Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/F1/Bookings_f1.xlsx')
write.xlsx(F1HomeTeam_Bookings,'Analytics/F1/Bookings_f1.xlsx', sheetName = "HomeBookings")
write.xlsx(F1AwayTeam_Bookings,'Analytics/F1/Bookings_f1.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(F1Team_Bookings,'Analytics/F1/Bookings_f1.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
F1HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM F1_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
F1AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM F1_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
F1Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM F1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM F1_analytics GROUP BY AwayTeam")
F1Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/34 from F1Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/F1/CrossBookings_f1.xlsx')
write.xlsx(F1HomeTeam_CrossBookings,'Analytics/F1/CrossBookings_f1.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(F1AwayTeam_CrossBookings,'Analytics/F1/CrossBookings_f1.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(F1Team_CrossBookings,'Analytics/F1/CrossBookings_f1.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#shirtsXbookings
F1HomeTeam_ShirtsXbookings <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM F1_analytics GROUP BY HomeTeam ORDER BY SUM(ShirtsXbookings) DESC")
F1AwayTeam_ShirtsXbookings <- sqldf("SELECT AwayTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM F1_analytics GROUP BY AwayTeam ORDER BY SUM(ShirtsXbookings) DESC")
F1Team_ShirtsXbookingshomeaway <- sqldf("SELECT HomeTeam,sum(ShirtsXbookings) as TShirtsXbookings FROM F1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(ShirtsXbookings) as TShirtsXbookings FROM F1_analytics GROUP BY AwayTeam")
F1Team_ShirtsXbookings <- sqldf("SELECT HomeTeam,SUM(TShirtsXbookings),SUM(TShirtsXbookings)/34 from F1Team_ShirtsXbookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TShirtsXbookings) DESC")
unlink('Analytics/F1/ShirtsXbookings_f1.xlsx')
write.xlsx(F1HomeTeam_ShirtsXbookings,'Analytics/F1/ShirtsXbookings_f1.xlsx', sheetName = "HomeShirtsXbookings")
write.xlsx(F1AwayTeam_ShirtsXbookings,'Analytics/F1/ShirtsXbookings_f1.xlsx', sheetName = "AwayShirtsXbookings", append = TRUE)
write.xlsx(F1Team_ShirtsXbookings,'Analytics/F1/ShirtsXbookings_f1.xlsx', sheetName = "TotalShirtsXbookings" , append = TRUE)
#TGMXcorners
F1HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM F1_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
F1AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM F1_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
F1Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM F1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM F1_analytics GROUP BY AwayTeam")
F1Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/34 from F1Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/F1/TGMXcorners_f1.xlsx')
write.xlsx(F1HomeTeam_TGMXcorners,'Analytics/F1/TGMXcorners_f1.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(F1AwayTeam_TGMXcorners,'Analytics/F1/TGMXcorners_f1.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(F1Team_TGMXcorners,'Analytics/F1/TGMXcorners_f1.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
F1HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM F1_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
F1AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM F1_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
F1Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM F1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM F1_analytics GROUP BY AwayTeam")
F1Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/34 from F1Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/F1/MultiBookings_f1.xlsx')
write.xlsx(F1HomeTeam_MultiBookings,'Analytics/F1/MultiBookings_f1.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(F1AwayTeam_MultiBookings,'Analytics/F1/MultiBookings_f1.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(F1Team_MultiBookings,'Analytics/F1/MultiBookings_f1.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#Shirts
F1HomeTeam_ligueone_shirts <- sqldf("SELECT HomeTeam,sum(ligueone_shirts) as Tligueone_shirts FROM F1_analytics GROUP BY HomeTeam ORDER BY SUM(ligueone_shirts) DESC")
F1AwayTeam_ligueone_shirts <- sqldf("SELECT AwayTeam,sum(ligueone_shirts) as Tligueone_shirts FROM F1_analytics GROUP BY AwayTeam ORDER BY SUM(ligueone_shirts) DESC")
F1Team_ligueone_shirtshomeaway <- sqldf("SELECT HomeTeam,sum(ligueone_shirts) as Tligueone_shirts FROM F1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(ligueone_shirts) as Tligueone_shirts FROM F1_analytics GROUP BY AwayTeam")
F1Team_ligueone_shirts <- sqldf("SELECT HomeTeam,SUM(Tligueone_shirts),SUM(Tligueone_shirts)/34 from F1Team_ligueone_shirtshomeaway GROUP BY HomeTeam ORDER BY SUM(Tligueone_shirts) DESC")
unlink('Analytics/F1/ligueone_shirts_f1.xlsx')
write.xlsx(F1HomeTeam_ligueone_shirts,'Analytics/F1/ligueone_shirts_f1.xlsx', sheetName = "Homeligueone_shirts")
write.xlsx(F1AwayTeam_ligueone_shirts,'Analytics/F1/ligueone_shirts_f1.xlsx', sheetName = "Awayligueone_shirts", append = TRUE)
write.xlsx(F1Team_ligueone_shirts,'Analytics/F1/ligueone_shirts_f1.xlsx', sheetName = "Totalligueone_shirts" , append = TRUE)
#RefereeFirstCard
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM F1_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM F1_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM F1_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM F1_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM F1_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM F1_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/F1/RefereeFirstCard_f1.xlsx')
write.xlsx(one_and15,'Analytics/F1/RefereeFirstCard_f1.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/F1/RefereeFirstCard_f1.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/F1/RefereeFirstCard_f1.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/F1/RefereeFirstCard_f1.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/F1/RefereeFirstCard_f1.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/F1/RefereeFirstCard_f1.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM F1_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT HomeTeam,COUNT(*) FROM F1_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM F1_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM F1_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM F1_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM F1_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM F1_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT AwayTeam,COUNT(*) FROM F1_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM F1_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM F1_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM F1_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM F1_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/F1/TeamFirstCard_f1.xlsx')

write.xlsx(Hone_and15,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/F1/TeamFirstCard_f1.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
###################################################################################################################################################################################

###################################################################################################################################################################################
###########OTHER LEAGUES######################
#B1
B1_analytics <- readxl::read_excel('../Rsoccer/B1_SPREAD.xlsx')
B1_analytics <- B1_analytics[,-1]
B1_analytics <- as.data.frame(B1_analytics)

#Goalmins
B1HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM B1_analytics GROUP BY HomeTeam ORDER BY SUM(Total_Goalmins) DESC")
B1AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(Total_Goalmins) as TGM FROM B1_analytics GROUP BY AwayTeam ORDER BY SUM(Total_Goalmins) DESC")
B1Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM B1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Total_Goalmins) as TGM FROM B1_analytics GROUP BY AwayTeam")
B1Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/30 from B1Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/B1/Goalmins_b1.xlsx')
write.xlsx(B1HomeTeam_goalmins,'Analytics/B1/Goalmins_b1.xlsx', sheetName = "HomeGoalmins")
write.xlsx(B1AwayTeam_goalmins,'Analytics/B1/Goalmins_b1.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(B1Team_goalmins,'Analytics/B1/Goalmins_b1.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
B1HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM B1_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
B1AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM B1_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
B1Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM B1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM B1_analytics GROUP BY AwayTeam")
B1Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/30 from B1Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/B1/Bookings_b1.xlsx')
write.xlsx(B1HomeTeam_Bookings,'Analytics/B1/Bookings_b1.xlsx', sheetName = "HomeBookings")
write.xlsx(B1AwayTeam_Bookings,'Analytics/B1/Bookings_b1.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(B1Team_Bookings,'Analytics/B1/Bookings_b1.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
B1HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM B1_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
B1AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM B1_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
B1Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM B1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM B1_analytics GROUP BY AwayTeam")
B1Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/30 from B1Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/B1/CrossBookings_b1.xlsx')
write.xlsx(B1HomeTeam_CrossBookings,'Analytics/B1/CrossBookings_b1.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(B1AwayTeam_CrossBookings,'Analytics/B1/CrossBookings_b1.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(B1Team_CrossBookings,'Analytics/B1/CrossBookings_b1.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#TGMXcorners
B1HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM B1_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
B1AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM B1_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
B1Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM B1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM B1_analytics GROUP BY AwayTeam")
B1Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/30 from B1Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/B1/TGMXcorners_b1.xlsx')
write.xlsx(B1HomeTeam_TGMXcorners,'Analytics/B1/TGMXcorners_b1.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(B1AwayTeam_TGMXcorners,'Analytics/B1/TGMXcorners_b1.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(B1Team_TGMXcorners,'Analytics/B1/TGMXcorners_b1.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
B1HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM B1_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
B1AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM B1_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
B1Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM B1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM B1_analytics GROUP BY AwayTeam")
B1Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/30 from B1Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/B1/MultiBookings_b1.xlsx')
write.xlsx(B1HomeTeam_MultiBookings,'Analytics/B1/MultiBookings_b1.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(B1AwayTeam_MultiBookings,'Analytics/B1/MultiBookings_b1.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(B1Team_MultiBookings,'Analytics/B1/MultiBookings_b1.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM B1_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM B1_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM B1_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM B1_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM B1_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM B1_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/B1/RefereeFirstCard_b1.xlsx')
write.xlsx(one_and15,'Analytics/B1/RefereeFirstCard_b1.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/B1/RefereeFirstCard_b1.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/B1/RefereeFirstCard_b1.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/B1/RefereeFirstCard_b1.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/B1/RefereeFirstCard_b1.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/B1/RefereeFirstCard_b1.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM B1_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT HomeTeam,COUNT(*) FROM B1_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM B1_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM B1_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM B1_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM B1_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM B1_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT AwayTeam,COUNT(*) FROM B1_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM B1_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM B1_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM B1_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM B1_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/B1/TeamFirstCard_b1.xlsx')

write.xlsx(Hone_and15,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/B1/TeamFirstCard_b1.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
###############################################################################################################################################################################
###############################################################################################################################################################################
#D2
D2_analytics <- readxl::read_excel('../Rsoccer/D2_SPREAD.xlsx')
D2_analytics <- D2_analytics[,-1]
D2_analytics <- as.data.frame(D2_analytics)

#Goalmins
D2HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM D2_analytics GROUP BY HomeTeam ORDER BY SUM(Total_Goalmins) DESC")
D2AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(Total_Goalmins) as TGM FROM D2_analytics GROUP BY AwayTeam ORDER BY SUM(Total_Goalmins) DESC")
D2Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM D2_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Total_Goalmins) as TGM FROM D2_analytics GROUP BY AwayTeam")
D2Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/34 from D2Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/D2/Goalmins_d2.xlsx')
write.xlsx(D2HomeTeam_goalmins,'Analytics/D2/Goalmins_d2.xlsx', sheetName = "HomeGoalmins")
write.xlsx(D2AwayTeam_goalmins,'Analytics/D2/Goalmins_d2.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(D2Team_goalmins,'Analytics/D2/Goalmins_d2.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
D2HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM D2_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
D2AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM D2_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
D2Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM D2_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM D2_analytics GROUP BY AwayTeam")
D2Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/34 from D2Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/D2/Bookings_d2.xlsx')
write.xlsx(D2HomeTeam_Bookings,'Analytics/D2/Bookings_d2.xlsx', sheetName = "HomeBookings")
write.xlsx(D2AwayTeam_Bookings,'Analytics/D2/Bookings_d2.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(D2Team_Bookings,'Analytics/D2/Bookings_d2.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
D2HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM D2_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
D2AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM D2_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
D2Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM D2_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM D2_analytics GROUP BY AwayTeam")
D2Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/34 from D2Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/D2/CrossBookings_d2.xlsx')
write.xlsx(D2HomeTeam_CrossBookings,'Analytics/D2/CrossBookings_d2.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(D2AwayTeam_CrossBookings,'Analytics/D2/CrossBookings_d2.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(D2Team_CrossBookings,'Analytics/D2/CrossBookings_d2.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#TGMXcorners
D2HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM D2_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
D2AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM D2_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
D2Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM D2_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM D2_analytics GROUP BY AwayTeam")
D2Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/34 from D2Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/D2/TGMXcorners_d2.xlsx')
write.xlsx(D2HomeTeam_TGMXcorners,'Analytics/D2/TGMXcorners_d2.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(D2AwayTeam_TGMXcorners,'Analytics/D2/TGMXcorners_d2.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(D2Team_TGMXcorners,'Analytics/D2/TGMXcorners_d2.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
D2HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM D2_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
D2AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM D2_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
D2Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM D2_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM D2_analytics GROUP BY AwayTeam")
D2Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/34 from D2Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/D2/MultiBookings_d2.xlsx')
write.xlsx(D2HomeTeam_MultiBookings,'Analytics/D2/MultiBookings_d2.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(D2AwayTeam_MultiBookings,'Analytics/D2/MultiBookings_d2.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(D2Team_MultiBookings,'Analytics/D2/MultiBookings_d2.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM D2_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and34 <- sqldf("SELECT Referee,COUNT(*) FROM D2_analytics WHERE Match_First_YCTime BETWEEN '15' AND '34' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM D2_analytics WHERE Match_First_YCTime BETWEEN '34' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM D2_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM D2_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM D2_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/D2/RefereeFirstCard_d2.xlsx')
write.xlsx(one_and15,'Analytics/D2/RefereeFirstCard_d2.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and34,'Analytics/D2/RefereeFirstCard_d2.xlsx', sheetName = "fifteen_and34", append = TRUE)
write.xlsx(thirty_and45,'Analytics/D2/RefereeFirstCard_d2.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/D2/RefereeFirstCard_d2.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/D2/RefereeFirstCard_d2.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/D2/RefereeFirstCard_d2.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D2_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and34 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D2_analytics WHERE Home_First_YCTime BETWEEN '15' AND '34' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D2_analytics WHERE Home_First_YCTime BETWEEN '34' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D2_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM D2_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM D2_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D2_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and34 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D2_analytics WHERE Away_First_YCTime BETWEEN '15' AND '34' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D2_analytics WHERE Away_First_YCTime BETWEEN '34' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D2_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM D2_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM D2_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/D2/TeamFirstCard_d2.xlsx')

write.xlsx(Hone_and15,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and34,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Hfifteen_and34", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and34,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Afifteen_and34", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/D2/TeamFirstCard_d2.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
####################################################################################################################################################################
######################################################################################################################################################################
#E1
E1_analytics <- readxl::read_excel('../Rsoccer/E1_SPREAD.xlsx')
E1_analytics <- E1_analytics[,-1]
E1_analytics <- as.data.frame(E1_analytics)

#Goalmins
E1HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM E1_analytics GROUP BY HomeTeam ORDER BY SUM(Total_Goalmins) DESC")
E1AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(Total_Goalmins) as TGM FROM E1_analytics GROUP BY AwayTeam ORDER BY SUM(Total_Goalmins) DESC")
E1Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM E1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Total_Goalmins) as TGM FROM E1_analytics GROUP BY AwayTeam")
E1Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/46 from E1Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/E1/Goalmins_e1.xlsx')
write.xlsx(E1HomeTeam_goalmins,'Analytics/E1/Goalmins_e1.xlsx', sheetName = "HomeGoalmins")
write.xlsx(E1AwayTeam_goalmins,'Analytics/E1/Goalmins_e1.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(E1Team_goalmins,'Analytics/E1/Goalmins_e1.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
E1HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM E1_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
E1AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM E1_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
E1Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM E1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM E1_analytics GROUP BY AwayTeam")
E1Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/46 from E1Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/E1/Bookings_e1.xlsx')
write.xlsx(E1HomeTeam_Bookings,'Analytics/E1/Bookings_e1.xlsx', sheetName = "HomeBookings")
write.xlsx(E1AwayTeam_Bookings,'Analytics/E1/Bookings_e1.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(E1Team_Bookings,'Analytics/E1/Bookings_e1.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
E1HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM E1_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
E1AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM E1_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
E1Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM E1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM E1_analytics GROUP BY AwayTeam")
E1Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/46 from E1Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/E1/CrossBookings_e1.xlsx')
write.xlsx(E1HomeTeam_CrossBookings,'Analytics/E1/CrossBookings_e1.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(E1AwayTeam_CrossBookings,'Analytics/E1/CrossBookings_e1.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(E1Team_CrossBookings,'Analytics/E1/CrossBookings_e1.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#TGMXcorners
E1HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM E1_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
E1AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM E1_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
E1Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM E1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM E1_analytics GROUP BY AwayTeam")
E1Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/46 from E1Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/E1/TGMXcorners_e1.xlsx')
write.xlsx(E1HomeTeam_TGMXcorners,'Analytics/E1/TGMXcorners_e1.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(E1AwayTeam_TGMXcorners,'Analytics/E1/TGMXcorners_e1.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(E1Team_TGMXcorners,'Analytics/E1/TGMXcorners_e1.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
E1HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM E1_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
E1AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM E1_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
E1Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM E1_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM E1_analytics GROUP BY AwayTeam")
E1Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/46 from E1Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/E1/MultiBookings_e1.xlsx')
write.xlsx(E1HomeTeam_MultiBookings,'Analytics/E1/MultiBookings_e1.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(E1AwayTeam_MultiBookings,'Analytics/E1/MultiBookings_e1.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(E1Team_MultiBookings,'Analytics/E1/MultiBookings_e1.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM E1_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and46 <- sqldf("SELECT Referee,COUNT(*) FROM E1_analytics WHERE Match_First_YCTime BETWEEN '15' AND '46' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM E1_analytics WHERE Match_First_YCTime BETWEEN '46' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM E1_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM E1_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM E1_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/E1/RefereeFirstCard_e1.xlsx')
write.xlsx(one_and15,'Analytics/E1/RefereeFirstCard_e1.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and46,'Analytics/E1/RefereeFirstCard_e1.xlsx', sheetName = "fifteen_and46", append = TRUE)
write.xlsx(thirty_and45,'Analytics/E1/RefereeFirstCard_e1.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/E1/RefereeFirstCard_e1.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/E1/RefereeFirstCard_e1.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/E1/RefereeFirstCard_e1.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E1_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and46 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E1_analytics WHERE Home_First_YCTime BETWEEN '15' AND '46' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E1_analytics WHERE Home_First_YCTime BETWEEN '46' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E1_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM E1_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM E1_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E1_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and46 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E1_analytics WHERE Away_First_YCTime BETWEEN '15' AND '46' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E1_analytics WHERE Away_First_YCTime BETWEEN '46' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E1_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM E1_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM E1_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/E1/TeamFirstCard_e1.xlsx')

write.xlsx(Hone_and15,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and46,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Hfifteen_and46", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and46,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Afifteen_and46", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/E1/TeamFirstCard_e1.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)
###################################################################################################################################################################
###################################################################################################################################################################
#SC0
SC0_analytics <- readxl::read_excel('../Rsoccer/SC0_SPREAD.xlsx')
SC0_analytics <- SC0_analytics[,-1]
SC0_analytics <- as.data.frame(SC0_analytics)

#Goalmins
SC0HomeTeam_goalmins <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM SC0_analytics GROUP BY HomeTeam ORDER BY SUM(Total_Goalmins) DESC")
SC0AwayTeam_goalmins <- sqldf("SELECT AwayTeam,sum(Total_Goalmins) as TGM FROM SC0_analytics GROUP BY AwayTeam ORDER BY SUM(Total_Goalmins) DESC")
SC0Team_goalminshomeaway <- sqldf("SELECT HomeTeam,sum(Total_Goalmins) as TGM FROM SC0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Total_Goalmins) as TGM FROM SC0_analytics GROUP BY AwayTeam")
SC0Team_goalmins <- sqldf("SELECT HomeTeam,SUM(TGM),SUM(TGM)/38 from SC0Team_goalminshomeaway GROUP BY HomeTeam ORDER BY SUM(TGM) DESC")
unlink('Analytics/SC0/Goalmins_sc0.xlsx')
write.xlsx(SC0HomeTeam_goalmins,'Analytics/SC0/Goalmins_sc0.xlsx', sheetName = "HomeGoalmins")
write.xlsx(SC0AwayTeam_goalmins,'Analytics/SC0/Goalmins_sc0.xlsx', sheetName = "AwayGoalmins", append = TRUE)
write.xlsx(SC0Team_goalmins,'Analytics/SC0/Goalmins_sc0.xlsx', sheetName = "TotalGoalmins" , append = TRUE)
#Bookings
SC0HomeTeam_Bookings <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM SC0_analytics GROUP BY HomeTeam ORDER BY SUM(Bookings) DESC")
SC0AwayTeam_Bookings <- sqldf("SELECT AwayTeam,sum(Bookings) as TBookings FROM SC0_analytics GROUP BY AwayTeam ORDER BY SUM(Bookings) DESC")
SC0Team_Bookingshomeaway <- sqldf("SELECT HomeTeam,sum(Bookings) as TBookings FROM SC0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(Bookings) as TBookings FROM SC0_analytics GROUP BY AwayTeam")
SC0Team_Bookings <- sqldf("SELECT HomeTeam,SUM(TBookings),SUM(TBookings)/38 from SC0Team_Bookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TBookings) DESC")
unlink('Analytics/SC0/Bookings_sc0.xlsx')
write.xlsx(SC0HomeTeam_Bookings,'Analytics/SC0/Bookings_sc0.xlsx', sheetName = "HomeBookings")
write.xlsx(SC0AwayTeam_Bookings,'Analytics/SC0/Bookings_sc0.xlsx', sheetName = "AwayBookings", append = TRUE)
write.xlsx(SC0Team_Bookings,'Analytics/SC0/Bookings_sc0.xlsx', sheetName = "TotalBookings" , append = TRUE)
#Crossbookings
SC0HomeTeam_CrossBookings <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM SC0_analytics GROUP BY HomeTeam ORDER BY SUM(CrossBookings) DESC")
SC0AwayTeam_CrossBookings <- sqldf("SELECT AwayTeam,sum(CrossBookings) as TCrossBookings FROM SC0_analytics GROUP BY AwayTeam ORDER BY SUM(CrossBookings) DESC")
SC0Team_CrossBookingshomeaway <- sqldf("SELECT HomeTeam,sum(CrossBookings) as TCrossBookings FROM SC0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(CrossBookings) as TCrossBookings FROM SC0_analytics GROUP BY AwayTeam")
SC0Team_CrossBookings <- sqldf("SELECT HomeTeam,SUM(TCrossBookings),SUM(TCrossBookings)/38 from SC0Team_CrossBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TCrossBookings) DESC")
unlink('Analytics/SC0/CrossBookings_sc0.xlsx')
write.xlsx(SC0HomeTeam_CrossBookings,'Analytics/SC0/CrossBookings_sc0.xlsx', sheetName = "HomeCrossBookings")
write.xlsx(SC0AwayTeam_CrossBookings,'Analytics/SC0/CrossBookings_sc0.xlsx', sheetName = "AwayCrossBookings", append = TRUE)
write.xlsx(SC0Team_CrossBookings,'Analytics/SC0/CrossBookings_sc0.xlsx', sheetName = "TotalCrossBookings" , append = TRUE)
#TGMXcorners
SC0HomeTeam_TGMXcorners <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM SC0_analytics GROUP BY HomeTeam ORDER BY SUM(TGMXcorners) DESC")
SC0AwayTeam_TGMXcorners <- sqldf("SELECT AwayTeam,sum(TGMXcorners) as TTGMXcorners FROM SC0_analytics GROUP BY AwayTeam ORDER BY SUM(TGMXcorners) DESC")
SC0Team_TGMXcornershomeaway <- sqldf("SELECT HomeTeam,sum(TGMXcorners) as TTGMXcorners FROM SC0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(TGMXcorners) as TTGMXcorners FROM SC0_analytics GROUP BY AwayTeam")
SC0Team_TGMXcorners <- sqldf("SELECT HomeTeam,SUM(TTGMXcorners),SUM(TTGMXcorners)/38 from SC0Team_TGMXcornershomeaway GROUP BY HomeTeam ORDER BY SUM(TTGMXcorners) DESC")
unlink('Analytics/SC0/TGMXcorners_sc0.xlsx')
write.xlsx(SC0HomeTeam_TGMXcorners,'Analytics/SC0/TGMXcorners_sc0.xlsx', sheetName = "HomeTGMXcorners")
write.xlsx(SC0AwayTeam_TGMXcorners,'Analytics/SC0/TGMXcorners_sc0.xlsx', sheetName = "AwayTGMXcorners", append = TRUE)
write.xlsx(SC0Team_TGMXcorners,'Analytics/SC0/TGMXcorners_sc0.xlsx', sheetName = "TotalTGMXcorners" , append = TRUE)
#Multibookings
SC0HomeTeam_MultiBookings <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM SC0_analytics GROUP BY HomeTeam ORDER BY SUM(MultiBookings) DESC")
SC0AwayTeam_MultiBookings <- sqldf("SELECT AwayTeam,sum(MultiBookings) as TMultiBookings FROM SC0_analytics GROUP BY AwayTeam ORDER BY SUM(MultiBookings) DESC")
SC0Team_MultiBookingshomeaway <- sqldf("SELECT HomeTeam,sum(MultiBookings) as TMultiBookings FROM SC0_analytics GROUP BY HomeTeam UNION SELECT AwayTeam,SUM(MultiBookings) as TMultiBookings FROM SC0_analytics GROUP BY AwayTeam")
SC0Team_MultiBookings <- sqldf("SELECT HomeTeam,SUM(TMultiBookings),SUM(TMultiBookings)/38 from SC0Team_MultiBookingshomeaway GROUP BY HomeTeam ORDER BY SUM(TMultiBookings) DESC")
unlink('Analytics/SC0/MultiBookings_sc0.xlsx')
write.xlsx(SC0HomeTeam_MultiBookings,'Analytics/SC0/MultiBookings_sc0.xlsx', sheetName = "HomeMultiBookings")
write.xlsx(SC0AwayTeam_MultiBookings,'Analytics/SC0/MultiBookings_sc0.xlsx', sheetName = "AwayMultiBookings", append = TRUE)
write.xlsx(SC0Team_MultiBookings,'Analytics/SC0/MultiBookings_sc0.xlsx', sheetName = "TotalMultiBookings" , append = TRUE)
#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM SC0_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and38 <- sqldf("SELECT Referee,COUNT(*) FROM SC0_analytics WHERE Match_First_YCTime BETWEEN '15' AND '38' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM SC0_analytics WHERE Match_First_YCTime BETWEEN '38' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM SC0_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM SC0_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM SC0_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/SC0/RefereeFirstCard_sc0.xlsx')
write.xlsx(one_and15,'Analytics/SC0/RefereeFirstCard_sc0.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and38,'Analytics/SC0/RefereeFirstCard_sc0.xlsx', sheetName = "fifteen_and38", append = TRUE)
write.xlsx(thirty_and45,'Analytics/SC0/RefereeFirstCard_sc0.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/SC0/RefereeFirstCard_sc0.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/SC0/RefereeFirstCard_sc0.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/SC0/RefereeFirstCard_sc0.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SC0_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfifteen_and38 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SC0_analytics WHERE Home_First_YCTime BETWEEN '15' AND '38' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SC0_analytics WHERE Home_First_YCTime BETWEEN '38' AND '45' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SC0_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT HomeTeam,COUNT(*) FROM SC0_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT HomeTeam,COUNT(*) FROM SC0_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY HomeTeam ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SC0_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afifteen_and38 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SC0_analytics WHERE Away_First_YCTime BETWEEN '15' AND '38' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SC0_analytics WHERE Away_First_YCTime BETWEEN '38' AND '45' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SC0_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT AwayTeam,COUNT(*) FROM SC0_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT AwayTeam,COUNT(*) FROM SC0_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY AwayTeam ORDER BY COUNT(*) DESC")

unlink('Analytics/SC0/TeamFirstCard_sc0.xlsx')

write.xlsx(Hone_and15,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and38,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Hfifteen_and38", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and38,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Afifteen_and38", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/SC0/TeamFirstCard_sc0.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)

#############################################################################################################################################################################
#############################################################################################################################################################################
#BIGFIVE DATA FRAME
E0_bigfive <- E0_analytics
colnames(E0_bigfive)[39] <- "Total_Goalmins"
colnames(E0_bigfive)[41] <- "Shirts"
D1_bigfive <- D1_analytics
colnames(D1_bigfive)[39] <- "Total_Goalmins"
colnames(D1_bigfive)[41] <- "Shirts"
I1_bigfive <- I1_analytics
colnames(I1_bigfive)[39] <- "Total_Goalmins"
colnames(I1_bigfive)[41] <- "Shirts"
SP1_bigfive <- SP1_analytics
colnames(SP1_bigfive)[39] <- "Total_Goalmins"
colnames(SP1_bigfive)[41] <- "Shirts"
F1_bigfive <- F1_analytics
colnames(F1_bigfive)[39] <- "Total_Goalmins"
colnames(F1_bigfive)[41] <- "Shirts"

#combine all the leagues
BIGFIVE <- rbind(E0_bigfive,D1_bigfive,I1_bigfive,SP1_bigfive,F1_bigfive)
#add firstYCdiff
BIGFIVE$First_YCDiff <- abs(BIGFIVE$match_First_YCTime - BIGFIVE$Away_first_YCTime)

unlink('BIGFIVE20232024.xlsx')
write.xlsx(BIGFIVE,'BIGFIVE20232024.xlsx')
#OTHER LEAGUES
OTHER_LEAGUES <- rbind(B1_analytics,D2_analytics,E1_analytics)
unlink('OTHERLEAGUES20232024.xlsx')
write.xlsx(OTHER_LEAGUES,'OTHERLEAGUES20232024.xlsx')

####################################################################################################################################################################
####################################################################################################################################################################
#big five analytics
BIGFIVE_analytics <- readxl::read_excel('BIGFIVE20232024.xlsx')
BIGFIVE_analytics <- BIGFIVE_analytics[,-1]
BIGFIVE_analytics <- as.data.frame(BIGFIVE_analytics)

#Goalmins
BIGFIVE_goalmins <- sqldf("SELECT Div,sum(Total_Goalmins) as TGM FROM BIGFIVE_analytics GROUP BY Div ORDER BY SUM(Total_Goalmins) DESC")

unlink('Analytics/BIGFIVE/Goalmins_bf.xlsx')
write.xlsx(BIGFIVE_goalmins,'Analytics/BIGFIVE/Goalmins_bf.xlsx', sheetName = "Goalmins")

#Bookings
BIGFIVE_Bookings <- sqldf("SELECT Div,sum(Bookings) as TBookings FROM BIGFIVE_analytics GROUP BY Div ORDER BY SUM(Bookings) DESC")

unlink('Analytics/BIGFIVE/Bookings_bf.xlsx')
write.xlsx(BIGFIVE_Bookings,'Analytics/BIGFIVE/Bookings_bf.xlsx', sheetName = "Bookings")

#Crossbookings
BIGFIVE_CrossBookings <- sqldf("SELECT Div,sum(CrossBookings) as TCrossBookings FROM BIGFIVE_analytics GROUP BY Div ORDER BY SUM(CrossBookings) DESC")

unlink('Analytics/BIGFIVE/CrossBookings_bf.xlsx')
write.xlsx(BIGFIVE_CrossBookings,'Analytics/BIGFIVE/CrossBookings_bf.xlsx', sheetName = "CrossBookings")

#shirtsXbookings
BIGFIVE_ShirtsXbookings <- sqldf("SELECT Div,sum(ShirtsXbookings) as TShirtsXbookings FROM BIGFIVE_analytics GROUP BY Div ORDER BY SUM(ShirtsXbookings) DESC")

unlink('Analytics/BIGFIVE/ShirtsXbookings_bf.xlsx')
write.xlsx(BIGFIVE_ShirtsXbookings,'Analytics/BIGFIVE/ShirtsXbookings_bf.xlsx', sheetName = "ShirtsXbookings")

#TGMXcorners
BIGFIVE_TGMXcorners <- sqldf("SELECT Div,sum(TGMXcorners) as TTGMXcorners FROM BIGFIVE_analytics GROUP BY Div ORDER BY SUM(TGMXcorners) DESC")

unlink('Analytics/BIGFIVE/TGMXcorners_bf.xlsx')
write.xlsx(BIGFIVE_TGMXcorners,'Analytics/BIGFIVE/TGMXcorners_bf.xlsx', sheetName = "TGMXcorners")

#Multibookings
BIGFIVE_MultiBookings <- sqldf("SELECT Div,sum(MultiBookings) as TMultiBookings FROM BIGFIVE_analytics GROUP BY Div ORDER BY SUM(MultiBookings) DESC")

unlink('Analytics/BIGFIVE/MultiBookings_bf.xlsx')
write.xlsx(BIGFIVE_MultiBookings,'Analytics/BIGFIVE/MultiBookings_bf.xlsx', sheetName = "MultiBookings")

#Shirts
BIGFIVE_shirts <- sqldf("SELECT Div,sum(Shirts) as shirts FROM BIGFIVE_analytics GROUP BY Div ORDER BY SUM(Shirts) DESC")

unlink('Analytics/BIGFIVE/epl_shirts_bf.xlsx')
write.xlsx(BIGFIVE_shirts,'Analytics/BIGFIVE/shirts_bf.xlsx', sheetName = "shirts")

#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/BIGFIVE/RefereeFirstCard_bf.xlsx')
write.xlsx(one_and15,'Analytics/BIGFIVE/RefereeFirstCard_bf.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/BIGFIVE/RefereeFirstCard_bf.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/BIGFIVE/RefereeFirstCard_bf.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/BIGFIVE/RefereeFirstCard_bf.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/BIGFIVE/RefereeFirstCard_bf.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/BIGFIVE/RefereeFirstCard_bf.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY Div ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY Div ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY Div ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY Div ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY Div ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY Div ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY Div ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY Div ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY Div ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY Div ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY Div ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY Div ORDER BY COUNT(*) DESC")
#matchfirstcard
Mone_and15 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Div ORDER BY COUNT(*) DESC")
Mfifteen_and30 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Div ORDER BY COUNT(*) DESC")
Mthirty_and45 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Div ORDER BY COUNT(*) DESC")
Mfortyfive_and60 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Div ORDER BY COUNT(*) DESC")
Msixty_and75 <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Div ORDER BY COUNT(*) DESC")
Mseventyfive_tomatchend <- sqldf("SELECT Div,COUNT(*) FROM BIGFIVE_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Div ORDER BY COUNT(*) DESC")


unlink('Analytics/BIGFIVE/TeamFirstCard_bf.xlsx')

write.xlsx(Hone_and15,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)

write.xlsx(Mone_and15,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Mone_and15", append = TRUE)
write.xlsx(Mfifteen_and30,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Mfifteen_and30", append = TRUE)
write.xlsx(Mthirty_and45,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Mthirty_and45", append = TRUE)
write.xlsx(Mfortyfive_and60,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Mfortyfive_and60", append = TRUE)
write.xlsx(Msixty_and75,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Msixty_and75", append = TRUE)
write.xlsx(Mseventyfive_tomatchend,'Analytics/BIGFIVE/TeamFirstCard_bf.xlsx', sheetName = "Mseventyfive_tomatchend", append = TRUE)

##############################################################################################################################################################
##############################################################################################################################################################
#other leagues analytics
OTHERLEAGUES_analytics <- readxl::read_excel('OTHERLEAGUES20232024.xlsx')
OTHERLEAGUES_analytics <- OTHERLEAGUES_analytics[,-1]
OTHERLEAGUES_analytics <- as.data.frame(OTHERLEAGUES_analytics)

#Goalmins
OTHERLEAGUES_goalmins <- sqldf("SELECT Div,sum(Total_Goalmins) as TGM FROM OTHERLEAGUES_analytics GROUP BY Div ORDER BY SUM(Total_Goalmins) DESC")

unlink('Analytics/OTHERLEAGUES/Goalmins_ol.xlsx')
write.xlsx(OTHERLEAGUES_goalmins,'Analytics/OTHERLEAGUES/Goalmins_ol.xlsx', sheetName = "Goalmins")

#Bookings
OTHERLEAGUES_Bookings <- sqldf("SELECT Div,sum(Bookings) as TBookings FROM OTHERLEAGUES_analytics GROUP BY Div ORDER BY SUM(Bookings) DESC")

unlink('Analytics/OTHERLEAGUES/Bookings_ol.xlsx')
write.xlsx(OTHERLEAGUES_Bookings,'Analytics/OTHERLEAGUES/Bookings_ol.xlsx', sheetName = "Bookings")

#Crossbookings
OTHERLEAGUES_CrossBookings <- sqldf("SELECT Div,sum(CrossBookings) as TCrossBookings FROM OTHERLEAGUES_analytics GROUP BY Div ORDER BY SUM(CrossBookings) DESC")

unlink('Analytics/OTHERLEAGUES/CrossBookings_ol.xlsx')
write.xlsx(OTHERLEAGUES_CrossBookings,'Analytics/OTHERLEAGUES/CrossBookings_ol.xlsx', sheetName = "CrossBookings")

#TGMXcorners
OTHERLEAGUES_TGMXcorners <- sqldf("SELECT Div,sum(TGMXcorners) as TTGMXcorners FROM OTHERLEAGUES_analytics GROUP BY Div ORDER BY SUM(TGMXcorners) DESC")

unlink('Analytics/OTHERLEAGUES/TGMXcorners_ol.xlsx')
write.xlsx(OTHERLEAGUES_TGMXcorners,'Analytics/OTHERLEAGUES/TGMXcorners_ol.xlsx', sheetName = "TGMXcorners")

#Multibookings
OTHERLEAGUES_MultiBookings <- sqldf("SELECT Div,sum(MultiBookings) as TMultiBookings FROM OTHERLEAGUES_analytics GROUP BY Div ORDER BY SUM(MultiBookings) DESC")

unlink('Analytics/OTHERLEAGUES/MultiBookings_ol.xlsx')
write.xlsx(OTHERLEAGUES_MultiBookings,'Analytics/OTHERLEAGUES/MultiBookings_ol.xlsx', sheetName = "MultiBookings")

#############################################################################################################################################################
#Referee first card times
one_and15 <- sqldf("SELECT Referee,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Referee ORDER BY COUNT(*) DESC")
fifteen_and30 <- sqldf("SELECT Referee,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Referee ORDER BY COUNT(*) DESC")
thirty_and45 <- sqldf("SELECT Referee,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Referee ORDER BY COUNT(*) DESC")
fortyfive_and60 <- sqldf("SELECT Referee,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Referee ORDER BY COUNT(*) DESC")
sixty_and75 <- sqldf("SELECT Referee,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Referee ORDER BY COUNT(*) DESC")
seventyfive_tomatchend <- sqldf("SELECT Referee,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('Analytics/OTHERLEAGUES/RefereeFirstCard_ol.xlsx')
write.xlsx(one_and15,'Analytics/OTHERLEAGUES/RefereeFirstCard_ol.xlsx', sheetName = "one_and15")
write.xlsx(fifteen_and30,'Analytics/OTHERLEAGUES/RefereeFirstCard_ol.xlsx', sheetName = "fifteen_and30", append = TRUE)
write.xlsx(thirty_and45,'Analytics/OTHERLEAGUES/RefereeFirstCard_ol.xlsx', sheetName = "thirty_and45", append = TRUE)
write.xlsx(fortyfive_and60,'Analytics/OTHERLEAGUES/RefereeFirstCard_ol.xlsx', sheetName = "fortyfive_and60", append = TRUE)
write.xlsx(sixty_and75,'Analytics/OTHERLEAGUES/RefereeFirstCard_ol.xlsx', sheetName = "sixty_and75", append = TRUE)
write.xlsx(seventyfive_tomatchend,'Analytics/OTHERLEAGUES/RefereeFirstCard_ol.xlsx', sheetName = "seventyfive_tomatchend", append = TRUE)
#Team first card at home,away and general
#Home
Hone_and15 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Home_First_YCTime BETWEEN '1' AND '15' GROUP BY Div ORDER BY COUNT(*) DESC")
Hfifteen_and30 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Home_First_YCTime BETWEEN '15' AND '30' GROUP BY Div ORDER BY COUNT(*) DESC")
Hthirty_and45 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Home_First_YCTime BETWEEN '30' AND '45' GROUP BY Div ORDER BY COUNT(*) DESC")
Hfortyfive_and60 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Home_First_YCTime BETWEEN '45' AND '60' GROUP BY Div ORDER BY COUNT(*) DESC")
Hsixty_and75 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Home_First_YCTime BETWEEN '60' AND '75' GROUP BY Div ORDER BY COUNT(*) DESC")
Hseventyfive_tomatchend <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Home_First_YCTime BETWEEN '75' AND '100' GROUP BY Div ORDER BY COUNT(*) DESC")
#Away
Aone_and15 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Away_First_YCTime BETWEEN '1' AND '15' GROUP BY Div ORDER BY COUNT(*) DESC")
Afifteen_and30 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Away_First_YCTime BETWEEN '15' AND '30' GROUP BY Div ORDER BY COUNT(*) DESC")
Athirty_and45 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Away_First_YCTime BETWEEN '30' AND '45' GROUP BY Div ORDER BY COUNT(*) DESC")
Afortyfive_and60 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Away_First_YCTime BETWEEN '45' AND '60' GROUP BY Div ORDER BY COUNT(*) DESC")
Asixty_and75 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Away_First_YCTime BETWEEN '60' AND '75' GROUP BY Div ORDER BY COUNT(*) DESC")
Aseventyfive_tomatchend <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Away_First_YCTime BETWEEN '75' AND '100' GROUP BY Div ORDER BY COUNT(*) DESC")
#matchfirstcard
Mone_and15 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '1' AND '15' GROUP BY Div ORDER BY COUNT(*) DESC")
Mfifteen_and30 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '15' AND '30' GROUP BY Div ORDER BY COUNT(*) DESC")
Mthirty_and45 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '30' AND '45' GROUP BY Div ORDER BY COUNT(*) DESC")
Mfortyfive_and60 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '45' AND '60' GROUP BY Div ORDER BY COUNT(*) DESC")
Msixty_and75 <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '60' AND '75' GROUP BY Div ORDER BY COUNT(*) DESC")
Mseventyfive_tomatchend <- sqldf("SELECT Div,COUNT(*) FROM OTHERLEAGUES_analytics WHERE Match_First_YCTime BETWEEN '75' AND '100' GROUP BY Div ORDER BY COUNT(*) DESC")


unlink('Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx')

write.xlsx(Hone_and15,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Hone_and15")
write.xlsx(Hfifteen_and30,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Hfifteen_and30", append = TRUE)
write.xlsx(Hthirty_and45,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Hthirty_and45", append = TRUE)
write.xlsx(Hfortyfive_and60,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Hfortyfive_and60", append = TRUE)
write.xlsx(Hsixty_and75,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Hsixty_and75", append = TRUE)
write.xlsx(Hseventyfive_tomatchend,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Hseventyfive_tomatchend", append = TRUE)

write.xlsx(Aone_and15,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Aone_and15", append = TRUE)
write.xlsx(Afifteen_and30,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Afifteen_and30", append = TRUE)
write.xlsx(Athirty_and45,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Athirty_and45", append = TRUE)
write.xlsx(Afortyfive_and60,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Afortyfive_and60", append = TRUE)
write.xlsx(Asixty_and75,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Asixty_and75", append = TRUE)
write.xlsx(Aseventyfive_tomatchend,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Aseventyfive_tomatchend", append = TRUE)

write.xlsx(Mone_and15,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Mone_and15", append = TRUE)
write.xlsx(Mfifteen_and30,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Mfifteen_and30", append = TRUE)
write.xlsx(Mthirty_and45,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Mthirty_and45", append = TRUE)
write.xlsx(Mfortyfive_and60,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Mfortyfive_and60", append = TRUE)
write.xlsx(Msixty_and75,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Msixty_and75", append = TRUE)
write.xlsx(Mseventyfive_tomatchend,'Analytics/OTHERLEAGUES/TeamFirstCard_ol.xlsx', sheetName = "Mseventyfive_tomatchend", append = TRUE)

#########################################################################################################################################################################
#############MISC ANALYSI#######################################
BIGFIVE_firstyellowboth1Half <- sqldf("SELECT * FROM BIGFIVE_analytics WHERE Home_first_YCTime > 1 AND Home_first_YCTime <= 45 AND Away_first_YCTime > 1 AND Away_first_YCTime <= 45")
BIGFIVE_firstyellowboth2Half <- sqldf("SELECT * FROM BIGFIVE_analytics WHERE Home_first_YCTime > 45 AND Home_first_YCTime <= 100 AND Away_first_YCTime > 45 AND Away_first_YCTime <= 100")
BIGFIVE_fights <- sqldf("SELECT * FROM BIGFIVE_analytics WHERE Home_first_YCTime = Away_first_YCTime AND Crossbookings > 0")
