#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

epl_match_report <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier="1st")

bundes_match_report <- fb_match_results(country = "GER", gender = "M", season_end_year = 2024, tier="1st")
########################################################################################################################################
sqldf("SELECT Referee,COUNT(*),AVG(TY) FROM LIGUEONE_spread GROUP BY Referee ORDER BY COUNT(*) desc")
#########################################################################################################################################
View(final_doublefixture_e0)
########################################################################################################################################
library('xlsx')
E0_spreaducl <- readxl::read_excel('../Rsoccer/E0_spread.xlsx')
E0_spreaducl <- E0_spreaducl[,c(-1)]

E0_advstats <- readxl::read_excel('EPL_SPREAD.xlsx')
E0_advstats <- E0_advstats[,-1]
EPL_FINALSPREAD <- dplyr::left_join(E0_spreaducl,EPL_spread)
EPL_FINALSPREAD <- EPL_FINALSPREAD %>% dplyr::relocate(51,.after = 37)
write.xlsx(EPL_FINALSPREAD,'EPL_FINALSPREAD.xlsx')

########################################################################################################################################


###MLS TEST
mls_match_results <- fb_match_results(country = "USA", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(mls_match_results,"mls_match_results.xlsx")
mls_urls <- fb_match_urls(country = "USA", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(mls_urls,"mls_urls.xlsx")

mls_match_sumary <- fb_match_summary(match_url = mls_urls)
write.xlsx(mls_match_sumary,"mls_match_summary.xlsx")

mls_match_shots <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "summary" , team_or_player = "team")

write.xlsx(mls_match_shots,"mls_shots.xlsx")

mls_match_corners <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "passing_types" , team_or_player = "team")

write.xlsx(mls_match_corners,"mls_corners.xlsx")

mls_match_fouls <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "misc" , team_or_player = "team")

write.xlsx(mls_match_fouls,"mls_fouls.xlsx")

