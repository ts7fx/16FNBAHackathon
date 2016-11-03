setwd("~/Documents/NBAHackathon/Data")

library(data.table)
library(stringr)
library(bit64)

# Hackathon_nba_2014-15_sv_box_scores.txt
sv.box.scores.1415 <- fread("Hackathon_nba_2014-15_sv_box_scores.txt")

# Hackathon_nba_2015-16_sv_box_scores.txt
sv.box.scores.1516 <- fread("Hackathon_nba_2015-16_sv_box_scores.txt")

# Hackathon_play_by_play.txt
input <- "Hackathon_play_by_play.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(play.by.play <- fread("Hackathon_play_by_play.txt", sep = " ", drop = "V1"), colnames)

# Hackathon_player_names_matched_team.txt
input <- "Hackathon_player_names_matched_team.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(player.team <- fread("Hackathon_player_names_matched_team.txt", sep = " ", drop = "V1"), colnames)

# Hackathon_playoff_hustle_stats_2016.txt
input <- "Hackathon_playoff_hustle_stats_2016.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(hustle.stats.16 <- fread("Hackathon_playoff_hustle_stats_2016.txt", sep = " ", drop = "V1"), colnames)

# Hackathon_sv_possession_summary_2014-15.txt
input <- "Hackathon_sv_possession_summary_2014-15.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(possession.1415 <- fread(input, sep = " ", drop = "V1"), colnames)

# Hackathon_sv_possession_summary_2015-16.txt
input <- "Hackathon_sv_possession_summary_2015-16.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(possession.1516 <- fread(input, sep = " ", drop = "V1"), colnames)

# Hackathon_sv_raw_playoff_2016.txt
input <- "Hackathon_sv_raw_playoff_2016.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(playoff.16 <- fread(input, sep = " ", drop = "V1"), colnames)

# Hackathon_sv_rebound_summary_2014-15.txt
input <- "Hackathon_sv_rebound_summary_2014-15.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(rebound.1415 <- fread(input, sep = " ", drop = "V1"), colnames)

# Hackathon_sv_rebound_summary_2015-16.txt
input <- "Hackathon_sv_rebound_summary_2015-16.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(rebound.1516 <- fread(input, sep = " ", drop = "V1"), colnames)

# Hackathon_sv_shot_summary_2014-15.txt
input <- "Hackathon_sv_shot_summary_2014-15.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(shot.1415 <- fread(input, sep = " ", drop = "V1"), colnames)

# Hackathon_sv_shot_summary_2015-16.txt
input <- "Hackathon_sv_shot_summary_2015-16.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(shot.1516 <- fread(input, sep = " ", drop = "V1"), colnames)

# Hackathon_teamid_link.txt
input <- "Hackathon_teamid_link.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(teamid.link <- fread(input, sep = " ", drop = "V1"), colnames)











