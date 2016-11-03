setwd("~/Documents/UVA/NBA Hackathon/Data for 2016 NBA Basketball Analytics Hackathon")

library(data.table)
library(stringr)
library(bit64)
library(jpeg)
library(grid)
library(gridExtra)
library(ggplot2)
library(gganimate)
library(dplyr)
library(MASS)
library(mclust)
library(e1071)
library(MASS)
library(caret)

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

# new_player_stats_2015-16.txt
player.stats <- read.csv("new_player_stats_2015-16.txt")
player.tot <- which(player.stats$Tm == "TOT")
player.tot.names <- as.character(player.stats$Player)[player.tot]
player.stats <- player.stats %>% 
  filter(!(as.character(player.stats$Player) %in% player.tot.names & as.character(player.stats$Tm) != "TOT"))

# Opponent Points Per Game.csv
OPPG <- read.csv("Opponent Points Per Game.csv")

#################################################
full.court <- readJPEG("FullCourt.jpeg")

p <- ggplot(data = playoff.16[151:200, ], aes(frame = 1:50)) +
  annotation_custom(rasterGrob(full.court, width=unit(1.2,"npc"), height=unit(1.73,"npc")),
                    0, 94, 0, 50) +
  coord_cartesian(xlim = c(0, 94), ylim = c(0, 50)) +
  geom_label(aes(x = P1X, y = P1Y, label = "1")) +
  geom_label(aes(x = P2X, y = P2Y, label = "2")) +
  geom_label(aes(x = P3X, y = P3Y, label = "3")) +
  geom_label(aes(x = P4X, y = P4Y, label = "4")) +
  geom_label(aes(x = P5X, y = P5Y, label = "5")) +
  geom_point(col = "brown", size = 3, aes(x = BX, y = YB))



gg_animate(p, pause = 0.001)

######################## Outsource Data
fitted.rows <- which(player.stats$G >= 30 & player.stats$MP >= 10)
player.stats.not.scaled <- player.stats %>% 
  dplyr::select(STL, BLK, DRB, MP)%>% 
  as.data.frame()
player.stats.scaled <- scale(player.stats.not.scaled)
player.stats.scaled <- player.stats.scaled[fitted.rows, ]

player.nm <- player.stats %>% 
  dplyr::select(Player)
player.nm <- as.character(player.nm[fitted.rows, ])

# mclust.fit <- Mclust(player.stats.scaled)
# clusters <- data.frame(cluster = unname(mclust.fit$classification))

player.df <- cbind(player.nm, player.stats.scaled)
final.df <- cbind(player.nm, player.stats.not.scaled[fitted.rows,], clusters)
final.df$player.nm <- as.character(final.df$player.nm)
final.df$player.nm  <- gsub("\\\\.*", "", final.df$player.nm)

# subsetting player_team data for 2015-2016 and correcting names for final.df
player.team1516 <- data.frame()
player.team1516 <- player.team[player.team$Person_id %in% sv.box.scores.1516$PERSON_ID, ]
final.df$player.nm %in% player.team1516$Name 
rows_mismatch <- which(final.df$player.nm %in% player.team1516$Name == F)
final.df$player.nm[rows_mismatch]
final.df$player.nm[130] = "PJ Hairston"
final.df$player.nm[131] = "Timothy Hardaway Jr."
final.df$player.nm[220] = "CJ McCollum"
final.df$player.nm[228] = "CJ Miles"
final.df$player.nm[256] = "Kelly Oubre Jr."
final.df$player.nm[280] = "JJ Redick"
final.df$player.nm[330] = "PJ Tucker"
final.df$player.nm[344] = "TJ Warren"
x <- player.team1516 %>% 
  group_by(Name) %>% 
  summarize(Person_id = first(Person_id))
df <- x[x$Name %in% final.df$player.nm,] %>% 
  inner_join(final.df, by = c("Name" = "player.nm")) %>% 
  mutate(Person_id = as.character(Person_id))

############################### Trial
shots.challenged.percentage <- function(shotdata, playermap){
  sd <- shotdata; pm <- playermap
  sd <- data.frame(sd$CLOSE_DEF_PERSON_ID, sd$CLOSE_DEF_DIST)
  colnames(sd)<-c('id','dist')
  sd<-group_by(sd, id) %>%
    summarise(l=length(id),per=sum(dist < 2))
  sd$PERCENT_CONTESTING<-sd$per/sd$l
  map <- unique(data.frame(playermap$Person_id, playermap$Name))
  colnames(map)<-c('id','name')
  re <-merge(sd,map,by="id") %>%
    distinct(id,.keep_all=TRUE)
  return(re)
}

challenge <- shots.challenged.percentage(shot.1516, player.team) %>% 
  dplyr::select(id, PERCENT_CONTESTING)
challenge$id <- as.character(challenge$id)

challenge <- shots.challenged.percentage(shot.1516, player.team) %>% 
  dplyr::select(id, PERCENT_CONTESTING)
challenge$id <- as.character(challenge$id)

trial <- sv.box.scores.1516 %>% 
  dplyr::select(DREB_CHANCE_DEFER, 
                DEF_RIM_FGA, 
                DEF_RIM_FGM,
                PERSON_ID) %>% 
  filter(!DEF_RIM_FGA == 0) %>% 
  mutate(DREB_CHANCE_DEFER = as.numeric(DREB_CHANCE_DEFER),
         DEF_RIM_FGA = as.numeric(DEF_RIM_FGA),
         DEF_RIM_FGM = as.numeric(DEF_RIM_FGM),
         PERSON_ID = PERSON_ID) %>% 
  group_by(PERSON_ID) %>% 
  summarize(DREB_CHANCE_DEFER = sum(DREB_CHANCE_DEFER),
            DEF_RIM_FGA = sum(DEF_RIM_FGA),
            DEF_RIM_FGM = sum(DEF_RIM_FGM),
            FGP = DEF_RIM_FGM/DEF_RIM_FGA) %>% 
  dplyr::select(-(DEF_RIM_FGA:DEF_RIM_FGM)) %>% 
  inner_join(challenge, by = c("PERSON_ID" = "id")) %>% 
  inner_join(df, by = c("PERSON_ID" = "Person_id")) %>% 
  mutate(PERSON_ID = as.factor(PERSON_ID),
         STEAL_RATE = STL/MP,
         BLK_RATE = BLK/MP) %>% 
  dplyr::select(-Name, -cluster, -FGP, -STL, -BLK)


trial.person <- data.frame(PERSON_ID = trial$PERSON_ID)
trial.scaled <- as.data.frame(scale(trial[, 2:ncol(trial)]))

mcluster.fit <- Mclust(trial.scaled)
# output

clusters <- mcluster.fit$classification
trial$PERSON_ID <- as.character(trial$PERSON_ID)
player.team1 <- player.team %>% 
  distinct(Person_id, .keep_all=TRUE)

trial.person$PERSON_ID <- as.character(trial.person$PERSON_ID)
player.cluster <- cbind(trial.person, trial.scaled, data.frame(CLUSTERS = clusters)) %>% 
  inner_join(player.team1, by = c("PERSON_ID" = "Person_id")) %>% 
  dplyr::select(-c(Team_id, Game_id))

df <- player.cluster %>% 
  group_by(CLUSTERS) %>% 
  summarize(BLK_RATE = mean(BLK_RATE), 
            STEAL_RATE = mean(STEAL_RATE), 
            MP = mean(MP),
            DRB = mean(DRB),
            PERCENT_CONTESTING = mean(PERCENT_CONTESTING),
            DREB_CHANCE_DEFER = mean(DREB_CHANCE_DEFER))
# output

table1 <- as.data.frame.list(table(player.cluster$CLUSTERS))
names(table1) <- c(1, 2, 3, 4)
row.names(table1)[1] <- "Total # of Players"

table2 <- table(player.cluster$Team_Name, player.cluster$CLUSTERS) %>% 
  as.data.frame.matrix()
top5 <- as.character(OPPG$TEAM)[1:5]
table3 <- table2[which(row.names(table2) %in% top5),]
x <- unname(apply(table3, 1, sum))
y <- matrix(rep(x, 4), 5, 4)
z <- table3/y
apply(z, 2, mean)
  

  
