input <- "Hackathon_sv_shot_summary_2015-16.txt"
colnames <- str_replace_all(str_split(readLines(input, n = 1), '\" ')[[1]], "[\"]", "")
setnames(shot.1516 <- fread(input, sep = " ", drop = "V1"), colnames)

d <- shot.1516
range(shot.1516$CLOSE_DEF_SV_PLAYER_ID)
range(shot.1516$CLOSE_DEF_PERSON_ID)
class(shot.1516$CLOSE_DEF_PERSON_ID)

d<-data.frame(d$CLOSE_DEF_PERSON_ID, d$CLOSE_DEF_DIST)
colnames(d)<-c('id','dist')


library(dplyr)

v<-group_by(d, id) %>%
  summarise(l=length(id),per=sum(dist < 2)) 
w<-group_by(d, id) %>%
  summarise(per=sum(dist < 2)) 


%>%
  mutate(per = sum(d$dist<=2)/length(id))

e <- unique(data.frame(player.team$Person_id, player.team$Name))
f <- unique(e)
colnames(e)<-c('id','name')
k <-merge(e,v,by="id")
k<-unique(k)

# compute the percentage of shots challenged by all defensive players
library(dplyr)
shots.challenged.percentage <- function(shotdata, playermap){
  sd <- shotdata; pm <- playermap
  sd <- data.frame(sd$CLOSE_DEF_PERSON_ID, sd$CLOSE_DEF_DIST)
  colnames(sd)<-c('id','dist')
  sd<-group_by(sd, id) %>%
    summarise(l=length(id),per=sum(dist < 2))
  sd$percent<-sd$per/sd$l
  map <- unique(data.frame(playermap$Person_id, playermap$Name))
  colnames(map)<-c('id','name')
  re <-merge(sd,map,by="id") %>%
     
  return(re)
}

a <- shots.challenged.percentage(shot.1516,player.team)
test<-unique(test,by='id')
class(unique(test$id))


###################
temp<-sv.box.scores.1516
temp<-data.frame(as.numeric(sv.box.scores.1516$DEF_RIM_FGM),as.numeric(sv.box.scores.1516$DEF_RIM_FGA), sv.box.scores.1516$PERSON_ID)
colnames(temp) <- c('FGM','FGA','id')
temp$FGM <- as.numeric(temp$FGM)
temp$FGA <- as.numeric(temp$FGA)
class(temp$FGM)
temp$FGP<-(temp$FGM) / (temp$FGA)
head(temp,6)
max(temp$FGP)
#remove NaNs
temp <- temp[complete.cases(temp), ]

lalala<-group_by(temp, id) %>%
  summarise(totalFGM = sum(FGM), totalFGA = sum(FGA))
lalala$FGP<-lalala$totalFGM/lalala$totalFGA


defense.effectiveness <- function(boxdata, playermap){
  
  
}





#use a:
new <- merge(a,lalala, by='id')





player<-read.csv("p.csv")
player <- group_by(player, Team_Name)

spurs <- player[player$Team_Name == "San Antonio Spurs",] %>%
  group_by(CLUSTERS) %>%
  summarise(count = length(CLUSTERS))

spurs <- player[player$Team_Name == "San Antonio Spurs",] %>%
  group_by(CLUSTERS) %>%
  summarise(count = length(CLUSTERS))

spurs <- group_by(player, Team_Name, CLUSTERS) %>%
  summarise(count = length(CLUSTERS)) %>%
  mutate(percent = count / sum(count))


