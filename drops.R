
#load libraries
library(dplyr)
library(ggplot2)
library(ggthemes)

#load dataset
drops <- read.csv("C:/Users/GRA/Desktop/Misc/R Working Directory/Other/NFL/nfl_drops/drops.csv")

#format dataset
drops$rank <- as.numeric(drops$rank)
drops$recep <- as.numeric(drops$recep)
drops$drops <- as.numeric(drops$drops)
drops$targets <- as.numeric(drops$targets)
drops$year2 <- as.factor(drops$year)

#create subsets for plotting
drops.2015 <- drops %>% filter(year == 2015) %>% select(team, drops, flag) %>% group_by(team, flag) %>% summarise(drops = sum(drops))
drop.rate.2015 <- drops %>% filter(year == 2015) %>% select(team, drop.rate, flag) %>% group_by(team, flag) %>% summarise(mean.drop.rate =mean(drop.rate))
position <- drops %>% filter(year == 2015) %>% select(pos, drops, flag) %>% group_by(pos, flag) %>% summarise(drops = sum(drops))
position.rate <- drops %>% filter(year == 2015) %>% select(pos, drop.rate, flag) %>% group_by(pos, flag) %>% summarise(drop.rate = mean(drop.rate))

#various league-wide plots
(team.drops <- ggplot(drops.2015, aes(x=reorder(team, -drops), y=drops, fill = flag))
+ geom_bar(stat = "identity") + scale_fill_manual(values = c("gray37", "darkgreen")) + labs(x="\nTeam", y="Drops\n", title="Drops per Team (2015 Season)\n")
+ guides(fill=FALSE))

(team.drop.rate <- ggplot(drop.rate.2015, aes(x=reorder(team, -mean.drop.rate), y=mean.drop.rate, fill = flag))
+ geom_bar(stat = "identity") + scale_fill_manual(values = c("gray37", "darkgreen")) + labs(x="\nTeam", y="Mean Drop Rate\n", title="Mean Drop Rate per Team (2015 Season)\n")
+ guides(fill=FALSE))

(potition.drops <- ggplot(subset(position, drops>0), aes(x=reorder(pos, -drops), y=drops, fill = pos))
+ geom_bar(stat = "identity") + scale_fill_manual(values = c("gray37", "gray37", "gray37")) + labs(x="\nPosition", y="Drops\n", title="Drops per Position (2015 Season)\n")
+ guides(fill=FALSE))

(potition.drop.rate <- ggplot(subset(position.rate, drop.rate>0), aes(x=reorder(pos, -drop.rate), y=drop.rate, fill = pos))
+ geom_bar(stat = "identity") + scale_fill_manual(values = c("gray37", "gray37", "gray37")) + labs(x="\nPosition", y="League Wide Mean Drop Rate\n", title="Mean Drop Rate per Position (2015 Season)\n")
+ guides(fill=FALSE))


#creating subsets of drops.csv for PHI plotting 
eagles <- drops %>% filter(team == "PHI")
eagles.pos <- eagles %>% select(pos, drops, year2) %>% group_by(pos, year2) %>% summarise(drops = sum(drops))
eagles.rate <- eagles %>% select(pos, drops, drop.rate, year2) %>% group_by(pos, year2) %>% summarise(mean.drop.rate = mean(drop.rate))
eagles.rate.name <- eagles %>% select(pos, drops, drop.rate, player, year2) %>% group_by(pos, player, year2) %>% summarise(mean.drop.rate = mean(drop.rate))
eagles.pos.name <- eagles %>% select(pos, drops, player, year2) %>% group_by(pos, player, year2) %>% summarise(drops = sum(drops))

#various PHI plots
(phi.year <- ggplot(subset(eagles, drops > 0), aes(x=year2, y=drops, fill = year2)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values = c("darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen")) + 
  labs(y="Drops\n", x="", title = "PHI Drops per Year\n") + guides(fill=FALSE))

(phi.pos <- ggplot(subset(eagles.pos, drops > 0), aes(x=year2, y=drops, fill = pos)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values=c("darkgreen", "darkslategray4", "seagreen4")) + 
  labs(y="Drops\n", x="", title = "PHI Drops per Year(by Position)\n") + labs(fill=''))

(phi.pos.rate <- ggplot(subset(eagles.rate, mean.drop.rate > 0), aes(x=year2, y=mean.drop.rate, fill = pos)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values=c("darkgreen", "darkslategray4", "seagreen4")) + 
  labs(y="Mean Drop Rate\n", x="", title = "PHI Mean Drop Rate per Year (by Position) \n") + labs(fill=''))

(phi.player.name <- ggplot(subset(eagles.pos.name, drops > 0 & year2 == 2015), 
  aes(x=reorder(player, -drops), y=drops, fill = pos)) + geom_bar(stat="identity" )+ 
  scale_fill_manual(values=c("darkgreen", "darkslategray4", "seagreen4")) + 
  labs(x="", y="Drops", title = "2015 PHI Drops per Player\n" ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(fill=""))

(phi.player.rate <- ggplot(subset(eagles.rate.name, mean.drop.rate > 0 & year2 == 2015), 
  aes(x=reorder(player, -mean.drop.rate), y=mean.drop.rate, fill = pos)) + geom_bar(stat="identity")+ scale_fill_manual(values=c("darkgreen", 
  "darkslategray4", "seagreen4")) + labs(y="Mean Drop Rate\n", x="", 
  title = "2015 PHI Mean Drop Rate per Player\n") + labs(fill='') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))



