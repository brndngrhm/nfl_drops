#load necessary libraries----
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rvest)

#
#
#

#scrape data using rvest package----
url.2015 <- "http://www.sportingcharts.com/nfl/stats/drops/2015/"
url.2014 <- "http://www.sportingcharts.com/nfl/stats/drops/2014/"
url.2013 <- "http://www.sportingcharts.com/nfl/stats/drops/2013/"
url.2012 <- "http://www.sportingcharts.com/nfl/stats/drops/2012/"
url.2011 <- "http://www.sportingcharts.com/nfl/stats/drops/2011/"
url.2010 <- "http://www.sportingcharts.com/nfl/stats/drops/2010/"
url.2009 <- "http://www.sportingcharts.com/nfl/stats/drops/2009/"

drops.2015 <- as.data.frame(url.2015 %>% html() %>% html_nodes(xpath = '//*[@id="ccf80cdbbdfa4d39b6df47dc7cff2fc5"]') %>% html_table())
drops.2014 <- as.data.frame(url.2014 %>% html() %>% html_nodes(xpath = '//*[@id="9b21e1ac7c3e4b7f9d660adc49d12b34"]') %>% html_table())
drops.2013 <- as.data.frame(url.2013 %>% html() %>% html_nodes(xpath = '//*[@id="6ac7439236cf42efbb4d2c798e172abb"]') %>% html_table())
drops.2012 <- as.data.frame(url.2012 %>% html() %>% html_nodes(xpath = '//*[@id="4d2fbd64923b49458959bc235809cfc1"]') %>% html_table())
drops.2011 <- as.data.frame(url.2011 %>% html() %>% html_nodes(xpath = '//*[@id="919bbc4d6342462dac040492062f3c54"]') %>% html_table())
drops.2010 <- as.data.frame(url.2010 %>% html() %>% html_nodes(xpath = '//*[@id="cc4d7cff89c8401aadcd2299a98161cd"]') %>% html_table())
drops.2009 <- as.data.frame(url.2009 %>% html() %>% html_nodes(xpath = '//*[@id="8e4b04f8252a4d99899526d1761789ad"]') %>% html_table())

#Formats and combines data----

#adds year column to each dataframe
drops.2015$year <- "2015"
drops.2014$year <- "2014"
drops.2013$year <- "2013"
drops.2012$year <- "2012"
drops.2011$year <- "2011"
drops.2010$year <- "2010"
drops.2009$year <- "2009"

#combines and formats data frames
drops <- rbind(drops.2009, drops.2010, drops.2011, drops.2012, drops.2013, drops.2014, drops.2015)
names(drops) <- tolower(names(drops))
names(drops)[8] <- "comp.rate"
names(drops)[9] <- "drop.rate"
drops$flag <- "Other"
drops$flag[drops$team == "PHI"] <- "PHI"
drops$player <- as.factor(drops$player)
drops$pos <- as.factor(drops$pos)
drops$team <- as.factor(drops$team)
drops$year <- as.factor(drops$year)
drops$flag <- as.factor(drops$flag)

#formats comp.rate and drop.rate to get rid of % sign and set as numeric
drops$comp.rate <- sub('%$', '', drops$comp.rate)
drops$drop.rate <- sub('%$', '', drops$drop.rate)
drops$comp.rate <- as.numeric(drops$comp.rate)
drops$drop.rate <- as.numeric(drops$drop.rate)
drops$comp.rate <- drops$comp.rate/100
drops$drop.rate <- drops$drop.rate/100

#checks structure
str(drops)

#
#
#

#league-wide plots----

#create subsets for plotting
drops.2015 <- drops %>% filter(year == 2015) %>% select(team, drops, flag) %>% group_by(team, flag) %>% summarise(drops = sum(drops))
drop.rate.2015 <- drops %>% filter(year == 2015) %>% select(team, drop.rate, flag) %>% group_by(team, flag) %>% summarise(mean.drop.rate =mean(drop.rate))
position <- drops %>% filter(year == 2015) %>% select(pos, drops, flag) %>% group_by(pos, flag) %>% summarise(drops = sum(drops))
position.rate <- drops %>% filter(year == 2015) %>% select(pos, drop.rate, flag) %>% group_by(pos, flag) %>% summarise(drop.rate = mean(drop.rate))

#various league-wide plots
(team.drops <- ggplot(drops.2015, aes(x=reorder(team, -drops), y=drops, fill = flag))
+ geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values = c("gray37", "darkgreen")) + labs(x="\nTeam", y="Drops\n", title="Drops per Team (2015 Season)\n")
+ guides(fill=FALSE) + geom_hline(aes(yintercept=mean(drops), alpha=.6)))

(team.drop.rate <- ggplot(drop.rate.2015, aes(x=reorder(team, -mean.drop.rate), y=mean.drop.rate, fill = flag))
+ geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values = c("gray37", "darkgreen")) + labs(x="\nTeam", y="Mean Drop Rate\n", title="Mean Drop Rate per Team (2015 Season)\n")
+ guides(fill=FALSE)+ geom_hline(aes(yintercept=mean(mean.drop.rate), alpha=.6)))

(potition.drops <- ggplot(subset(position, drops>0), aes(x=reorder(pos, -drops), y=drops, fill = pos))
+ geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values = c("gray37", "gray37", "gray37")) + labs(x="\nPosition", y="Drops\n", title="Drops per Position (2015 Season)\n")
+ guides(fill=FALSE)+ geom_hline(aes(yintercept=mean(drops), alpha=.6)))

(potition.drop.rate <- ggplot(subset(position.rate, drop.rate>0), aes(x=reorder(pos, -drop.rate), y=drop.rate, fill = pos))
+ geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values = c("gray37", "gray37", "gray37")) + labs(x="\nPosition", y="League Wide Mean Drop Rate\n", title="Mean Drop Rate per Position (2015 Season)\n")
+ guides(fill=FALSE)+ geom_hline(aes(yintercept=mean(drop.rate), alpha=.6)))

(potition.team.drop.rate <- ggplot(subset(position.rate, drop.rate>0), aes(x=pos, y=drop.rate, fill = flag))
+ geom_bar(stat = "identity", position = "dodge", alpha = .6) + scale_fill_manual(values = c("gray37", "darkgreen"))
+ labs(x = "", y = "Mean Drop Rate\n", title = "Drop Rates - NFL Avg. vs Eagles Avg.\n(2015 Season)\n")
+ guides(fill=FALSE))

#PHL plots----

#creating subsets of drops for PHI plotting 
eagles <- drops %>% filter(team == "PHI")
eagles.pos <- eagles %>% select(pos, drops, year) %>% group_by(pos, year) %>% summarise(drops = sum(drops))
eagles.rate <- eagles %>% select(pos, drops, drop.rate, year) %>% group_by(pos, year) %>% summarise(mean.drop.rate = mean(drop.rate))
eagles.rate.name <- eagles %>% select(pos, drops, drop.rate, player, year) %>% group_by(pos, player, year) %>% summarise(mean.drop.rate = mean(drop.rate))
eagles.pos.name <- eagles %>% select(pos, drops, player, year) %>% group_by(pos, player, year) %>% summarise(drops = sum(drops))
eagles.scatter <- eagles %>% select(pos, drops, targets, year)
  
#various PHI plots
(phi.year <- ggplot(subset(eagles, drops > 0), aes(x=year, y=drops, fill = year)) + 
  geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values = c("darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen")) + 
  labs(y="Drops\n", x="", title = "PHI Drops per Year\n") + guides(fill=FALSE))

(phi.pos <- ggplot(subset(eagles.pos, drops > 0), aes(x=year, y=drops, fill = pos)) + 
  geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values=c("darkgreen", "darkslategray4", "seagreen4")) + 
  labs(y="Drops\n", x="", title = "PHI Drops per Year(by Position)\n") + labs(fill=''))

(phi.pos.rate <- ggplot(subset(eagles.rate, mean.drop.rate > 0), aes(x=year, y=mean.drop.rate, fill = pos)) + 
  geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values=c("darkgreen", "darkslategray4", "seagreen4")) + 
  labs(y="Mean Drop Rate\n", x="", title = "PHI Mean Drop Rate per Year (by Position) \n") + labs(fill=''))

(phi.player.name <- ggplot(subset(eagles.pos.name, drops > 0 & year == 2015), 
  aes(x=reorder(player, -drops), y=drops, fill = pos)) + geom_bar(stat="identity", alpha = .6)+ 
  scale_fill_manual(values=c("darkgreen", "darkslategray4", "seagreen4")) + 
  labs(x="", y="Drops", title = "2015 PHI Drops per Player\n" ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(fill=""))

(phi.player.rate <- ggplot(subset(eagles.rate.name, mean.drop.rate > 0 & year == 2015), 
  aes(x=reorder(player, -mean.drop.rate), y=mean.drop.rate, fill = pos)) + geom_bar(stat="identity", alpha = .6)+ scale_fill_manual(values=c("darkgreen", 
  "darkslategray4", "seagreen4")) + labs(y="Mean Drop Rate\n", x="", 
  title = "2015 PHI Mean Drop Rate per Player\n") + labs(fill='') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(targets <- ggplot(eagles, aes(x=year, y=targets, fill=year)) +
  geom_bar(stat = "identity", alpha = .6) + scale_fill_manual(values = c("darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen")) + 
  labs(y="Targets\n", x="", title = "PHI Targets per Year\n") + guides(fill=FALSE))

#corellation stats
cor(drops$targets, drops$drops)
cor(eagles$targets, eagles$drops)
