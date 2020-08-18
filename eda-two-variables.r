setwd('~/Repos/personal/data_analyst_nanodegree/exploratory-data-analysis')

library(ggplot2)
pf <- read.csv("pseudo_facebook.tsv", sep='\t')

qplot(x = age, y = friend_count, data = pf)
qplot(age, friend_count, data=pf)

ggplot(aes(x=age, y=friend_count), data=pf) + 
  geom_point(alpha = 1/20) +
  xlim(13, 90) + 
  coord_trans(y = 'sqrt')

summary(pf$age)

ggplot(aes(x=age, y=friendships_initiated), data=pf) + 
  geom_point(alpha = 1/20) +
  xlim(13, 90) 

install.packages('dplyr')
library(dplyr)
age_groups <- group_by(pf, age)

pf.fc_by_age <- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
pf.fc_by_age <- arrange(pf.fc_by_age, age)

head(pf.fc_by_age)

pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)

head(pf.fc_by_age, 20)

p1 <- ggplot(aes(x=age, y=friend_count_mean), data=subset(pf.fc_by_age, age < 71)) +
  geom_line()


ggplot(aes(x=age, y=friend_count), data=pf) + 
  geom_point(alpha = 1/20,
             position = position_jitter(h = 0),
             color = 'orange') +
  xlim(13, 90) + 
  coord_trans(y = 'sqrt') +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue')


library(Hmisc)
x <- pf$age
y <- pf$friend_count
cor.test(x=pf$age, y=pf$friend_count, method="pearson")


ggplot(aes(x=www_likes_received, y=likes_received), data=pf) + 
  geom_point(alpha=1/5) +
  xlim(0, quantile(pf$www_likes_received, 0.95)) + 
  ylim(0, quantile(pf$likes_received, 0.95)) + 
  geom_smooth(method='lm', color='red')
  

with(subset(pf, 
            www_likes_received < quantile(www_likes_received, 0.95) &&
            likes_received < quantile(likes_received, 0.95)),
    cor.test(x=www_likes_received, y=likes_received, method="pearson")) 

install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell
Mitchell

ggplot(aes(x=Month, y=Temp), data=Mitchell) +
  geom_point() +
  scale_x_discrete(breaks = seq(0, 203, 12) )

summary(Mitchell$Month)

cor.test(x=Mitchell$Month, y=Mitchell$Temp, method='pearson')

pf$age_with_months <- pf$age + (12-pf$dob_month) / 12

pf.fc_by_age_months <- pf %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age_with_months)

p2 <- ggplot(aes(x=age_with_months, y=friend_count_mean), data=subset(pf.fc_by_age_months, age_with_months < 71)) +
  geom_line()


library(gridExtra)
grid.arrange(p2, p1, ncol=1)
