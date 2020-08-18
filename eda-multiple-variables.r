pf <- read.delim('pseudo_facebook.tsv')
library(dplyr)

pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n()) %>%
  ungroup() %>%
  arrange(age)

head(pf.fc_by_age_gender)

ggplot(aes(x=age, y=mean_friend_count), data=pf.fc_by_age_gender) +
  geom_line(aes(color = gender))

install.packages('reshape2')
library(reshape2)
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                             age ~ gender,
                             value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide)

ggplot(aes(x=age, y=female / male), data=pf.fc_by_age_gender.wide) +
  geom_line() + 
  geom_hline(aes(yintercept=1), linetype=2)

summary(pf)
summary(pf$tenure)
?pf

pf$year_joined <- floor(2014 - (pf$tenure / 365))
summary(pf$year_joined)

table(pf$year_joined)
?cut
pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))
head(pf$year_joined.bucket)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = mean, linetype=2)

friending_rate <- with(subset(pf, tenure > 0), friend_count / tenure)
summary(friending_rate)

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_smooth(aes(color = year_joined.bucket))
                             