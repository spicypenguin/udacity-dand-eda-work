setwd('/users/jsinclair/Repos/personal/data_analyst_nanodegree/exploratory-data-analysis/')
getwd()
list.files()
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf)

install.packages('ggplot2')
library(ggplot2)
qplot(x = dob_day, data= pf) +
  scale_x_continuous(breaks=1:31) + 
  facet_wrap(~dob_month, ncol = 3)

qplot(x = tenure/365, data = pf,
      color = I('black'), fill= I('#099DD9'),
      binwidth = 0.25, xlab = 'Years tenure') +
  scale_x_continuous(breaks = seq(1, 7, 1), lim = c(0, 7))

qplot(x = age, data = pf, binwidth = 1, color = I('black'), fill= I('#099DD9'))

qplot(x = friend_count, data = pf)
install.packages('gridExtra')
library(gridExtra)

p1 <- ggplot(aes(x = friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()
grid.arrange(p1, p2, p3, ncol=1)

qplot(x = friend_count, y=..count../sum(..count..), data=subset(pf, !is.na(gender)),
      binwidth = 10, geom = 'freqpoly', color=gender) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50))


qplot(x = www_likes, data=subset(pf, !is.na(gender)),
      geom = 'freqpoly', color=gender) +
  scale_x_continuous() + 
  scale_x_log10()
                                    
aggregate(df$www_likes, by=list(Gender=df$gender), FUN=sum)
by(pf$www_likes, pf$gender, FUN=sum)

qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)), geom = 'boxplot',
      ylim = c(0, 1000))

qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)), geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 250))

names(pf)

qplot(x = gender, y = friendships_initiated,
      data = subset(pf, !is.na(gender)), geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 250))

by(pf$friendships_initiated, pf$gender, summary)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

length(pf$mobile_check_in)
sum(pf$mobile_check_in == 1) / length(pf$mobile_check_in)
