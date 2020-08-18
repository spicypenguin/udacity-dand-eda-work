yo <- read.csv('yogurt.csv')
str(yo)

yo$id <- factor(yo$id)
str(yo)

library(ggplot2)
ggplot(aes(x=price), data = yo) +
  geom_histogram(binwidth=10)

summary(yo)
length(unique(yo$price))
table(yo$price)
str(yo)

yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
summary(yo$all.purchases)

ggplot(aes(x=time, y=price), data=yo) +
  geom_jitter(alpha=1/25, shape = 21)

set.seed(4000)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x=time, y=price), 
       data = subset(yo, id %in% sample.ids)) +
  facet_wrap(~id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)
