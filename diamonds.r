library(ggplot2)
data(diamonds)
summary(diamonds)
names(diamonds)
size(diamonds)
nrow(diamonds)

qplot(x = price, data=diamonds)
summary(diamonds$price)
sum(diamonds$price < 500)
sum(diamonds$price < 250)
sum(diamonds$price >= 15000)


qplot(x = price, data=diamonds, binwidth=10)+
  scale_x_continuous(limits = c(0, 1500))

qplot(x = price, data=diamonds, binwidth=50)+
  facet_wrap(~cut)
by(diamonds$price, diamonds$cut, summary)

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales='free')
qplot(x = price / carat, data = diamonds) + 
  facet_wrap(~cut, scales='free') + 
  scale_x_log10()

names(diamonds)

qplot(x = cut, y = price,
      data = diamonds, geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 7500))
by(diamonds$price, diamonds$color, summary)

qplot(x = color, y = price/carat,
      data = diamonds, geom = 'boxplot') + 
  coord_cartesian(ylim = c(0, 6000))

qplot(x = carat, data=diamonds,
      geom = 'freqpoly', binwidth=0.1) +
  scale_y_continuous(limits = c(2000, 15000))
by(diamonds$carat, sum(diamonds$color!=0), sum)
