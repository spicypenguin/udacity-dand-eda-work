library(ggplot2)
data("diamonds")
summary(diamonds)

ggplot(aes(x=price), data=diamonds) +
  facet_wrap(~color) +
  geom_histogram(aes(color = cut), binwidth=100)
  

ggplot(aes(x=price, y=table), data=diamonds) +
  geom_point(aes(color = cut))

ggplot(aes(x=x*y*z, y=price), 
       data=subset(diamonds, x*y*z < quantile(x*y*z, 0.99))) +
  geom_point(aes(color=clarity)) + 
  scale_y_log10()
  
pf <- read.delim('pseudo_facebook.tsv')
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count
pf$year_joined <- floor(2014 - (pf$tenure / 365))
pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))

ggplot(aes(x=tenure, y=prop_initiated), data=pf) +
  geom_smooth(aes(color=year_joined.bucket))
  
summary(with(subset(pf, year_joined.bucket == 2004), prop_initiated))
library(dplyr)
grouped_data <- pf %>%
  group_by(year_joined) %>%
  summarise(
    mean_prop_initiated = mean(prop_initiated),
    n = n()) %>%
  arrange(year_joined)

ggplot(aes(x=year_joined.bucket, y=prop_initiated), data=pf) +
  geom_bar(stat = 'summary') +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=8)

table(grouped_data)
summary(pf$year_joined.bucket)

ggplot(aes(x=cut, y=price/carat), data=diamonds) +
  facet_wrap(~clarity) +
  geom_point(aes(color=color), position="jitter", alpha=1/2) + 
  scale_color_brewer(type = 'div')
