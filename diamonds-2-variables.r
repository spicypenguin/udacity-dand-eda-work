library(ggplot2)
data(diamonds)

ggplot(aes(x=x, y=price), data=diamonds) +
  geom_point()

cor.test(x=diamonds$x, y=diamonds$price, method='pearson')
cor.test(x=diamonds$y, y=diamonds$price, method='pearson')
cor.test(x=diamonds$z, y=diamonds$price, method='pearson')

ggplot(aes(x=depth, y=price), data=diamonds) +
  geom_point()

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=1/100)

cor.test(x=diamonds$depth, y=diamonds$price, method='pearson')

ggplot(data = subset(diamonds, carat < quantile(carat, 0.99) & price < quantile(price, 0.99)), aes(x = carat, y = price)) +
  geom_point()

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
head(diamonds$volume)

ggplot(aes(x=volume, y=price), data=diamonds) +
  geom_point()

volume_excluded <- subset(diamonds, diamonds$volume < 800 & diamonds$volume != 0)
cor.test(x=volume_excluded$price, y=volume_excluded$volume, method='pearson')

ggplot(aes(x=volume, y=price), data=volume_excluded) +
  geom_point(alpha=1/20) +
  geom_smooth(method='lm', color='red')

summary(volume_excluded$volume)

library(dplyr)
diamonds.diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)

head(diamonds.diamondsByClarity)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

plot_1 <- ggplot(aes(x=clarity, y=mean_price), data=diamonds_mp_by_clarity) +
  geom_bar(stat="identity")

plot_2 <- ggplot(aes(x=color, y=mean_price), data=diamonds_mp_by_color) +
  geom_bar(stat="identity")

library(gridExtra)
grid.arrange(plot_1, plot_2, ncol=1)
