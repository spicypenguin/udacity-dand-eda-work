library(ggplot2)
data(diamonds)

ggplot(aes(x=carat, y=price), data=diamonds) +
  ylim(0, quantile(diamonds$price, 0.99)) +
  xlim(0, quantile(diamonds$carat, 0.99)) +
  geom_point()

install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')
install.packages('plyr')

library(GGally)
library(scales)
library(memisc)

set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

library(gridExtra)

plot1 <- ggplot(aes(x=price), data=diamonds) +
  geom_histogram() +
  ggtitle('Price')

plot2 <- ggplot(aes(x=price), data=diamonds) +
  geom_histogram() +
  scale_x_log10() +
  ggtitle('Price (log10)')

grid.arrange(plot1, plot2, ncol=1)

cuberoot_trans <- function() trans_new('cuberoot', transform = function(x) x^(1/3), inverse = function(x) x^3)

ggplot(aes(carat, price), data = diamonds) + 
  geom_point(alpha=1/2, size=3/4, position='jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

library(RColorBrewer)

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(color=clarity), alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(color=cut), alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')


ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(color=color), alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color',
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data=diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)
