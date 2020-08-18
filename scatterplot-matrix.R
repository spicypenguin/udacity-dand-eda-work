install.packages('GGally') 
library(GGally)

theme_set(theme_minimal(20))
set.seed(1836)

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])
