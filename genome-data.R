nci <- read.table('nci.tsv')
colnames(nci) <- c(1:64)

library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp) <- c('gene', 'case', 'value')
head(nci.long.samp)

ggplot(aes(y=gene, x=case, fill=value),
       data=nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c('blue','red'))(100))
