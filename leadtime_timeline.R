# leadtime_timeline.R

library(ggplot2)

# also look at http://stackoverflow.com/questions/9862519/gantt-style-time-line-plot-in-base-r
# or http://stackoverflow.com/questions/7492274/draw-a-chronological-timeline-with-ggplot2

df1 = data.frame(  rbind(       c(1.2, "Detection from screening", 2),
                                c(2.2, "Detection without screening", 2),
                                c(1, "Detectable \n by testing", 0),
                                c(2, "Symptoms \n begin", 0),
                                c(1.75, "Lead time", 1.2),
                                c(1.5, "Detectable preclinical period", -1.7)
), stringsAsFactors=FALSE)

head(df1)
# see http://stackoverflow.com/questions/14717217/converting-from-a-character-to-a-numeric-data-frame

head(df1)

df2 <- as.data.frame(sapply(df1[,c(1,3)], as.numeric))
class(df2$X1)

df3 <- cbind(df2,X2=df1$X2)
head(df3)

class(df3$X1)

xe <- rep(1,nrow(df3))
ye <- rep(1, nrow(df3))

df4 <- cbind(df3, xe, ye)

seg1 <- as.data.frame( rbind( c(1.2, 0, 1.2, 1.8),
                              c(2.2, 0, 2.2, 1.8),
                              c(1, -2.1, 2, -2.1),
                              c(1.2, 0.9, 2.2, 0.9),
                              c(1, -2.2, 1, 0),
                              c(2, -2.2, 2, 0)
))
names(seg1) <- c("xstart", "ystart", "xe", "ye")
head(seg1)

dev.off()
timeline <- ggplot(seg1, aes(x=xstart, y=ystart, xend=xe, yend=ye)) + 
  geom_segment() +
  geom_hline(yintercept=0, size=1) +  
  theme_bw()
timeline


timeline + geom_text(data=df4, aes(x = X1, y=X3, label = X2)) +
  xlim(c(0.9, 2.3)) + ylim(c(-2.5,2.5))


setEPS()
postscript("P:/Ann/grad_school/classes/epid.710/cheatsheet/part2/leadtime.eps",width=8, height=2.5)

# see http://stackoverflow.com/questions/6528180/ggplot2-plot-without-axes-legends-etc
timeline + geom_text(data=df4, aes(x = X1, y=X3, label = X2)) +
  xlim(c(0.9, 2.3)) +
 theme(axis.line=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    legend.position="none",
                    panel.background=element_blank(),
                    panel.border=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    plot.background=element_blank())
dev.off()


# or look at the following for google vis.
# http://stackoverflow.com/questions/8317584/r-ggplot-time-series-with-events