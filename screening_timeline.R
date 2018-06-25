
# see http://www.r-bloggers.com/version-0-9-of-timeline-on-cran/ for following code
#install.packages('timeline',repos='http://cran.r-project.org')
require(timeline)
library(ggplot2)

data(ww2)
head(ww2)
ww2.events
timeline(ww2, ww2.events, event.spots=2, event.label='', event.above=FALSE)


# Make a timeline for the "Natural history of disease slide"

StartDate <- c(1.5, 2.5, 1, 2, 3, 4, 5, 1.5, 2.5, 4.5)
EndDate   <- c(3.5, 3.5, 2, 3, 4, 5, 6, 2.5, 3.5, 5.5)
group <- c(1, 2, rep(3,5), rep(4,3) )

info <- c("latency", "detectable preclinical", 
           "Biological Onset",
           "Detectable by testing",
           "Symptoms begin",
           "Diagnosed with disease",
           "Becomes disabling",
           "Primary Prevention",
           "Secondary Prevention",
           "Tertiary Prevention"
           )

df2 <- data.frame(info, group, StartDate, EndDate)
df2

timeline(df2) 


ggplot(df2) +
  geom_text( aes(x=StartDate, y=group, label=info), position="jitter") +
  geom_segment( aes(x=StartDate, y=group, xend=StartDate, yend=0, alpha=0.7))

ggplot(df) +
    geom_text( aes(x=StartDate, y=group+0.2, label=info)) +
    geom_segment( aes(x=StartDate, xend=EndDate, y=group, yend=group), size=3)

# also look at http://stackoverflow.com/questions/9862519/gantt-style-time-line-plot-in-base-r
# or http://stackoverflow.com/questions/7492274/draw-a-chronological-timeline-with-ggplot2

df1 = data.frame(  rbind(   c(1.0, "Biological \nonset", 0),
                                c(2, "Detectable\n by testing", 0),
                                c(3, "Symptoms \nbegin", 0),
                                c(4, "Diagnosed\n with disease", 0),
                                c(5, "Becomes\n disabling", 0),
                                c(2.2, "Latency", 0.035),
                                c(2.4, "Detectable\n preclinical", 0.02),
                                c(0.5, "Primary prevention", -0.022),
                                c(2.5, "Secondary prevention", -0.022),
                                c(5, "Tertiary prevention", -0.022)
), stringsAsFactors=FALSE)
head(df1)
# see http://stackoverflow.com/questions/14717217/converting-from-a-character-to-a-numeric-data-frame

head(df1)

df2 <- as.data.frame(sapply(df1[,c(1,3)], as.numeric))
class(df2$X1)

df3 <- cbind(df2,X2=df1$X2)
head(df3)

class(df3$X1)

xe <- rep(0,nrow(df3))
ye <- rep(0, nrow(df3))

df4 <- cbind(df3, xe, ye)

seg1 <- as.data.frame( rbind( c(1, 0.03, 3, 0.03),
                              c(1, 0.03, 1, 0),
                              c(3, 0.03, 3, 0),
                              c(3, 0.02, 3, 0), 
                              c(2, 0.02, 3, 0.02),
                              c(2, 0.02, 2, 0),
                              c(4.5, -0.02 , 4.5, 0),
                              c(5.5, -0.02 , 5.5, 0),
                              c(0.5, -0.02 , 0.5, 0),
                              c(2.5, -0.02 , 2.5, 0),
                              c(4.5, -0.02, 5.5, -0.02)
                              
))
names(seg1) <- c("xstart", "ystart", "xe", "ye")
head(seg1)


timeline <- ggplot(seg1, aes(x=xstart, y=ystart, xend=xe, yend=ye)) + 
  geom_segment() +
  geom_hline(yintercept=0, size=1) +  
  theme_bw()
timeline


dev.off()
timeline + geom_text(data=df4, aes(x = X1, y=X3, label = X2)) 


setEPS()
postscript("timeline.eps",width=8, height=2.5)

# see http://stackoverflow.com/questions/6528180/ggplot2-plot-without-axes-legends-etc
last_plot() + theme(axis.line=element_blank(),
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