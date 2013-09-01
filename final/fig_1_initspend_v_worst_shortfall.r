
library("reshape")
library("ggplot2")

setwd("c:/Users/druce/ec2/safewithdrawal")

# define colors
dvblue = "#000099"
dvred = "#e41a1c"
dvgreen = "#4daf4a"
dvpurple = "#984ea3"
dvorange = "#ff7f00"
dvyellow = "#ffff33"
dvgray="#666666"

efrontier0 <- read.csv("initspend_v_worst_shortfall.csv")
efrontier0$worst = efrontier0$worst_shortfall
keep = c("initspend","worst")
plotframe0 = efrontier0[, keep]
plotframe0$frontier = "0"

efrontier90 <- read.csv("initspend_v_percentile90.csv")
efrontier90$worst = efrontier90$shortfall_percentile90
plotframe90 = efrontier90[, keep]
plotframe90$frontier = "90"

efrontier80 <- read.csv("initspend_v_percentile80.csv")
efrontier80$worst = efrontier80$shortfall_percentile80
plotframe80 = efrontier80[, keep]
plotframe80$frontier = "80"

efrontier70 <- read.csv("initspend_v_percentile70.csv")
efrontier70$worst = efrontier70$shortfall_percentile70
plotframe70 = efrontier70[, keep]
plotframe70$frontier = "70"

## efrontier60 <- read.csv("initspend_v_percentile60.csv")
## efrontier60$worst = efrontier60$shortfall_percentile60
## plotframe60 = efrontier60[, keep]
## plotframe60$frontier = "60"

## efrontier50 <- read.csv("initspend_v_percentile50.csv")
## efrontier50$worst = efrontier50$shortfall_percentile50
## plotframe50 = efrontier50[, keep]
## plotframe50$frontier = "50"

plotframe <- plotframe0
plotframe <- merge(plotframe, plotframe90, all=TRUE)
plotframe <- merge(plotframe, plotframe80, all=TRUE)
plotframe <- merge(plotframe, plotframe70, all=TRUE)
plotframe <- plotframe[plotframe$worst <=50, ]
#plotframe <- merge(plotframe, plotframe60, all=TRUE)
#plotframe <- merge(plotframe, plotframe50, all=TRUE)


ggplot(data=plotframe, aes(x=worst, y=initspend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
    ylab("Initial spending") +
    xlab("Shortfall %") +
    theme_bw() +
    theme(legend.position="top",
          legend.direction = "horizontal",
          plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid"),
          legend.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid")
    ) +
    labs(title="Initial spending vs. shortfall, selected percentiles", size=16) +
    theme(legend.text=element_text(size=12), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12)) +
    #    geom_point(size=3) +
    geom_line(size=0.5) +
    scale_colour_manual("", breaks = c("0","90","80","70","60","50") ,
                        values = c(dvred, dvblue, dvorange, dvgreen, dvpurple, dvblack),
                        labels = c("Worst shortfall","90th percentile","80th","70th","60th","50th")) +
    expand_limits(y = 0)

    #    scale_shape_manual("", breaks = c("0","10","20","30","40","50") ,
    #                        values = c(1,2,3,4,5,6),
    #                        labels = c("0","10","20","30","40","50")) +


