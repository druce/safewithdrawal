
library("reshape")
library("ggplot2")

setwd("c:/Users/druce/ec2/safewithdrawal")

efrontier0 <- read.csv("ce4_v_worst_shortfall.csv")
efrontier0$worst = efrontier0$worst_shortfall
efrontier0$ce = efrontier0$ce4
keep = c("ce","worst")
plotframe0 = efrontier0[, keep]
plotframe0$frontier = "0"

efrontier1 <- read.csv("ce4_v_percentile90.csv")
efrontier1$worst = efrontier1$shortfall_percentile90
efrontier1$ce = efrontier1$ce4
plotframe1 = efrontier1[, keep]
plotframe1$frontier = "90"

efrontier2 <- read.csv("ce4_v_percentile80.csv")
efrontier2$worst = efrontier2$shortfall_percentile80
efrontier2$ce = efrontier2$ce4
plotframe2 = efrontier2[, keep]
plotframe2$frontier = "80"

efrontier3 <- read.csv("ce4_v_percentile70.csv")
efrontier3$worst = efrontier3$shortfall_percentile70
efrontier3$ce = efrontier3$ce4
plotframe3 = efrontier3[, keep]
plotframe3$frontier = "70"

efrontier4 <- read.csv("ce4_v_percentile60.csv")
efrontier4$worst = efrontier4$shortfall_percentile60
efrontier4$ce = efrontier4$ce4
plotframe4 = efrontier4[, keep]
plotframe4$frontier = "60"

efrontier5 <- read.csv("ce4_v_percentile50.csv")
efrontier5$worst = efrontier5$shortfall_percentile50
efrontier5$ce = efrontier5$ce4
plotframe5 = efrontier5[, keep]
plotframe5$frontier = "50"

plotframe <- plotframe0
plotframe <- merge(plotframe, plotframe1, all=TRUE)
plotframe <- merge(plotframe, plotframe2, all=TRUE)
plotframe <- merge(plotframe, plotframe3, all=TRUE)
plotframe <- merge(plotframe, plotframe4, all=TRUE)
plotframe <- merge(plotframe, plotframe5, all=TRUE)

ggplot(data=plotframe, aes(x=worst, y=ce, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Lifetime certainty equivalent spending") +
    xlab("Shortfall %") +
    theme_bw() +
    theme(legend.position="top",
          legend.direction = "horizontal",
          plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid"),
          legend.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid")
    ) +
    labs(title="Lifetime CE spending vs. selected percentile shortfalls (gamma=4)", size=12) +
    opts(legend.text=theme_text(size=12), axis.title.x=theme_text(size=12), axis.title.y=theme_text(size=12)) +
    #    geom_point(size=3) +
    geom_line(size=0.5) +
    scale_colour_manual("", breaks = c("0","90","80","70","60","50") ,
                        values = c(dvred, dvblue, dvorange, dvgreen, dvpurple, dvblack),
                        labels = c("Worst shortfall","90th percentile",">80th","70th","60th","50th (median)"))
    #    scale_shape_manual("", breaks = c("0","10","20","30","40","50") ,
    #                        values = c(1,2,3,4,5,6),
    #                        labels = c("0","10","20","30","40","50")) +
    #expand_limits(x = 0, y = 0)

