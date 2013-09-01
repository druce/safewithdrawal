
library("reshape")
library("ggplot2")

setwd("c:/Users/druce/ec2/safewithdrawal")

#ce0 = lifespend
efrontier0 <- read.csv("lifespend_v_worst_shortfall.csv")
efrontier0$worst = efrontier0$worst_shortfall
efrontier0$ce = efrontier0$lifespend
keep = c("ce","worst")
plotframe0 = efrontier0[, keep]
plotframe0$frontier = "0"

efrontier1 <- read.csv("ce1_v_worst_shortfall.csv")
efrontier1$worst = efrontier1$worst_shortfall
efrontier1$ce = efrontier1$ce1
plotframe1 = efrontier1[, keep]
plotframe1$frontier = "1"

efrontier2 <- read.csv("ce2_v_worst_shortfall.csv")
efrontier2$worst = efrontier2$worst_shortfall
efrontier2$ce = efrontier2$ce2
plotframe2 = efrontier2[, keep]
plotframe2$frontier = "2"

efrontier4 <- read.csv("ce4_v_worst_shortfall.csv")
efrontier4$worst = efrontier4$worst_shortfall
efrontier4$ce = efrontier4$ce4
plotframe4 = efrontier4[, keep]
plotframe4$frontier = "4"

efrontier8 <- read.csv("ce8_v_worst_shortfall.csv")
efrontier8$worst = efrontier8$worst_shortfall
efrontier8$ce = efrontier8$ce8
plotframe8 = efrontier8[, keep]
plotframe8$frontier = "8"

plotframe <- plotframe0
plotframe <- merge(plotframe, plotframe1, all=TRUE)
plotframe <- merge(plotframe, plotframe2, all=TRUE)
plotframe <- merge(plotframe, plotframe4, all=TRUE)
plotframe <- merge(plotframe, plotframe8, all=TRUE)

mylabs = list(bquote(gamma==0), bquote(gamma==1), bquote(gamma==2), bquote(gamma==4), bquote(gamma==8))
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
    labs(title="Lifetime CE spending vs. worst shortfall, selected gammas", size=12) +
    opts(legend.text=theme_text(size=12), axis.title.x=theme_text(size=12), axis.title.y=theme_text(size=12)) +
    #    geom_point(size=3) +
    geom_line(size=0.5) +
    scale_colour_manual("", breaks = c("0","1","2","4", "8") ,
                        values = c(dvred, dvblue, dvgreen, dvpurple, dvblack),
                        labels =mylabs)
    #    scale_shape_manual("", breaks = c("0","10","20","30","40","50") ,
    #                        values = c(1,2,3,4,5,6),
    #                        labels = c("0","10","20","30","40","50")) +
    #expand_limits(x = 0, y = 0)

