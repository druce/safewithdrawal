
library("reshape")
library("ggplot2")

setwd("c:/Users/druce/ec2/safewithdrawal")

efrontier <- read.csv("initspend_v_probmaxdecline0.csv")
efrontier$prob=efrontier$probmaxdecline0
keep = c("initspend","prob")
plotframe = efrontier[, keep]
plotframe$frontier = "0"

efrontier10 <- read.csv("initspend_v_probmaxdecline10.csv")
efrontier10$prob=efrontier10$probmaxdecline10
plotframe10 = efrontier10[, keep]
plotframe10$frontier = "10"

efrontier20 <- read.csv("initspend_v_probmaxdecline20.csv")
efrontier20$prob=efrontier20$probmaxdecline20
plotframe20 = efrontier20[, keep]
plotframe20$frontier = "20"

efrontier30 <- read.csv("initspend_v_probmaxdecline30.csv")
efrontier30$prob=efrontier30$probmaxdecline30
plotframe30 = efrontier30[, keep]
plotframe30$frontier = "30"

efrontier40 <- read.csv("initspend_v_probmaxdecline40.csv")
efrontier40$prob=efrontier40$probmaxdecline40
plotframe40 = efrontier40[, keep]
plotframe40$frontier = "40"

efrontier50 <- read.csv("initspend_v_probmaxdecline50.csv")
efrontier50$prob=efrontier50$probmaxdecline50
plotframe50 = efrontier50[, keep]
plotframe50$frontier = "50"

plotframe <- merge(plotframe, plotframe10, all=TRUE)
plotframe <- merge(plotframe, plotframe20, all=TRUE)
plotframe <- merge(plotframe, plotframe30, all=TRUE)
plotframe <- merge(plotframe, plotframe40, all=TRUE)
plotframe <- merge(plotframe, plotframe50, all=TRUE)
plotframe <- plotframe[plotframe$prob <=.50, ]
#plotframe <- merge(plotframe, plotframe60, all=TRUE)
#plotframe <- merge(plotframe, plotframe50, all=TRUE)

ggplot(data=plotframe, aes(x=prob, y=initspend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
    ylab("Initial spending") +
    xlab("Probability of shortfall") +
    theme_bw() +
    theme(legend.position="top",
          legend.direction = "horizontal",
          plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid"),
          legend.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid")
          ) +
    labs(title="Initial spending vs. shortfall probability, selected levels", size=16) +
    theme(legend.text=element_text(size=12), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12)) +
#    geom_point(size=3) +
    geom_line(size=0.5) +
    scale_colour_manual("", breaks = c("0","10","20","30","40","50") ,
                        values = c(dvred, dvblue, dvorange, dvgreen, dvpurple, dvblack),
                        labels = c("Prob (shortfall > 0%)","> 10%","> 20%","> 30%","> 40%","> 50%")) +
    ## scale_shape_manual("", breaks = c("frontier"),
    ##                    values = c(1),
    ##                    labels = c("Initial spending v. probability of shortfall")) +
    expand_limits(x = 0, y = 0)

