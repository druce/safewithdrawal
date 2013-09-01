
library("reshape")
library("ggplot2")

setwd("c:/Users/druce/ec2/safewithdrawal")

efrontier <- read.csv("initspend_v_probmaxdecline0.csv")

keep = c("initspend","probmaxdecline0")
plotframe = efrontier[, keep]
plotframe$frontier = "frontier"

ggplot(data=plotframe, aes(x=probmaxdecline0, y=initspend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Initial spending") +
    xlab("Probability of shortfall") +
    theme_bw() +
    theme(legend.position="top",
          legend.direction = "horizontal",
          plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid"),
          legend.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid")
          ) +
    opts(legend.text=theme_text(size=16), axis.title.x=theme_text(size=12), axis.title.y=theme_text(size=12)) +
    geom_point(size=3) +
    geom_line(size=1) +
    scale_colour_manual("", breaks = c("frontier"),
                        values = c("#e41a1c"),
                        labels = c("Initial spending v. probability of shortfall")) +
    scale_shape_manual("", breaks = c("frontier"),
                       values = c(1),
                       labels = c("Initial spending v. probability of shortfall")) +
    expand_limits(x = 0, y = 0)

