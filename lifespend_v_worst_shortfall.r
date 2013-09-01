
library("reshape")
library("ggplot2")

setwd("c:/Users/druce/ec2/safewithdrawal")

efrontier0 <- read.csv("lifespend_v_worst_shortfall.csv")
efrontier0$worst = efrontier0$worst_shortfall
keep = c("lifespend","worst")
plotframe0 = efrontier0[, keep]
plotframe0$frontier = "0"

efrontier90 <- read.csv("lifespend_v_percentile90.csv")
efrontier90$worst = efrontier90$shortfall_percentile90
plotframe90 = efrontier90[, keep]
plotframe90$frontier = "90"

efrontier80 <- read.csv("lifespend_v_percentile80.csv")
efrontier80$worst = efrontier80$shortfall_percentile80
plotframe80 = efrontier80[, keep]
plotframe80$frontier = "80"

## efrontier70 <- read.csv("lifespend_v_percentile70.csv")
## efrontier70$worst = efrontier70$shortfall_percentile70
## plotframe70 = efrontier70[, keep]
## plotframe70$frontier = "70"

## efrontier60 <- read.csv("lifespend_v_percentile60.csv")
## efrontier60$worst = efrontier60$shortfall_percentile60
## plotframe60 = efrontier60[, keep]
## plotframe60$frontier = "60"

## efrontier50 <- read.csv("lifespend_v_percentile50.csv")
## efrontier50$worst = efrontier50$shortfall_percentile50
## plotframe50 = efrontier50[, keep]
## plotframe50$frontier = "50"

plotframe <- plotframe0
plotframe <- merge(plotframe, plotframe90, all=TRUE)
plotframe <- merge(plotframe, plotframe80, all=TRUE)
## plotframe <- merge(plotframe, plotframe70, all=TRUE)
## plotframe <- merge(plotframe, plotframe60, all=TRUE)
## plotframe <- merge(plotframe, plotframe50, all=TRUE)

# delete this datapoint, not wrong but looks like an artifact on the plot
# other lines don't have an x=85 point so it ends up crossing other lines
plotframe = rbind(plotframe[1:107,],plotframe[109:121,])


ggplot(data=plotframe, aes(x=worst, y=lifespend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Lifetime spending") +
    xlab("Shortfall %") +
    theme_bw() +
    theme(legend.position="top",
          legend.direction = "horizontal",
          plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid"),
          legend.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid")
    ) +
    labs(title="Lifetime spending vs. shortfall, selected percentiles", size=16) +
    opts(legend.text=theme_text(size=12), axis.title.x=theme_text(size=12), axis.title.y=theme_text(size=12)) +
    #    geom_point(size=3) +
    geom_line(size=0.5) +
    scale_colour_manual("", breaks = c("0","90","80") ,
                        values = c(dvred, dvblue, dvgreen),
                        labels = c("Worst shortfall","90th percentile","80th"))
                                        #    scale_shape_manual("", breaks = c("0","10","20","30","40","50") ,
    #                        values = c(1,2,3,4,5,6),
    #                        labels = c("0","10","20","30","40","50")) +
    #expand_limits(x = 0, y = 0)

