efrontier0 <- read.csv("ce4_v_probmaxdecline0.csv")
efrontier0$prob = efrontier0$probmaxdecline0
keep = c("ce4","prob")
plotframe0 = efrontier0[, keep]
plotframe0$frontier = "0"

efrontier10 <- read.csv("ce4_v_probmaxdecline10.csv")
efrontier10$prob = efrontier10$probmaxdecline10
plotframe10 = efrontier10[, keep]
plotframe10$frontier = "10"

efrontier20 <- read.csv("ce4_v_probmaxdecline20.csv")
efrontier20$prob = efrontier20$probmaxdecline20
plotframe20 = efrontier20[, keep]
plotframe20$frontier = "20"

efrontier30 <- read.csv("ce4_v_probmaxdecline30.csv")
efrontier30$prob = efrontier30$probmaxdecline30
plotframe30 = efrontier30[, keep]
plotframe30$frontier = "30"

efrontier40 <- read.csv("ce4_v_probmaxdecline40.csv")
efrontier40$prob = efrontier40$probmaxdecline40
plotframe40 = efrontier40[, keep]
plotframe40$frontier = "40"

efrontier50 <- read.csv("ce4_v_probmaxdecline50.csv")
efrontier50$prob = efrontier50$probmaxdecline50
plotframe50 = efrontier50[, keep]
plotframe50$frontier = "50"

plotframe <- plotframe0
plotframe <- merge(plotframe, plotframe10, all=TRUE)
plotframe <- merge(plotframe, plotframe20, all=TRUE)
plotframe <- merge(plotframe, plotframe30, all=TRUE)
plotframe <- merge(plotframe, plotframe40, all=TRUE)
plotframe <- merge(plotframe, plotframe50, all=TRUE)


ggplot(data=plotframe, aes(x=prob, y=ce4, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Lifetime spending") +
    xlab("Shortfall prob %") +
    theme_bw() +
    theme(legend.position="top",
          legend.direction = "horizontal",
          plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid"),
          legend.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid")
    ) +
    labs(title="Lifetime spending vs. shortfall probabilities", size=16) +
    opts(legend.text=theme_text(size=12), axis.title.x=theme_text(size=12), axis.title.y=theme_text(size=12)) +
    #    geom_point(size=3) +
    geom_line(size=0.5) +
    scale_colour_manual("", breaks = c("0","10","20","30","40","50") ,
                        values = c(dvred, dvblue, dvorange, dvgreen, dvpurple, dvblack),
                        labels = c("No shortfall","No shortfall>10%",">20%",">30%",">40%",">50%"))

ggplot(data=plotframe0, aes(x=prob, y=ce4, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Lifetime spending") +
    xlab("Shortfall prob %") +
    theme_bw() +
    theme(legend.position="top",
          legend.direction = "horizontal",
          plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid"),
          legend.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = 0.1, linetype="solid")
    ) +
    labs(title="Lifetime spending vs. shortfall probability", size=16) +
    opts(legend.text=theme_text(size=12), axis.title.x=theme_text(size=12), axis.title.y=theme_text(size=12)) +
    #    geom_point(size=3) +
    geom_line(size=1) +
    scale_colour_manual("", breaks = c("0") ,
                        values = c(dvred),
                        labels = c("Probability of no shortfall"))

