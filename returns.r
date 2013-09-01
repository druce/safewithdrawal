dvblue = "#000099"
dvred = "#e41a1c"
dvgreen = "#4daf4a"
dvpurple = "#984ea3"
dvorange = "#ff7f00"
dvyellow = "#ffff33"
dvgray="#666666"
dvlightblue = "#3333FF"
dvdarkgreen = "#009900"
dvpink = "#FF9999"
dvblack = "#000000"

ageframe <- function (lifetable, age, maxage)
{
    # number of rows returned: maxage-age
    start = age+1
    end=maxage
    keep <- c("Age","Mmort","Mlives","MLE")
    retframe = lifetable[start:end,keep]
    retframe$survivepct=retframe$Mlives/retframe[1,"Mlives"]
    return(retframe)
}

############################################################

cohortframe <- function (ageframe, portframe, startyear)
{
    retframe <-ageframe
    retireyears=nrow(ageframe)
    endyear = startyear+retireyears-1
    retframe$portreturn = portframe[startyear:endyear,"portreturn"]
    retframe$year = portframe[startyear:endyear,"Year"]
    return(retframe)

}

############################################################

cohortframestep <- function (ageframex, portframe, startyear, startequity, equitystep)
{
    minLE = 15 # start stepping down equity by equity step when LE < minLE

    retframe <-ageframex
    retireyears <- nrow(ageframex)
    endyear <- startyear+retireyears-1
    retframe$equitypct <- 0
    retframe$bondpct <- 0
    retframe$portreturn <- 0
    latestequity=startequity
    equitystep = -equitystep
    ## unvectorized
    ## for (index in 1:nrow(retframe)) {
    ##     latestequity <- latestequity+equitystep
    ##     if (latestequity < 0)
    ##         latestequity <- 0
    ##     retframe[index,"equitypct"] <- latestequity
    ##     retframe[index,"bondpct"] <- 1-latestequity
    ## }

    nosteplen = nrow(ageframex[ageframex$MLE > 15,])
    if (nosteplen > 0)
        nosteplen = nosteplen-1

    ## vectorized version of above
    lenframe = nrow(retframe)
    if (equitystep != 0)
    {
        stepvector = seq(startequity,0,equitystep)
        stepvector = append(rep(startequity, nosteplen), stepvector)

        nsteps = length(stepvector)
        if (lenframe > nsteps)
            stepvector = append(stepvector, rep(0,lenframe-nsteps))
        if (lenframe < nsteps)
            stepvector = stepvector[1:lenframe]
    }
    else {
        stepvector = rep(startequity, lenframe)
    }

    retframe$equitypct = stepvector
    retframe$bondpct = 1-retframe$equitypct

    retframe$realstocks = portframe[startyear:endyear,"realstocks"]
    retframe$realbonds = portframe[startyear:endyear,"realbonds"]
    retframe$portreturn <- retframe$equitypct * retframe$realstocks + retframe$bondpct * retframe$realbonds
    retframe$year = portframe[startyear:endyear,"Year"]
    return(retframe)

}


############################################################


calcspending <- function(cohortframex, spendingfactor, mysmoothingfactor, LEbuffer)
{
    csframe <- cohortframex
    csframe$spend=0
    csframe$startportval=0
    csframe$portval_MA=0
    csframe$endportval=0
    portval=100
    last_MA=100
    smoothup = mysmoothingfactor # or * 2 to adjust up more slowly
    smoothdown = mysmoothingfactor

    for (index in 1: nrow(csframe)) {
        myrow = csframe[index,]
        myrow["startportval"]=portval

        # ability to adjust asymmetrically
        if (portval < last_MA)
            # smooth when going down
            portval_MA = last_MA + (portval - last_MA)/smoothdown
        else
            # smooth when going up
            portval_MA = last_MA + (portval - last_MA)/smoothup

        myrow["portval_MA"] = portval_MA
        last_MA = portval_MA

        spendrate = spendingfactor/(myrow["MLE"]+LEbuffer)
        spend = spendrate * portval_MA
        spend = min(spend, portval) # don't spend below 0 (possible if very smoothed and big market drop)
        myrow["spend"]=spend

        portval = portval - spend
        portval = portval * (1 + myrow["portreturn"])
        myrow["endportval"]=portval
        csframe[index,]=myrow
    }
    return(csframe)
}

# for loop is incredibly slow - rewrite the portion necessitating for loop in C++ so for loop can be vectorized

install.packages(c("Rcpp", "rbenchmark", "inline", "RUnit"))
library('Rcpp')

cppFunction('
  NumericVector doCalcSpendC (NumericVector returnSeries, NumericVector spendingRates, double smoothfactor) {
    int n = returnSeries.size();
    NumericVector out(n);

    double portval=100;
    double portvalMA=100;
    double spendAmt = 0;

    for(int i = 0; i < n; ++i) {
      portvalMA = portvalMA + (portval - portvalMA)/smoothfactor;
      spendAmt = spendingRates[i] * portvalMA;
      if (spendAmt > portval) {
        spendAmt = portval;
      }
      portval = (portval - spendAmt) * (1 + returnSeries[i]);
      out[i] = spendAmt;
    }

    return out;
  }
')

cppFunction('
  NumericVector doCalcSpendC2 (NumericVector returnSeries, NumericVector spendingRates, double spendingconst, double smoothfactor) {
    int n = returnSeries.size();
    NumericVector out(n);

    double portval=100;
    double portvalMA=100;
    double spendAmt = 0;

    for(int i = 0; i < n; ++i) {
      portvalMA = portvalMA + (portval - portvalMA)/smoothfactor;
      spendAmt = spendingRates[i] * portvalMA + spendingconst;
      if (spendAmt > portval) {
        spendAmt = portval;
      }
      portval = (portval - spendAmt) * (1 + returnSeries[i]);
      out[i] = spendAmt;
    }

    return out;
  }
')

calcspending2 <- function(cohortframex, spendingfactor, mysmoothingfactor, LEbuffer)
{
    # vectorized using function written in C++

    csframe <- cohortframex
    csframe$spendrate = spendingfactor/(csframe$MLE+LEbuffer)

    csframe$spend = doCalcSpendC(csframe$portreturn, csframe$spendrate, mysmoothingfactor)

    return(csframe)
}


############################################################

calcspending3 <- function(cohortframex, spendingfactor, spendingconst, mysmoothingfactor, LEbuffer)
{
    # vectorized using function written in C++

    csframe <- cohortframex
    csframe$spendrate = spendingfactor/(csframe$MLE+LEbuffer)

    csframe$spend = doCalcSpendC2(csframe$portreturn, csframe$spendrate, spendingconst, mysmoothingfactor)

    return(csframe)
}


############################################################

calcspendingfixed <- function(cohortframex, spendingfactor)
{
    retframe <- cohortframex
    retframe$spend=0
    retframe$startportval=0
    retframe$endportval=0
    portval=100
    for (index in 1: nrow(retframe)) {
        myrow = retframe[index,]
        myrow["startportval"]=portval
        if ( portval > 4) {
            spend = 4
        } else {
            spend <- portval
        }
        myrow["spend"] <- spend

        portval = portval - spend
        portval = portval * (1 + myrow["portreturn"])
        myrow["endportval"]=portval
        retframe[index,]=myrow
    }
    return(retframe)
}

############################################################

testspendingfactor <- function(realreturns, spendingfactor, minage, maxage, startequity, step, mysmoothingfactor, LEbuffer)
{
    print (sprintf("Testing sfactor %.2f, %d to %d, startequity %.2f, step %.2f, smooth %.1f, LEbuffer %.1f ",spendingfactor, minage, maxage, startequity, step, mysmoothingfactor, LEbuffer))

    tempageframe <- ageframe(lifetable, minage, maxage)
    ntrials <- nrow(realreturns)-nrow(tempageframe) +1
    nyears <- nrow(tempageframe)
    trials <- data.frame(1:nyears)
    colnames(trials) = "age"
    for (index in 1:ntrials) {
        tempframe1 = cohortframestep(tempageframe,realreturns,index,startequity,step)
        tempframe2 = calcspending2(tempframe1,spendingfactor,mysmoothingfactor, LEbuffer)
        colname = paste("trial",index, sep="")
        trials[colname] <- tempframe2$spend
    }

    trials <- trials[2:ncol(trials)]
    return(trials)

}

############################################################

testspendingfactor2 <- function(realreturns, spendingfactor, spendingconst, minage, maxage, startequity, step, mysmoothingfactor, LEbuffer)
{
    print (sprintf("Testing sfactor %.2f, sconst %.2f, %d to %d, startequity %.2f, step %.2f, smooth %.1f, LEbuffer %.1f ",spendingfactor, spendingconst, minage, maxage, startequity, step, mysmoothingfactor, LEbuffer))

    tempageframe <- ageframe(lifetable, minage, maxage)
    ntrials <- nrow(realreturns)-nrow(tempageframe) +1
    nyears <- nrow(tempageframe)
    trials <- data.frame(1:nyears)
    colnames(trials) = "age"
    for (index in 1:ntrials) {
        tempframe1 = cohortframestep(tempageframe,realreturns,index,startequity,step)
        tempframe2 = calcspending3(tempframe1,spendingfactor,spendingconst, mysmoothingfactor, LEbuffer)
        colname = paste("trial",index, sep="")
        trials[colname] <- tempframe2$spend
    }

    trials <- trials[2:ncol(trials)]
    return(trials)

}

############################################################

testspendingfactorfixed <- function(realreturns, spendingfactor, minage, maxage)
{
    tempageframe <- ageframe(lifetable, minage, maxage)
    ntrials <- nrow(realreturns)-nrow(tempageframe) +1
    nyears <- nrow(tempageframe)
    trials <- data.frame(1:nyears)
    colnames(trials) = "age"
    for (index in 1:ntrials) {
        tempframe1 = cohortframe(tempageframe,realreturns,index)
        tempframe2 = calcspendingfixed(tempframe1,spendingfactor)
        colname = paste("trial",index, sep="")
        trials[colname] <- tempframe2$spend
    }

    trials <- trials[2:ncol(trials)]
    return(trials)
}

############################################################

calctrialssummary <- function(trials)
{
    trialssummary <- data.frame(1:nrow(trials))
    colnames(trialssummary) = "year"
    trialssummary$mean=apply(trials,1,mean)

    trialssummary$max=apply(trials,1,max)
    trialssummary$min=apply(trials,1,min)
    trialssummary$sd=apply(trials,1,sd)
    trialssummary$plus1sd=trialssummary$mean + trialssummary$sd
    trialssummary$minus1sd=trialssummary$mean - trialssummary$sd
    keep <- c("year", "mean","min","max","minus1sd","plus1sd")
    trialssummary <- trialssummary[,keep]

    return(trialssummary)
}


############################################################

calcexpectedspending<- function(trials, survival)
{
    trialssummary <- data.frame(1:nrow(trials))
    colnames(trialssummary) = "year"

    trialssummary$survivepct=survival$survivepct
    trialssummary$mean=apply(trials,1,mean)
    trialssummary$expmean= trialssummary$mean * trialssummary$survivepct
    trialssummary$max=apply(trials,1,max)
    trialssummary$expmax= trialssummary$max * trialssummary$survivepct
    trialssummary$min=apply(trials,1,min)
    trialssummary$expmin= trialssummary$min * trialssummary$survivepct
    trialssummary$sd=apply(trials,1,sd)
    trialssummary$plus1sd=trialssummary$mean + trialssummary$sd
    trialssummary$expplus1sd= trialssummary$plus1sd * trialssummary$survivepct
    trialssummary$minus1sd=trialssummary$mean - trialssummary$sd
    trialssummary$expminus1sd= trialssummary$minus1sd * trialssummary$survivepct

    keep <- c("year", "expmean","expmin","expmax", "expplus1sd", "expminus1sd")
    trialssummary <- trialssummary[,keep]
    return(trialssummary)
}

############################################################

chartsummary <- function(trialssummary, mytitle)
{
    meltframe <- melt(trialssummary, id = 'year')
    ggplot(data=meltframe, aes(x=year, y=value, colour=variable)) +
     scale_x_continuous() +
     ylab("Annual spending (% of initial portfolio)") +
     xlab("Retirement year") +
     theme_bw() +
     geom_line(size=0.6) +
     theme(legend.position="top",
          legend.direction = 'horizontal') +
#          plot.background = element_rect(colour = 'black', fill = '#FFFFFF', size = 1, linetype='solid')) +
     labs(title=mytitle)+
     scale_colour_manual('', breaks = c('mean', 'min','max','minus1sd','plus1sd'),
                         values = c("#000099", "#CC0000","#009900","#999999","#999999"),
                        labels = c('Mean', 'Worst case', 'Best case', '-1 SD', '+1SD'))
}

############################################################

testsfactors <- function(realreturns, ageframex, nsfactors, startequity, step, mysmoothingfactor, LEbuffer)
{
    sfactorsummary <- data.frame(1:nsfactors)
    colnames(sfactorsummary) = "n"
    sfactorsummary$sfactor <- 0
    sfactorsummary$initspend <- 0
    sfactorsummary$expspend <- 0
    sfactorsummary$minspend <- 0
    sfactorsummary$maxspend <- 0
    sfactorsummary$plus1sd <- 0
    sfactorsummary$minus1sd <- 0
    sfactorsummary$sdspend <- 0
    sfactorsummary$shortfall25 <- 0
    sfactorsummary$shortfall10 <- 0
    sfactorsummary$shortfall50 <- 0
    sfactorsummary$worstshortfall <- 0

    for (index in 1:nsfactors)
    {
        spendingfactor = index/20
        trials = testspendingfactor(realreturns, spendingfactor, 65, 100, startequity, step, mysmoothingfactor, LEbuffer)
        trialssummary = calcexpectedspending(trials, ageframex)

        sfactorsummary$initspend[index] = trialssummary[1,'expmean']
        sfactorsummary$sfactor[index] = spendingfactor
        sfactorsummary$expspend[index] = sum(trialssummary[,"expmean"])
        sfactorsummary$minspend[index] = sum(trialssummary[,"expmin"])
        sfactorsummary$maxspend[index] = sum(trialssummary[,"expmax"])
        sfactorsummary$plus1sd[index] = sum(trialssummary[,"expplus1sd"])
        sfactorsummary$minus1sd[index] = sum(trialssummary[,"expminus1sd"])
        sfactorsummary$sdspend[index]=rowMeans(apply(trials, 2, sd)/trials[1,])

        trialstmp=trials
        worstshortfall=0
        for (index2 in 1:ncol(trialstmp))
        {
            trialstmp[index2]=trialstmp[index2]/trialstmp[1,index2]
            shortfall = (1 - min(trialstmp[index2]))
            if (shortfall > worstshortfall)
                worstshortfall = shortfall
        }
        sfactorsummary$worstshortfall[index]=worstshortfall

        trialstmp[trialstmp<0.75]=-1
        trialstmp[trialstmp>0]=0
        trialstmp[trialstmp<0]=1

        for (index2 in 1:ncol(trialstmp))
            trialstmp[index2]=trialstmp[index2] * ageframex$survivepct

        sfactorsummary$shortfall25[index]=mean(apply(trialstmp,2,max))

        trialstmp=trials
        for (index2 in 1:ncol(trialstmp))
            trialstmp[index2]=trialstmp[index2]/trialstmp[1,index2]

        trialstmp[trialstmp<0.90]=-1
        trialstmp[trialstmp>0]=0
        trialstmp[trialstmp<0]=1

        for (index2 in 1:ncol(trialstmp))
            trialstmp[index2]=trialstmp[index2] * ageframex$survivepct

        sfactorsummary$shortfall10[index]=mean(apply(trialstmp,2,max))

        trialstmp=trials
        for (index2 in 1:ncol(trialstmp))
            trialstmp[index2]=trialstmp[index2]/trialstmp[1,index2]
        trialstmp[trialstmp<0.50]=-1
        trialstmp[trialstmp>0]=0
        trialstmp[trialstmp<0]=1

        for (index2 in 1:ncol(trialstmp))
            trialstmp[index2]=trialstmp[index2] * ageframex$survivepct

        sfactorsummary$shortfall50[index]=mean(apply(trialstmp,2,max))

    }
    return(sfactorsummary)
}


testsfactors2 <- function(realreturns, ageframex, nsfactors, startequity, step, mysmoothingfactor, LEbuffer)
{
    # simplified - just worstshortfall, initspend, $expend
    sfactorsummary <- data.frame(1:nsfactors)
    colnames(sfactorsummary) = "n"
    sfactorsummary$sfactor <- 0
    sfactorsummary$initspend <- 0
    sfactorsummary$expspend <- 0
    #sfactorsummary$minspend <- 0
    #sfactorsummary$maxspend <- 0
    #sfactorsummary$plus1sd <- 0
    #sfactorsummary$minus1sd <- 0
    #sfactorsummary$sdspend <- 0
    sfactorsummary$worstshortfall <- 0

    for (index in 1:nsfactors)
    {
        spendingfactor = index/20
        trials = testspendingfactor(realreturns, spendingfactor, 65, 100, startequity, step, mysmoothingfactor, LEbuffer)
        trialssummary = calcexpectedspending(trials, ageframex)

        sfactorsummary$initspend[index] = trialssummary[1,'expmean']
        sfactorsummary$sfactor[index] = spendingfactor
        sfactorsummary$expspend[index] = sum(trialssummary[,"expmean"])
        #sfactorsummary$minspend[index] = sum(trialssummary[,"expmin"])
        #sfactorsummary$maxspend[index] = sum(trialssummary[,"expmax"])
        #sfactorsummary$plus1sd[index] = sum(trialssummary[,"expplus1sd"])
        #sfactorsummary$minus1sd[index] = sum(trialssummary[,"expminus1sd"])
        #sfactorsummary$sdspend[index]=rowMeans(apply(trials, 2, sd)/trials[1,])

        trialstmp=trials
        worstshortfall=0
        for (index2 in 1:ncol(trialstmp))
        {
            trialstmp[index2]=trialstmp[index2]/trialstmp[1,index2]
            shortfall = (1 - min(trialstmp[index2]))
            if (shortfall > worstshortfall)
                worstshortfall = shortfall
        }
        sfactorsummary$worstshortfall[index]=worstshortfall
    }
    return(sfactorsummary)
}

system.time(testsfactors(realreturns, ageframex, nsfactors,0.6,0,1,0))
system.time(testsfactors2(realreturns, ageframex, nsfactors,0.6,0,1,0))

############################################################

testsmooth <- function(realreturns, ageframex, nsfactors, step, mysmoothingfactor, LEbuffer)
{
    retframe = data.frame (sfactor = double(), startequitypct=double(), variable=character(), value=double())

    for (index1 in 0:20) {
        startequitypct = index1 * 0.05
        sfactors = testsfactors(realreturns, ageframex, nsfactors, startequitypct, step, mysmoothingfactor, LEbuffer)
        sfactors$startequitypct = startequitypct
        sfactortemp <- sfactors
        meltframe <- melt(sfactortemp, id = c('sfactor', 'startequitypct'))
        retframe <- merge(meltframe, retframe, all=TRUE)
    }
    return(retframe)
}

############################################################

genfrontier <- function(myframe, returnColName, riskColName)
{
    f1 = subset(myframe, myframe["variable"]==returnColName)
    f2 = subset(myframe, myframe["variable"]==riskColName)
    tmpframe = merge(f1, f2, by=c('sfactor','startequitypct'))
    keep <- c("value.x","value.y")
    tmpframe <- tmpframe[keep]
    colnames(tmpframe) = c(returnColName, riskColName)

    frontierframe = data.frame (spend=double(), risk = double())
    colnames(frontierframe) = c("spend","risk")

    while (nrow(tmpframe) > 0)
    {
        minRisk = min(tmpframe[,riskColName])
        tmp2 = subset(tmpframe, tmpframe[riskColName]==minRisk)
        maxRet = max(tmp2[, returnColName])
        newrow=c(maxRet,minRisk)
        frontierframe=rbind(frontierframe, newrow)

        tmpframe = subset(tmpframe, tmpframe[riskColName] > minRisk)
        if (nrow(tmpframe) > 0)
            tmpframe = subset(tmpframe, tmpframe[returnColName] > maxRet)
    }

    colnames(frontierframe) = c("spend","risk")
    return(frontierframe)
}

genfrontier2 <- function(myframe, returnColName, riskColName, reportColName)
    # for each level of riskcolname, find max returnColName,
    # return frame with riskcol, reportcol (eg find best shortfall v expspend, return risk v initspend
{
    f1 = subset(myframe, myframe["variable"]==returnColName)
    f2 = subset(myframe, myframe["variable"]==riskColName)
    f3 = subset(myframe, myframe["variable"]==reportColName)
    tmpframe1 = merge(f1, f2, by=c('sfactor','startequitypct'))
    keep <- c("sfactor","startequitypct","value.x","value.y")
    tmpframe1 <- tmpframe1[keep]
    colnames(tmpframe1) = c("sfactor","startequitypct",returnColName, riskColName)

    tmpframe2 <- merge(tmpframe1, f3, by=c('sfactor','startequitypct'))
    keep <-  c("sfactor","startequitypct",returnColName, riskColName, "value")
    tmpframe2 <- tmpframe2[keep]
    colnames(tmpframe2) = c("sfactor","startequitypct",returnColName, riskColName, reportColName)

    frontierframe = data.frame (spend=double(), risk = double())
    colnames(frontierframe) = c("spend","risk")

    while (nrow(tmpframe2) > 0)
    {
        minRisk = min(tmpframe2[,riskColName])
        tmp2 = subset(tmpframe2, tmpframe2[riskColName]==minRisk)
        maxRet = max(tmp2[, returnColName])
        tmp3 = subset(tmp2, tmp2[returnColName]==maxRet)
        maxReport = max(tmp3[, reportColName])
        newrow=c(maxReport,minRisk)
        frontierframe=rbind(frontierframe, newrow)

        tmpframe2 = subset(tmpframe2, tmpframe2[riskColName] > minRisk)
        if (nrow(tmpframe2) > 0)
            tmpframe2 = subset(tmpframe2, tmpframe2[reportColName] > maxReport)
    }

    colnames(frontierframe) = c("spend","risk")
    return(frontierframe)
}


genfrontier3 <- function(myframe, returnColName, riskColName, reportColName)
    # for each level of riskcolname, find max returnColName,
    # return frame with riskcol, reportcol (eg find best shortfall v expspend, return risk v initspend
    # this version will return equity pct, smoothing factor, s (for table instead of chart)

{
    tmpframe = data.frame(cast(myframe,sfactor+startequitypct+smooth~variable,mean))
    # all possibilities for returnCol, riskCol should be represented:
    keep <-  c("sfactor","startequitypct","smooth","initspend","expspend","worstshortfall")
    tmpframe <- tmpframe[keep]

    frontierframe = tmpframe
    frontierframe<- frontierframe[which(is.na(frontierframe$text)), ] # delete all rows - expr always na - generates warning

    while (nrow(tmpframe) > 0)
    {
        minRisk = min(tmpframe[,riskColName])
        tmp2 = subset(tmpframe, tmpframe[riskColName]==minRisk)
        maxRet = max(tmp2[, returnColName])
        tmp3 = subset(tmp2, tmp2[returnColName]==maxRet)
        maxReport = max(tmp3[, reportColName])
        tmp4 = subset(tmp3, tmp3[reportColName]==maxReport)
        newrow=tmp4[1,]
        frontierframe=rbind(frontierframe, newrow)

        tmpframe = subset(tmpframe, tmpframe[riskColName] > minRisk)
        if (nrow(tmpframe) > 0)
            tmpframe = subset(tmpframe, tmpframe[reportColName] > maxReport)
    }

    keep <-  c("sfactor","startequitypct","smooth","initspend","expspend","worstshortfall")
    frontierframe <- frontierframe[keep]
    return(frontierframe)
}



############################################################

chartfrontier <- function(myframe, frontier1name, frontier1desc, frontier2name, frontier2desc)
{
    ggplot(data=myframe, aes(x=risk, y=spend, colour=frontier, shape=frontier)) +
     scale_x_continuous() +
     ylab("Spend") +
     xlab("Worst shortfall") +
     theme_bw() +
     geom_point(size=3) +
     geom_line(size=1) +
     opts(legend.position="top",
          legend.direction = 'horizontal',
          plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Spend v. Worst Shortfall", breaks = c(frontier1name, frontier2name),
                         values = c("#000099", "#990000"),
                         labels = c(frontier1desc, frontier2desc)) +
     scale_shape_manual("Spend v. Worst Shortfall", breaks = c(frontier1name, frontier2name),
                        values = c(1, 4),
                        labels = c(frontier1desc, frontier2desc))
}



library(reshape)
library(ggplot2)
library(lattice)

returns <- read.csv("~/Documents/returns.csv")  # http://blog.streeteye.com/blog/wp-content/uploads/2013/01/returns.csv
lifetable <- read.csv("~/Documents/Lifetable.csv") # http://blog.streeteye.com/blog/wp-content/uploads/2013/01/Lifetable.csv

returns$realstocks=returns$Stocks-returns$CPI
returns$realbonds=returns$Bonds-returns$CPI
returns$realbills=returns$Bills-returns$CPI

drops <- c("Stocks","Bonds","Bills","CPI")
realreturns <- returns[,!(names(returns) %in% drops)]
realreturns$portreturn = 0.6*realreturns$realstocks + 0.4 *realreturns$realbonds

#Figure 1

tempageframe <- ageframe(lifetable, 65, 100)
tempframe1 = cohortframestep(tempageframe,realreturns,1, 0.6, 0)
tempframe2 = calcspending2(tempframe1,1,1,0)
tempframe2$survival65 = tempframe2$survivepct * 10000
tempframe2$spend = tempframe2$spend * 1000
keep = c("year","spend","survival65" )
tempframe2 <- tempframe2[,keep]
meltframe <- melt(tempframe2, id = 'year')

ggplot(data=meltframe, aes(x=year, y=value, colour=variable)) +
     scale_x_continuous() +
     ylab("Spend") +
     xlab("Year") +
     theme_bw() +
     geom_line(size=1.4) +
     opts(legend.position="top",
          legend.direction = 'horizontal',
          plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Annual spending, retirement age 65, 60/40 portfolio, spending factor 1, 1928 cohort", breaks = c('spend', 'survival65'),
                         values = c("#CC0000", "#000099"),
                        labels = c('Annual Spend', 'Survivors from starting 10,000'))


#Figure 2
trials = testspendingfactor(realreturns, 1, 65, 100, 0.6, 0, 1, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts")

#Figure 2b
trials = testspendingfactor2(realreturns, 0.4, 1.5, 65, 95, 0.7, 0, 3.5, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,95)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Spending profile, all cohorts\n(var=0.4,const=1.5,eq=70%,smoothing=3.5,mort_ins=0")


microbenchmark(
               trials = testspendingfactor(realreturns, 1, 65, 100, 0.6, 0, 1, 0)
)

# Figure 3
facetframe = data.frame (year = integer(), variable=character(), value=double())

for (index in 1:24) {
    spendingfactor = index/20
    trials = testspendingfactor(realreturns, spendingfactor, 65, 100, 0.6, 0, 1, 0)
    trialssummary = calctrialssummary(trials)
    trialssummary$spendingfactor = spendingfactor
    meltframe <- melt(trialssummary, id = c('year','spendingfactor'))
    facetframe <- merge(meltframe, facetframe, all=TRUE)
}

ggplot(data=facetframe, aes(x=year, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Annual spending (% of initial portfolio)") +
    xlab("Retirement year") +
    theme_bw() +
    facet_wrap(~ spendingfactor, ncol = 4) +
    geom_line(size=1) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Average Spending, All Cohorts, Spending Factors 0.05 to 1.2", breaks = c('mean', 'min','max','minus1sd','plus1sd'),
                    values = c("#000099", "#CC0000","#009900","#999999","#999999"),
                    labels = c('Average Spend', 'Minimum', 'Maximum', '-1 SD', '+1SD'))

############################################################
# Figure 4
ageframex = ageframe(lifetable,65,100)
nsfactors = 40
sfsummary = testsfactors(realreturns, ageframex,nsfactors, 0.6, 0, 1, 0)

chart1=sfsummary
keep <- c("sfactor","expspend","minspend","maxspend","plus1sd","minus1sd")
chart1 <- chart1[keep]

meltframe <- melt(chart1, id = 'sfactor')

ggplot(data=meltframe, aes(x=sfactor, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Lifetime spend expectancy")+
    xlab("Spending factor") +
    theme_bw() +
    geom_line(size=1.4) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Lifetime Spend Expectancy v. Spending Factor", breaks = c('expspend', 'minspend','maxspend','plus1sd','minus1sd'),
                    values = c("#000099", "#CC0000", "#009900","#999999","#999999"),
                    labels = c('Mean', 'Worst investment case', 'Best investment case','+1SD','-1SD'))

# Figure 5

chart2=sfsummary
keep <- c("sfactor","shortfall10","shortfall25","shortfall50")
chart2 <- chart2[keep]
meltframe <- melt(chart2, id = 'sfactor')

ggplot(data=meltframe, aes(x=sfactor, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Shortfall probability") +
    xlab("Spending factor") +
    theme_bw() +
    geom_line(size=1.4) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Probability of Spending Shortfall v. Spending Factor", breaks = c('shortfall25','shortfall10','shortfall50'),
                    values = c("#009900", "#000099", "#CC0000"),
                    labels = c('25% shortfall probability','10% shortfall probability','50% Shortfall probability'))


#Figure 6
trials = testspendingfactor(realreturns, 0.5, 65, 100, 0.6, 0, 1, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts")


trials = testspendingfactorfixed(realreturns, 1, 65, 100)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max")
trialssummary <- trialssummary[,keep]

meltframe <- melt(trialssummary, id = 'year')
ggplot(data=meltframe, aes(x=year, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Annual spending (% of initial portfolio)") +
    xlab("Retirement year") +
    theme_bw() +
    geom_line(size=1.4) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual('Average Spending, All Cohorts (fixed 4% rule)', breaks = c('mean', 'min','max'),
                         values = c("#000099", "#CC0000","#009900","#999999","#999999"),
                        labels = c('Average Spend', 'Worst case', 'Best case'))


############################################################

#test in 1/2 step increments from 1.5 to 6 - create 10 frames
#create 10 efficient frontier frames
#combine 10 frontiers into 1 chart

# figure 8
ageframex = ageframe(lifetable,65,100)
nsfactors = 40
sfsummary = testsfactors(realreturns, ageframex,nsfactors, 0.6, 0, 3, 0)

chart8=sfsummary
keep <- c("sfactor","expspend","minspend","maxspend","plus1sd","minus1sd")
chart8 <- chart8[keep]

meltframe <- melt(chart8, id = 'sfactor')

ggplot(data=meltframe, aes(x=sfactor, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Lifetime spend expectancy")+
    xlab("Spending factor") +
    theme_bw() +
    geom_line(size=1.4) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Lifetime Spend Expectancy v. Spending Factor", breaks = c('expspend', 'minspend','maxspend','plus1sd','minus1sd'),
                    values = c("#000099", "#CC0000", "#009900","#999999","#999999"),
                    labels = c('Mean', 'Worst investment case', 'Best investment case','+1SD','-1SD'))

# figure 9

chart9=sfsummary
keep <- c("sfactor","shortfall10","shortfall25","shortfall50")
chart9 <- chart9[keep]
meltframe <- melt(chart9, id = 'sfactor')

ggplot(data=meltframe, aes(x=sfactor, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Shortfall probability") +
    xlab("Spending factor") +
    theme_bw() +
    geom_line(size=1.4) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Probability of Spending Shortfall v. Spending Factor", breaks = c('shortfall25','shortfall10','shortfall50'),
                    values = c("#009900", "#000099", "#CC0000"),
                    labels = c('25% shortfall probability','10% shortfall probability','50% Shortfall probability'))

# figure 10

trials = testspendingfactor(realreturns, 0.5, 65, 100, 0.6, 0, 3, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.5, smoothed)")


# alternate

# this will take a very long time (24h)
nsfactors = 24 # up to 1.2

6 step .00 to .05 set 0.01
11 smooth 1 to 6 step 0.5
6 buffer 0 to 5 step 1
21 equity 0 to 1 step .05
24 sfactor 0.1 to 1.2

testparamframe=data.frame (testname=character(), nsfactors = integer(), step=double(), mysmoothingfactor=double(), LEfactor=double(), stringsAsFactors=FALSE)

for (mystep in seq(0,1, 1))
    for (mysmooth in seq(10,60,5))
        for (mybuffer in seq(0,5,1))
{
    myname = sprintf("test_st%d_sm%d_b%d",mystep, mysmooth, mybuffer)
    testparamframe[nrow(testparamframe)+1,] = c(0,24, 0.01*mystep, 0.1*mysmooth, mybuffer)
    testparamframe[nrow(testparamframe),1] = myname
}


bigframe10 = testsmooth(realreturns, ageframex, nsfactors, 0, 1.0, 0)
bigframe15 = testsmooth(realreturns, ageframex, nsfactors, 0, 1.5, 0)
bigframe20 = testsmooth(realreturns, ageframex, nsfactors, 0, 2, 0)
bigframe25 = testsmooth(realreturns, ageframex, nsfactors, 0, 2.5, 0)
bigframe30 = testsmooth(realreturns, ageframex, nsfactors, 0, 3, 0)
bigframe35 = testsmooth(realreturns, ageframex, nsfactors, 0, 3.5, 0)
bigframe40 = testsmooth(realreturns, ageframex, nsfactors, 0, 4, 0)
bigframe45 = testsmooth(realreturns, ageframex, nsfactors, 0, 4.5, 0)
bigframe50 = testsmooth(realreturns, ageframex, nsfactors, 0, 5.0, 0)
bigframe55 = testsmooth(realreturns, ageframex, nsfactors, 0, 5.5, 0)
bigframe60 = testsmooth(realreturns, ageframex, nsfactors, 0, 6, 0)

bufframe10b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 1.0, 2)
bufframe15b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 1.5, 2)
bufframe20b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 2, 2)
bufframe25b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 2.5, 2)
bufframe30b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 3, 2)
bufframe35b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 3.5, 2)
bufframe40b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 4, 2)
bufframe45b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 4.5, 2)
bufframe50b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 5.0, 2)
bufframe55b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 5.5, 2)
bufframe60b2 = testsmooth(realreturns, ageframex, nsfactors, 0, 6, 2)

stepframe10b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 1.0, 2)
stepframe15b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 1.5, 2)
stepframe20b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 2, 2)
stepframe25b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 2.5, 2)
stepframe30b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 3, 2)
stepframe35b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 3.5, 2)
stepframe40b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 4, 2)
stepframe45b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 4.5, 2)
stepframe50b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 5.0, 2)
stepframe55b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 5.5, 2)
stepframe60b2s1 = testsmooth(realreturns, ageframex, nsfactors, 0.01, 6, 2)

bufframe30b1= testsmooth(realreturns, ageframex, nsfactors, 0, 3, 1)
bufframe30b2= testsmooth(realreturns, ageframex, nsfactors, 0, 3, 2)
bufframe30b3= testsmooth(realreturns, ageframex, nsfactors, 0, 3, 3)
bufframe30b4= testsmooth(realreturns, ageframex, nsfactors, 0, 3, 4)
bufframe30b5= testsmooth(realreturns, ageframex, nsfactors, 0, 3, 5)

bufframe10b1= testsmooth(realreturns, ageframex, nsfactors, 0, 1.0, 1)
bufframe15b1= testsmooth(realreturns, ageframex, nsfactors, 0, 1.5, 1)
bufframe20b1= testsmooth(realreturns, ageframex, nsfactors, 0, 2, 1)
bufframe25b1= testsmooth(realreturns, ageframex, nsfactors, 0, 2.5, 1)
bufframe30b1= testsmooth(realreturns, ageframex, nsfactors, 0, 3, 1)
bufframe35b1= testsmooth(realreturns, ageframex, nsfactors, 0, 3.5, 1)
bufframe40b1= testsmooth(realreturns, ageframex, nsfactors, 0, 4, 1)
bufframe45b1= testsmooth(realreturns, ageframex, nsfactors, 0, 4.5, 1)
bufframe50b1= testsmooth(realreturns, ageframex, nsfactors, 0, 5.0, 1)
bufframe55b1= testsmooth(realreturns, ageframex, nsfactors, 0, 5.5, 1)
bufframe60b1= testsmooth(realreturns, ageframex, nsfactors, 0, 6, 1)

############################################################
# do above, but leverage multiple cores

install.packages("foreach")
install.packages('doParallel')
library(doParallel)

# set up a frame with the parameters for each test
testparamframe=data.frame (testname=character(), nsfactors = integer(), step=double(), mysmoothingfactor=double(), LEfactor=double(), stringsAsFactors=FALSE)
testparamframe[1,] = c(0,24, 0.01, 3, 2)
testparamframe[nrow(testparamframe),1] = 'stepframe30b2s1'
testparamframe[nrow(testparamframe) +1,] = c(0,24, 0.01, 3.5, 2)
testparamframe[nrow(testparamframe),1] = 'stepframe35b2s1'
testparamframe[nrow(testparamframe) +1,] = c(0,24, 0.01, 4, 2)
testparamframe[nrow(testparamframe),1] = 'stepframe40b2s1'
testparamframe[nrow(testparamframe) +1,] = c(0,24, 0.01, 4.5, 2)
testparamframe[nrow(testparamframe),1] = 'stepframe45b2s1'
testparamframe[nrow(testparamframe) +1,] = c(0,24, 0.01, 5.0, 2)
testparamframe[nrow(testparamframe),1] = 'stepframe50b2s1'
testparamframe[nrow(testparamframe) +1,] = c(0,24, 0.01, 5.5, 2)
testparamframe[nrow(testparamframe),1] = 'stepframe55b2s1'
testparamframe[nrow(testparamframe) +1,] = c(0,24, 0.01, 6, 2)
testparamframe[nrow(testparamframe),1] = 'stepframe60b2s1'

# run them in parallel on 4 cores
registerDoParallel(cores=4)

# run each case in parallel foreach loop

foreach(i=1:nrow(testparamframe), .packages=c('plyr','reshape'), .combine=mycombine) %dopar% paralleltest(i)



paralleltest <- function (i)
{
    # run the test in index i
    # testparamframe, realreturns are global variables

    ageframex = ageframe(lifetable,65,100)

    myrow = testparamframe[i,]
    testname=myrow[1,1]
    nsfactors = myrow[1,2]
    step=myrow[1,3]
    mysmoothingfactor=myrow[1,4]
    LEfactor=myrow[1,5]

    retframe = testsmooth(realreturns, ageframex, nsfactors, step, mysmoothingfactor, LEfactor)
    attr(retframe, "newname") = testname
    return(retframe)
}

mycombine <- function(prev, frame) {
    # parallel running functions can't create global variables
    # when function returns, lookup newname attribute, create global variable with that name
    testname <- attr(frame, "newname")
    assign(testname, frame, envir = .GlobalEnv)
    return(frame)
}


jumbomerge <- function(framelist, namelist)
{
    tmpframe = framelist[1]
    tmpframe["label"] = namelist[1]
    retframe = tmpframe

    for (i in length(framelist)-1)
    {
        tmpframe = framelist[i+1]
        tmpframe["label"] = namelist[i+1]
        retframe = merge(retframe, tmpframe, all=TRUE)
    }
    return(retframe)
}

jumbob1frame <- jumbomerge(list(bufframe10b1,bufframe15b1,bufframe20b1,bufframe25b1,bufframe30b1,bufframe35b1,bufframe40b1,bufframe45b1,bufframe50b1,bufframe55b1,bufframe60b1),
                            list('bufframe10b1','bufframe15b1','bufframe20b1','bufframe25b1','bufframe30b1','bufframe35b1','bufframe40b1','bufframe45b1','bufframe50b1','bufframe55b1','bufframe60b1'))

jumbobufframe <- jumbomerge(list(bufframe10b2,bufframe15b2,bufframe20b2,bufframe25b2,bufframe30b2,bufframe35b2,bufframe40b2,bufframe45b2,bufframe50b2,bufframe55b2,bufframe60b2),
                            list('bufframe10b2','bufframe15b2','bufframe20b2','bufframe25b2','bufframe30b2','bufframe35b2','bufframe40b2','bufframe45b2','bufframe50b2','bufframe55b2','bufframe60b2'))

jumbostepframe <- jumbomerge(list(stepframe10b2s1,stepframe15b2s1,stepframe20b2s1,stepframe25b2s1,stepframe30b2s1,stepframe35b2s1,stepframe40b2s1,stepframe45b2s1,stepframe50b2s1,stepframe55b2s1,stepframe60b2s1),
                             list('stepframe10b2s1','stepframe15b2s1','stepframe20b2s1','stepframe25b2s1','stepframe30b2s1','stepframe35b2s1','stepframe40b2s1','stepframe45b2s1','stepframe50b2s1','stepframe55b2s1','stepframe60b2s1'))


nostepframe = genfrontier2(jumbobufframe, 'initspend', 'worstshortfall', 'initspend')
nostepframe$risk = nostepframe$risk*100
nostepframe$frontier = 'nostep'

stepframe = genfrontier2(jumbostepframe, 'initspend', 'worstshortfall', 'initspend')
stepframe$risk = stepframe$risk*100
stepframe$frontier = 'step'

myframe = merge(stepframe, nostepframe, all=TRUE)

ggplot(data=myframe, aes(x=risk, y=spend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Initial spending rate") +
    xlab("Maximum shortfall") +
    theme_bw() +
    geom_point(size=3) +
    geom_line(size=1) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Initial Spending v. Worst Shortfall, stepdown v. no stepdown", breaks = c('step','nostep'),
                         values = c(dvblue,dvred),
                         labels = c("1% equity stepdown", "no stepdown")) +
     scale_shape_manual("Initial Spending v. Worst Shortfall, stepdown v. no stepdown", breaks = c('step','nostep'),
                        values = c(1,4),
                        labels = c("1% equity stepdown", "no stepdown"))


nostepframe = genfrontier2(jumbobufframe, 'initspend', 'worstshortfall', 'initspend')
nostepframe$risk = nostepframe$risk*100
nostepframe$frontier = 'nostep'

stepframe = genfrontier2(jumbob1frame, 'initspend', 'worstshortfall', 'initspend')
stepframe$risk = stepframe$risk*100
stepframe$frontier = 'step'

myframe = merge(stepframe, nostepframe, all=TRUE)

ggplot(data=myframe, aes(x=risk, y=spend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Initial spending rate") +
    xlab("Maximum shortfall") +
    theme_bw() +
    geom_point(size=3) +
    geom_line(size=1) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Initial Spending v. Worst Shortfall, stepdown v. no stepdown", breaks = c('step','nostep'),
                         values = c(dvblue,dvred),
                         labels = c("1% equity stepdown", "no stepdown")) +
     scale_shape_manual("Initial Spending v. Worst Shortfall, stepdown v. no stepdown", breaks = c('step','nostep'),
                        values = c(1,4),
                        labels = c("1% equity stepdown", "no stepdown"))



tmpframe = bigframe10
tmpframe$smooth = 1.0
jumboframe = tmpframe

tmpframe=bigframe15
tmpframe$smooth=1.5
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe20
tmpframe$smooth=2.0
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe25
tmpframe$smooth=2.5
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe30
tmpframe$smooth=3.0
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe35
tmpframe$smooth=3.5
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe40
tmpframe$smooth=4.0
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe45
tmpframe$smooth=4.5
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe50
tmpframe$smooth=5.0
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe55
tmpframe$smooth=5.5
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

tmpframe=bigframe60
tmpframe$smooth=6.0
jumboframe = merge(jumboframe, tmpframe, all=TRUE)

############################################################

tmpframe = bufframe10b1
tmpframe$smooth = 1.0
jumbobufframe = tmpframe

tmpframe=bufframe15b1
tmpframe$smooth=1.5
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe20b1
tmpframe$smooth=2.0
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe25b1
tmpframe$smooth=2.5
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe30b1
tmpframe$smooth=3.0
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe35b1
tmpframe$smooth=3.5
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe40b1
tmpframe$smooth=4.0
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe45b1
tmpframe$smooth=4.5
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe50b1
tmpframe$smooth=5.0
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe55b1
tmpframe$smooth=5.5
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

tmpframe=bufframe60b1
tmpframe$smooth=6.0
jumbobufframe = merge(jumbobufframe, tmpframe, all=TRUE)

############################################################

#figure 15
myframe = genfrontier2(jumboframe, 'initspend', 'worstshortfall', 'initspend')
myframe$risk = myframe$risk*100
myframe$frontier = 'frontier'

ggplot(data=myframe, aes(x=risk, y=spend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Initial spending rate") +
    xlab("Maximum shortfall") +
    theme_bw() +
    geom_point(size=3) +
    geom_line(size=1) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Initial Spending v. Worst Shortfall", breaks = c('frontier'),
                         values = c("#e41a1c"),
                         labels = c("Initial spending v. worst shortfall")) +
     scale_shape_manual("Initial Spending v. Worst Shortfall", breaks = c('frontier'),
                        values = c(1),
                         labels = c("Initial spending v. worst shortfall"))


#table data
myframe = genfrontier3(jumboframe, 'initspend', 'worstshortfall', 'initspend')


#figure 16
trials = testspendingfactor(realreturns, 0.5, 65, 100, 0.75, 0, 2, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.5, eq=75%, smooth=2)")


# figure 17 - buffer = 2
trials = testspendingfactor(realreturns, 0.5, 65, 100, 0.75, 0, 2, 2)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.5, eq=75%, smooth=2, lifespan buffer=2)")

# figure 18 - efficient frontier - max init spend v shortfall risk

buffer = genfrontier2(jumbobufframe, 'initspend', 'worstshortfall', 'initspend')
buffer$frontier='buffer'
buffer$risk = buffer$risk*100

nobuffer = genfrontier2(jumboframe, 'initspend', 'worstshortfall', 'initspend')
nobuffer$frontier='nobuffer'
nobuffer$risk = nobuffer$risk*100

frontiermelt <- merge(buffer,nobuffer, all=TRUE)

ggplot(data=frontiermelt, aes(x=risk, y=spend, colour=frontier, shape=frontier)) +
     scale_x_continuous() +
     ylab("Initial spend (%)") +
     xlab("Worst shortfall (%)") +
     theme_bw() +
     geom_point(size=3) +
     geom_line(size=1) +
     opts(legend.position="top",
          legend.direction = 'horizontal',
          plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Spend v. Worst Shortfall", breaks = c('buffer', 'nobuffer'),
                         values = c("#000099", "#990000"),
                         labels = c("Life expectancy buffer=1", "No buffer")) +
     scale_shape_manual("Spend v. Worst Shortfall", breaks = c('buffer', 'nobuffer'),
                        values = c(1, 4),
                        labels = c("Life expectancy buffer=1", "No buffer"))

# figure 19 - efficient frontier - lifetime spend v shortfall risk

buffer = genfrontier2(jumbobufframe, 'initspend', 'worstshortfall', 'expspend')
buffer$frontier='buffer'
buffer$risk = buffer$risk*100

nobuffer = genfrontier2(jumboframe, 'initspend', 'worstshortfall', 'expspend')
nobuffer$frontier='nobuffer'
nobuffer$risk = nobuffer$risk*100

frontiermelt <- merge(buffer,nobuffer, all=TRUE)

ggplot(data=frontiermelt, aes(x=risk, y=spend, colour=frontier, shape=frontier)) +
     scale_x_continuous() +
     ylab("Lifetime spend (%)") +
     xlab("Worst shortfall (%)") +
     theme_bw() +
     geom_point(size=3) +
     geom_line(size=1) +
     opts(legend.position="top",
          legend.direction = 'horizontal',
          plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Lifetime Spend v. Worst Shortfall", breaks = c('buffer', 'nobuffer'),
                         values = c("#000099", "#990000"),
                         labels = c("Life expectancy buffer=1", "No buffer")) +
     scale_shape_manual("Lifetime Spend v. Worst Shortfall", breaks = c('buffer', 'nobuffer'),
                        values = c(1, 4),
                        labels = c("Life expectancy buffer=1", "No buffer"))


# try max expspend
buffer = genfrontier2(bufframe20b2, 'initspend', 'worstshortfall', 'expspend')
buffer$frontier='buffer'

nobuffer = genfrontier2(bigframe20, 'initspend', 'worstshortfall', 'expspend')
nobuffer$frontier='nobuffer'

frontiermelt <- merge(buffer,nobuffer, all=TRUE)
chartfrontier(frontiermelt, 'buffer','Buffer','nobuffer', 'No Buffer')


# figure 19 - buffer = 2
trials = testspendingfactor(realreturns, 0.75, 65, 100, 0.95, 0, 2, 2)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.5, eq=75%, smooth=2, lifespan buffer=2)")


buffer = genfrontier3(bufframe20b2, 'expspend', 'worstshortfall', 'initspend')

frontier1 = genfrontier(bufframe40b1, 'initspend', 'worstshortfall')
frontier1$frontier='buffer'

frontier2 = genfrontier(bigframe40, 'initspend', 'worstshortfall')
frontier2$frontier='nobuffer'

frontiermelt <- merge(frontier1, frontier2, all=TRUE)
chartfrontier(frontiermelt, 'buffer','Buffer','nobuffer', 'No Buffer')

############################################################

frontier1 = genfrontier(bufframe30b3, 'expspend', 'worstshortfall')
frontier1$frontier='buffer'

frontier2 = genfrontier(bigframe30, 'expspend', 'worstshortfall')
frontier2$frontier='nobuffer'

frontiermelt <- merge(frontier1, frontier2, all=TRUE)
chartfrontier(frontiermelt, 'buffer','Buffer','nobuffer', 'No Buffer')

############################################################

frontier1 = genfrontier2(bufframe30b3, 'expspend', 'worstshortfall', 'initspend')
frontier1$frontier='buffer'

frontier2 = genfrontier2(bigframe30, 'expspend', 'worstshortfall', 'initspend')
frontier2$frontier='nobuffer'

frontiermelt <- merge(frontier1, frontier2, all=TRUE)
chartfrontier(frontiermelt, 'buffer','Buffer','nobuffer', 'No Buffer')


############################################################

frontier1 = genfrontier2(bufframe30b3, 'initspend', 'worstshortfall', 'expspend')
frontier1$frontier='buffer'

frontier2 = genfrontier2(bigframe30, 'initspend', 'worstshortfall', 'expspend')
frontier2$frontier='nobuffer'

frontiermelt <- merge(frontier1, frontier2, all=TRUE)
chartfrontier(frontiermelt, 'buffer','Buffer','nobuffer', 'No Buffer')

2 pairs of charts - max expspend - show initspend
                    v max initspend

                    max initspend - show expspend
                    v max expspend

same axis

x = ( highest acceptable shortfall )
y = max initspend achievable with that shortfall
y = max expspend achievable with that shortfall

chart8 = subset(bigframe50, (is.element(variable, c("sfactor","expspend","minspend","maxspend","plus1sd","minus1sd"))))
chart8 = subset(chart8, startequitypct < 0.61 & startequitypct > 0.59) # numerical rounding BS
# some crazy R bug / cruft I don't get - chart doesn't work unless I cast/melt
z = cast(chart8, sfactor~variable, mean)
z = data.frame(cast(chart8, sfactor~variable, mean))
meltframe = melt(z, id='sfactor')

ggplot(data=meltframe, aes(x=sfactor, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Lifetime spend expectancy")+
    xlab("Spending factor") +
    theme_bw() +
    geom_line(size=1.4) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Lifetime Spend Expectancy v. Spending Factor\nEquity 60%, smoothing 1/3.5", breaks = c('expspend', 'minspend','maxspend','plus1sd','minus1sd'),
                    values = c("#000099", "#CC0000", "#009900","#999999","#999999"),
                    labels = c('Mean', 'Worst investment case', 'Best investment case','+1SD','-1SD'))

# figure 9
chart9 = subset(bigframe50, (is.element(variable, c("sfactor","shortfall10","shortfall25","shortfall50"))))
chart9 = subset(chart9, startequitypct < 0.61 & startequitypct > 0.59) # numerical rounding BS
# some crazy R bug / cruft I don't get - chart doesn't work unless I cast/melt
z = cast(chart9, sfactor~variable, mean)
z = data.frame(cast(chart9, sfactor~variable, mean))
meltframe = melt(z, id='sfactor')

ggplot(data=meltframe, aes(x=sfactor, y=value, colour=variable)) +
    scale_x_continuous() +
    ylab("Shortfall probability") +
    xlab("Spending factor") +
    theme_bw() +
    geom_line(size=1.4) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Lifetime Spend Expectancy v. Spending Factor\nEquity 60%, smoothing 1/3.5", breaks = c('shortfall10','shortfall25','shortfall50'),
                    values = c("#000099", "#009900", "#CC0000"),
                    labels = c('10% shortfall probability','25% shortfall probability','50% Shortfall probability'))


# figure10 same as above

# figure 11 efficient frontier

chart10tmp = subset(bigframe10, startequitypct < 0.61 & startequitypct > 0.59) # numerical rounding BS
frontier1 = genfrontier(chart10, 'expspend', 'shortfall10')
#frontier1 = genfrontier(zStep0frame, 'expspend', 'shortfall10')
#frontier1$risk = frontier1$risk*100
frontier1$frontier='nosmooth10'

chart10tmp = subset(bigframe30, startequitypct < 0.61 & startequitypct > 0.59) # numerical rounding BS
frontier2 = genfrontier(chart10tmp, 'expspend', 'shortfall10')
frontier2$risk = frontier2$risk*100
frontier2$frontier='smooth10'

frontiermelt <- merge(frontier1, frontier2, all=TRUE)

ggplot(data=frontiermelt, aes(x=risk, y=spend, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("Lifetime spend exp (% of initial portfolio)") +
    xlab("10% shortfall probability") +
    theme_bw() +
    geom_point(size=3) +
    geom_line(size=1) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Lifetime spending expectancy v. Shortfall probability", breaks = c('nosmooth10','smooth10'),
                    values = c("#000099", "#990000"),
                    labels = c('No smoothing', 'Smoothed spending')) +
    scale_shape_manual("Lifetime spending expectancy v. Shortfall probability", breaks = c('nosmooth10','smooth10'),
                    values = c(1, 4),
                    labels = c('No smoothing', 'Smoothed spending'))



tmpframe = subset(bigframe30, startequitypct < 0.01 )
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "0"
frontiermelt <- tmpfrontier

tmpframe = subset(bigframe30, startequitypct < 0.11 & startequitypct > 0.09) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "10"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.21 & startequitypct > 0.19) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "20"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.31 & startequitypct > 0.29) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "30"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.41 & startequitypct > 0.39) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "40"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.51 & startequitypct > 0.49) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "50"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.61 & startequitypct > 0.59) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "60"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.71 & startequitypct > 0.69) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "70"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.81 & startequitypct > 0.79) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "80"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.91 & startequitypct > 0.89) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "90"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30,  startequitypct > 0.99) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = "x100"
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)


ggplot(data=frontiermelt, aes(x=risk, y=spend, colour=frontier)) +
    scale_x_continuous() +
    ylab("Lifetime spend exp (% of initial portfolio)") +
    xlab("10% shortfall probability") +
    theme_bw() +
    geom_line(size=1) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Efficient frontier, by equity%", breaks = c('0','10','20','30','40','50','60','70','80','90','x100'),
                    values = c(dvblue, dvyellow, dvgreen, dvpurple,dvorange,dvred,dvblack,dvlightblue,dvdarkgreen,dvpink,dvgray),
                    labels = c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))


ggplot(data=frontiermelt, aes(x=risk, y=spend, colour=frontier)) +
    facet_wrap(~ frontier, ncol = 2) +
    scale_x_continuous() +
    ylab("Lifetime spend exp (% of initial portfolio)") +
    xlab("10% shortfall probability") +
    theme_bw() +
    geom_line(size=1) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Efficient frontier, by equity%", breaks = c('0','10','20','30','40','50','60','70','80','90','x100'),
                    values = c(dvblue, dvyellow, dvgreen, dvpurple,dvorange,dvred,dvblack,dvlightblue,dvdarkgreen,dvpink,dvgray),
                    labels = c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))


tmpframe = subset(bigframe30, startequitypct < 0.01 )
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 0
frontiermelt <- tmpfrontier

tmpframe = subset(bigframe30, startequitypct < 0.11 & startequitypct > 0.09) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 10
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.21 & startequitypct > 0.19) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 20
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.31 & startequitypct > 0.29) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 30
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.41 & startequitypct > 0.39) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 40
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.51 & startequitypct > 0.49) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 50
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.61 & startequitypct > 0.59) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 60
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.71 & startequitypct > 0.69) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 70
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.81 & startequitypct > 0.79) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 80
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30, startequitypct < 0.91 & startequitypct > 0.89) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 90
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)

tmpframe = subset(bigframe30,  startequitypct > 0.99) # numerical rounding BS
tmpfrontier = genfrontier(tmpframe, 'expspend', 'shortfall10')
tmpfrontier$frontier = 100
frontiermelt <- merge(frontiermelt, tmpfrontier, all=TRUE)


wireframe(myGrid$z ~ myGrid$x * myGrid$y, xlab="X", ylab="Y", zlab="Z", main="title", drape=TRUE, colorkey=TRUE, screen=list(z=45, x=-60), scales = list(y = list(at = seq(from = -2, to = 2, by = 0.1))))

frontiermelt$risk = frontiermelt$risk *100
wireframe(zzz$spend ~ zzz$frontier * zzz$risk , ylab="Risk", xlab="Equity%", zlab="Spend", main="title", drape=TRUE, colorkey=TRUE)

, screen=list(z=45, x=-60))
scales = list(y = list(at = seq(from = 0, to = -50, by = -10))))
xxx

z10= data.frame(cast(bigframe10,sfactor+startequitypct~variable))
z10$smoothmult=1

z15= data.frame(cast(bigframe15,sfactor+startequitypct~variable))
z15$smoothmult=1.5
jumboframe=merge(z10,z15,all="TRUE")

z20= data.frame(cast(bigframe20,sfactor+startequitypct~variable))
z20$smoothmult=2.0
jumboframe=merge(z20,jumboframe,all="TRUE")

z25= data.frame(cast(bigframe25,sfactor+startequitypct~variable))
z25$smoothmult=2.5
jumboframe=merge(z25,jumboframe,all="TRUE")

z30= data.frame(cast(bigframe30,sfactor+startequitypct~variable))
z30$smoothmult=3.0
jumboframe=merge(z30,jumboframe,all="TRUE")

z35= data.frame(cast(bigframe35,sfactor+startequitypct~variable))
z35$smoothmult=3.5
jumboframe=merge(z35,jumboframe,all="TRUE")

z40= data.frame(cast(bigframe40,sfactor+startequitypct~variable))
z40$smoothmult=4.0
jumboframe=merge(z40,jumboframe,all="TRUE")

z45= data.frame(cast(bigframe45,sfactor+startequitypct~variable))
z45$smoothmult=4.5
jumboframe=merge(z45,jumboframe,all="TRUE")

z50= data.frame(cast(bigframe50,sfactor+startequitypct~variable))
z50$smoothmult=5.0
jumboframe=merge(z50,jumboframe,all="TRUE")

z55= data.frame(cast(bigframe55,sfactor+startequitypct~variable))
z55$smoothmult=5.5
jumboframe=merge(z55,jumboframe,all="TRUE")

z60= data.frame(cast(bigframe60,sfactor+startequitypct~variable))
z60$smoothmult=6.0
jumboframe=merge(z60,jumboframe,all="TRUE")

keep <- c("sfactor","startequitypct","smoothmult","expspend","shortfall10","shortfall25","shortfall50")
jumboframe <- jumboframe[,keep]

# highest expspend, shortfall10=0
tmpframe=subset(jumboframe, shortfall10==0)
tmpframe2 <- tmpframe[order(tmpframe$expspend,decreasing=T),]
resultsframe=data.frame(tmpframe2[1,])

# highest expspend, shortfall25=0
tmpframe=subset(jumboframe, shortfall25==0)
tmpframe2 <- tmpframe[order(tmpframe$expspend,decreasing=T),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# highest expspend, shortfall50=0
tmpframe=subset(jumboframe, shortfall50==0)
tmpframe2 <- tmpframe[order(tmpframe$expspend,decreasing=T),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# highest expspend, shortfall10=0, sfactor = 0.55,
tmpframe2=subset(tmpframe, tmpframe$sfactor>0.54)
tmpframe2 <- tmpframe2[order(tmpframe2$expspend,decreasing=T),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# highest expspend, shortfall10=0, sfactor = 0.6,
tmpframe2=subset(tmpframe, tmpframe$sfactor>0.59)
tmpframe2 <- tmpframe2[order(tmpframe2$expspend,decreasing=T),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.65
tmpframe=subset(jumboframe, sfactor>0.64 & sfactor<0.66)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.7
tmpframe=subset(jumboframe, sfactor>0.69 & sfactor<0.71)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.55
tmpframe=subset(jumboframe, sfactor>0.54 & sfactor<0.56)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.6
tmpframe=subset(jumboframe, sfactor>0.59 & sfactor<0.61)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.65
tmpframe=subset(jumboframe, sfactor>0.64 & sfactor<0.66)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.7
tmpframe=subset(jumboframe, sfactor>0.69 & sfactor<0.71)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.75
tmpframe=subset(jumboframe, sfactor>0.74 & sfactor<0.76)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.8
tmpframe=subset(jumboframe, sfactor>0.79 & sfactor<0.81)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.85
tmpframe=subset(jumboframe, sfactor>0.84 & sfactor<0.86)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.9
tmpframe=subset(jumboframe, sfactor>0.89 & sfactor<0.91)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 0.95
tmpframe=subset(jumboframe, sfactor>0.94 & sfactor<0.96)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# lowest shortfall10, sfactor = 1
tmpframe=subset(jumboframe, sfactor>0.99)
tmpframe2 <- tmpframe[order(tmpframe$shortfall10),]
resultsframe[nrow(resultsframe)+1,]=tmpframe2[1,]

# Figure 14.
# Spending factor 0.35; Shortfall risk 0%
trials = testspendingfactor(realreturns, 0.35, 65, 100, 0.75, 0, 4, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.35, eq=75%, smooth=4)")

# Spending factor 0.45; Shortfall risk 0.1%; Lifetime spend expectancy 89.7; Initial spend 2.6%
trials = testspendingfactor(realreturns, 0.45, 65, 100, 0.6, 0, 4, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.45, eq=60%, smooth=4)")

#Spending factor 0.5; Shortfall risk 0.1%; Lifetime spend expectancy 98.0; Initial spend  2.9%
trials = testspendingfactor(realreturns, 0.5, 65, 100, 0.65, 0, 4.5, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.5, eq=65%, smooth=4.5)")

#Spending factor 0.55; Shortfall risk 5.4%; Lifetime spend expectancy 110.2; Initial spend 3.2%
trials = testspendingfactor(realreturns, 0.55, 65, 100, 0.75, 0, 4, 0)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survival = ageframe65$survivepct*10000
keep <- c("year", "mean","min","max","minus1sd","plus1sd")
trialssummary <- trialssummary[,keep]
chartsummary(trialssummary, "Average Spending, All Cohorts (s=0.55, eq=75%, smooth=4)")


# this will take a very long time
bufframe10 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 1.0)
bufframe15 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 1.5)
bufframe20 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 2)
bufframe25 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 2.5)
bufframe30 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 3)
bufframe35 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 3.5)
bufframe40 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 4)
bufframe45 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 4.5)
bufframe50 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 5.0)
bufframe55 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 5.5)
bufframe60 = testsmooth(realreturns, ageframex, nsfactors, 0.6, 0, 6)

frontier1 = genfrontier(bufframe30, 'expspend', 'shortfall10')
frontier1$frontier='buffer'
frontier1$risk = frontier1$risk*100

frontier2 = genfrontier(bigframe30, 'expspend', 'shortfall10')
frontier2$frontier='nobuffer'
frontier2$risk = frontier2$risk*100

frontiermelt <- merge(frontier1, frontier2, all=TRUE)
chartfrontier(frontiermelt, 'buffer','Buffer','nobuffer', 'No Buffer')


z10= data.frame(cast(bufframe10,sfactor+startequitypct~variable))
z10$smoothmult=1

z15= data.frame(cast(bufframe15,sfactor+startequitypct~variable))
z15$smoothmult=1.5
jumboframe=merge(z10,z15,all="TRUE")

z20= data.frame(cast(bufframe20,sfactor+startequitypct~variable))
z20$smoothmult=2.0
jumboframe=merge(z20,jumboframe,all="TRUE")

z25= data.frame(cast(bufframe25,sfactor+startequitypct~variable))
z25$smoothmult=2.5
jumboframe=merge(z25,jumboframe,all="TRUE")

z30= data.frame(cast(bufframe30,sfactor+startequitypct~variable))
z30$smoothmult=3.0
jumboframe=merge(z30,jumboframe,all="TRUE")

z35= data.frame(cast(bufframe35,sfactor+startequitypct~variable))
z35$smoothmult=3.5
jumboframe=merge(z35,jumboframe,all="TRUE")

z40= data.frame(cast(bufframe40,sfactor+startequitypct~variable))
z40$smoothmult=4.0
jumboframe=merge(z40,jumboframe,all="TRUE")

z45= data.frame(cast(bufframe45,sfactor+startequitypct~variable))
z45$smoothmult=4.5
jumboframe=merge(z45,jumboframe,all="TRUE")

z50= data.frame(cast(bufframe50,sfactor+startequitypct~variable))
z50$smoothmult=5.0
jumboframe=merge(z50,jumboframe,all="TRUE")

z55= data.frame(cast(bufframe55,sfactor+startequitypct~variable))
z55$smoothmult=5.5
jumboframe=merge(z55,jumboframe,all="TRUE")

z60= data.frame(cast(bufframe60,sfactor+startequitypct~variable))
z60$smoothmult=6.0
jumboframe=merge(z60,jumboframe,all="TRUE")

keep <- c("sfactor","startequitypct","smoothmult","expspend","shortfall10","shortfall25","shortfall50")
jumboframe <- jumboframe[,keep]


z20=subset(z20, shortfall10==0)
z25=subset(z25, shortfall10==0)
z30=subset(z30, shortfall10==0)
z35=subset(z35, shortfall10==0)
z40=subset(z40, shortfall10==0)
z45=subset(z45, shortfall10==0)
z50=subset(z50, shortfall10==0)
z55=subset(z55, shortfall10==0)
z60=subset(z60, shortfall10==0)


bigframeeq0 = subset(bigframe30, startequitypct ==0 )
frontiereq0 = genfrontier(bigframeeq0, 'expspend', 'shortfall10')
frontiereq0$frontier='eq0'

bigframeeq10 = subset(bigframe30, startequitypct == 0.1)
frontiereq10 = genfrontier(bigframeeq10, 'expspend', 'shortfall10')
frontiereq10$frontier = 'eq10'
frontiermelt <- merge(frontiereq0, frontiereq10, all=TRUE)

bigframeeq20 = subset(bigframe30, startequitypct == 0.2)
frontiereq20 = genfrontier(bigframeeq20, 'expspend', 'shortfall10')
frontiereq20$frontier = "eq20"
frontiermelt <- merge(frontiermelt, frontiereq20, all=TRUE)

bigframeeq30 = subset(bigframe30, startequitypct < 0.31 & startequitypct > 0.29)
frontiereq30 = genfrontier(bigframeeq30, 'expspend', 'shortfall10')
frontiereq30$frontier = "eq30"
frontiermelt <- merge(frontiermelt, frontiereq30, all=TRUE)

bigframeeq40 = subset(bigframe30, startequitypct>0.39 & startequitypct < 0.41)
frontiereq40 = genfrontier(bigframeeq40, 'expspend', 'shortfall10')
frontiereq40$frontier = "eq40"
frontiermelt <- merge(frontiermelt, frontiereq40, all=TRUE)

bigframeeq50 = subset(bigframe30, startequitypct>0.49 & startequitypct < 0.51)
frontiereq50 = genfrontier(bigframeeq50, 'expspend', 'shortfall10')
frontiereq50$frontier = "eq50"
frontiermelt <- merge(frontiermelt, frontiereq50, all=TRUE)

bigframeeq60 = subset(bigframe30, startequitypct>0.59 & startequitypct < 0.61)
frontiereq60 = genfrontier(bigframeeq60, 'expspend', 'shortfall10')
frontiereq60$frontier = "eq60"
frontiermelt <- merge(frontiermelt, frontiereq60, all=TRUE)

bigframeeq70 = subset(bigframe30, startequitypct>0.69 & startequitypct < 0.71)
frontiereq70 = genfrontier(bigframeeq70, 'expspend', 'shortfall10')
frontiereq70$frontier = "eq70"
frontiermelt <- merge(frontiermelt, frontiereq70, all=TRUE)

bigframeeq80 = subset(bigframe30, startequitypct>0.79 & startequitypct < 0.81)
frontiereq80 = genfrontier(bigframeeq80, 'expspend', 'shortfall10')
frontiereq80$frontier = "eq80"
frontiermelt <- merge(frontiermelt, frontiereq80, all=TRUE)

bigframeeq90 = subset(bigframe30, startequitypct>0.89 & startequitypct < 0.91)
frontiereq90 = genfrontier(bigframeeq90, 'expspend', 'shortfall10')
frontiereq90$frontier = "eq90"
frontiermelt <- merge(frontiermelt, frontiereq90, all=TRUE)

bigframeeq100 = subset(bigframe30, startequitypct>0.99 & startequitypct < 1.01)
frontiereq100 = genfrontier(bigframeeq100, 'expspend', 'shortfall10')
frontiereq100$frontier = "eqx100"
frontiermelt <- merge(frontiermelt, frontiereq100, all=TRUE)



ggplot(data=frontiermelt, aes(x=risk, y=spend, colour=frontier)) +
    facet_wrap(~ frontier, ncol = 2) +
    scale_x_continuous() +
    ylab("Lifetime spend exp (% of initial portfolio)") +
    xlab("10% shortfall probability") +
    theme_bw() +
    geom_line(size=1) +
    opts(legend.position="none",
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Efficient frontier, by equity%", breaks = c('eq0','eq10','eq20','eq30','eq40','eq50','eq60','eq70','eq80','eq90','eqx100'),
                    labels = c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'),
                    values = c("#000099", "#CC0000", "#009900","#CC0099", "#CC9900", "#CC9900","#00004545", "#660000", "#004500","#000000","#999999"))

frontier1$frontier='nosmooth6040'
frontier3$frontier='smooth6040'


shortfall10frame = data.frame (sfactor = double(), startequitypct=double(), variable=character(), value=double())
shortfall25frame = data.frame (sfactor = double(), startequitypct=double(), variable=character(), value=double())
shortfall50frame = data.frame (sfactor = double(), startequitypct=double(), variable=character(), value=double())

expspendframe$startequitypct = expspendframe$startequitypct * 100

shortfall25frame$startequitypct = shortfall25frame$startequitypct * 100
shortfall25frame$value = shortfall25frame$value * 100

shortfall10frame$startequitypct = shortfall10frame$startequitypct * 100
shortfall10frame$value = shortfall10frame$value * 100

shortfall50frame$startequitypct = shortfall50frame$startequitypct * 100
shortfall50frame$value = shortfall50frame$value * 100

zframe <- merge(expspendframe, shortfall10frame, all=TRUE)
zframe <- merge(zframe, shortfall25frame, all=TRUE)
zframe <- merge(zframe, shortfall50frame, all=TRUE)


ggplot(data=zframe, aes(x=sfactor, y=value, colour=variable)) +
     scale_x_continuous() +
     ylab("Annual spending (% of initial portfolio)") +
     xlab("Spending factor") +
     theme_bw() +
     facet_wrap(~ startequitypct, ncol = 3) +
     geom_line(size=1) +
     opts(legend.position="top",
          legend.direction = 'horizontal',
          plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
     scale_colour_manual("Lifetime spend expectancy v spend factor, by start equity pct", breaks = c('expspend', 'shortfall10', 'shortfall25', 'shortfall50'),
                         values = c("#000099", "#009900","#CC0000","#999999","#999999"),
                         labels = c('Lifetime Spend Expectancy', '10% Shortfall Probability', '25% Shortfall Probability', '50% Shortfall Probability'))

zStep0frame = zframe

f1 = subset(zStep0frame, zStep0frame["variable"]=="expspend")
f2 = subset(zStep0frame, zStep0frame["variable"]=="shortfall25")
frontier25 = merge(f1, f2, by=c('sfactor','startequitypct'))
keep <- c("value.x","value.y")
frontier25 <- frontier25[keep]
colnames(frontier25) = c("expspend","shortfall25")

.55 55 90.2 8.25

frontierframe = data.frame (spend=double(), risk = double())
colnames(frontierframe) = c("spend","risk")
tmpframe = frontier25

while (nrow(tmpframe) > 0)
{
    minRisk = min(tmpframe$shortfall25)
    tmp2 = subset(tmpframe, tmpframe["shortfall25"]==minRisk)
    maxRet = max(tmp2$expspend)
    newrow=c(maxRet,minRisk)
    frontierframe=rbind(frontierframe, newrow)

    tmpframe = subset(tmpframe, tmpframe["shortfall25"] > minRisk)
    tmpframe = subset(tmpframe, tmpframe["expspend"] >= maxRet)
}
colnames(frontierframe) = c("spend","risk")

frontier0 = frontierframe

frontier0$variable = "step0"
frontier1$variable = "step1"
frontiers = merge(frontier0, frontier1, all=TRUE)

ggplot(data=frontiers, aes(x=risk, y=spend, colour=variable, shape=variable)) +
    scale_x_continuous() +
    ylab("Lifetime spend exp (% of initial portfolio)") +
    xlab("25% shortfall probability") +
    theme_bw() +
    geom_point(size=3) +
    opts(legend.position="top",
         legend.direction = 'horizontal',
         plot.background = theme_rect(colour = 'black', fill = '#CCCCEE', size = 1, linetype='solid')) +
scale_colour_manual("Efficient frontier", breaks = c('step0','step1'),
                    values = c("#000099", "#990000"),
                    labels = c('level equity', 'decreasing equity')) +
    scale_shape_manual("Efficient frontier", breaks = c('step0','step1'),
                    values = c(1, 3),
                    labels = c('level equity', 'decreasing equity'))




# fixed 4% rule
trials = testspendingfactorfixed(realreturns, 1, 65, 100)
trialssummary = calctrialssummary(trials)
ageframe65 = ageframe(lifetable,65,100)
trialssummary$survivepct=ageframe65$survivepct
trialssummary$expmean = trialssummary$mean * trialssummary$survivepct
sum(trialssummary$expmean)

trials[trials<4]=-1
trials[trials>0]=0
trials[trials<0]=1
trials[,]=trials[,] * ageframe65$survivepct
mean(apply(trials,2,max))

# 0.5 spending factor -> look up from sfactorsummary


step 0 - spend


myframe=bigframe30

f1 = subset(myframe, myframe["variable"]=="expspend")
f2 = subset(myframe, myframe["variable"]=="worstshortfall")

tmpframe = merge(f1, f2, by=c('sfactor','startequitypct'))
keep <- c("value.x","value.y")
tmpframe <- tmpframe[keep]
colnames(tmpframe) = c("expspend", "worstshortfall")

frontierframe = data.frame (spend=double(), risk = double())
colnames(frontierframe) = c("spend","risk")

while (nrow(tmpframe) > 0)
{
    minRisk = min(tmpframe[,])
    tmp2 = subset(tmpframe, tmpframe[riskColName]==minRisk)
    maxRet = max(tmp2[, returnColName])
    newrow=c(maxRet,minRisk)
    frontierframe=rbind(frontierframe, newrow)

    tmpframe = subset(tmpframe, tmpframe["worstshortfall"] > minRisk)
    if (nrow(tmpframe) > 0)
        tmpframe = subset(tmpframe, tmpframe["expspend"] >= maxRet)
}

colnames(frontierframe) = c("spend","risk")
return(frontierframe)
}






