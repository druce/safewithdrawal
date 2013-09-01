import numpy as np
import pandas as pd
from pandas import *
import datetime
import pdb
import MySQLdb as mdb
import argparse

dbserver = 'localhost'
dbuser =  'safe'
dbpass =  'safe'
dbname = 'safewithdrawal'

tableColumns = {
    'id' : 0,
    'status' : 1,
    'ret_age' : 2,
    'ret_length' : 3,
    'ret_sex' : 4,
    'param_equitypct' : 5,
    'param_sfactor' : 6,
    'param_smoother' : 7,
    'param_attenuator' : 8,
    'param_sconstant' : 9,
    'initspend' : 10,
    'worst_shortfall' : 11,
    'lifespend' : 12,
    'ce1' : 13,
    'ce2' : 14,
    'ce4' : 15,
    'ce6' : 16,
    'ce8' : 17,
    'probmaxdecline0' : 18,
    'probmaxdecline10' : 19,
    'probmaxdecline20' : 20,
    'probmaxdecline30' : 21,
    'probmaxdecline40' : 22,
    'probmaxdecline50' : 23,
    'spend_std' : 24,
    'shortfall_percentile90' : 25,
    'shortfall_percentile80' : 26,
    'shortfall_percentile70' : 27,
    'shortfall_percentile60' : 28,
    'shortfall_percentile50' : 29,
}

def db_connect():
    try:
        con = mdb.connect(dbserver, dbuser, dbpass, dbname)
    except mdb.Error, e:
        print "Database connection error %d: %s" % (e.args[0], e.args[1])
        if con:
            con.close()
            return False
    return con


def do_query(con, sqlstr, debug=False):

    if debug:
        print sqlstr

    with con:
        try:
            cur = con.cursor()
            cur.execute(sqlstr)
            rows = cur.fetchall()
        except mdb.Error,e:
            print "Database query error %d: %s" % (e.args[0], e.args[1])
            if con:
                con.close()
                return False

    return rows



def efrontier(con, max_feature, min_feature):

    #pdb.set_trace()
    max_index = tableColumns[max_feature]
    min_index = tableColumns[min_feature]

    id_array = []
    max_array = []
    min_array = []
    param_equitypct_array=[]
    param_sfactor_array=[]
    param_smoother_array=[]
    param_attenuator_array=[]
    param_sconstant_array=[]
    worst_array=[]
    prob10_array=[]
    initspend_array=[]
    lifespend_array=[]
    ce4_array=[]

    rows = do_query(con, "select * from param_metrics where %s = (select min(%s) from param_metrics) order by %s desc limit 1;" % (min_feature, min_feature, max_feature))
    best = rows[0][max_index]
    bestmin = rows[0][min_index]

    print "%d: %.2f %.2f" % (0, best, bestmin)

    id_array.append(rows[0][tableColumns['id']])
    max_array.append(rows[0][tableColumns[max_feature]])
    min_array.append(rows[0][tableColumns[min_feature]])
    param_equitypct_array.append(rows[0][tableColumns['param_equitypct']])
    param_sfactor_array.append(rows[0][tableColumns['param_sfactor']])
    param_smoother_array.append(rows[0][tableColumns['param_smoother']])
    param_attenuator_array.append(rows[0][tableColumns['param_attenuator']])
    param_sconstant_array.append(rows[0][tableColumns['param_sconstant']])
    # always include these
    initspend_array.append(rows[0][tableColumns['initspend']])
    lifespend_array.append(rows[0][tableColumns['lifespend']])
    ce4_array.append(rows[0][tableColumns['ce4']])
    worst_array.append(rows[0][tableColumns['worst_shortfall']])
    prob10_array.append(rows[0][tableColumns['probmaxdecline10']])

    c=0
    while len(rows) > 0:
        sqlstr = """
select * from param_metrics
where %s = (select min(%s) from param_metrics where %s > %.11f)
order by %s desc limit 1;
""" % (min_feature, min_feature, max_feature, best, max_feature)
        #print sqlstr
        rows = do_query(con, sqlstr)
        if rows:
            # print rows
            best = rows[0][max_index]
            id_array.append(rows[0][tableColumns['id']])
            max_array.append(rows[0][tableColumns[max_feature]])
            min_array.append(rows[0][tableColumns[min_feature]])
            param_equitypct_array.append(rows[0][tableColumns['param_equitypct']])
            param_sfactor_array.append(rows[0][tableColumns['param_sfactor']])
            param_smoother_array.append(rows[0][tableColumns['param_smoother']])
            param_attenuator_array.append(rows[0][tableColumns['param_attenuator']])
            param_sconstant_array.append(rows[0][tableColumns['param_sconstant']])
            initspend_array.append(rows[0][tableColumns['initspend']])
            lifespend_array.append(rows[0][tableColumns['lifespend']])
            ce4_array.append(rows[0][tableColumns['ce4']])
            worst_array.append(rows[0][tableColumns['worst_shortfall']])
            prob10_array.append(rows[0][tableColumns['probmaxdecline10']])
            print "%d: %.2f %.2f" % (c+1, rows[0][tableColumns[max_feature]], rows[0][tableColumns[min_feature]])
        c+=1

    retdf = DataFrame({'id' : Series(id_array),
                       max_feature : Series(max_array),
                       min_feature: Series(min_array),
                       'equitypct' : Series(param_equitypct_array),
                       'sfactor' : Series(param_sfactor_array),
                       'smoother' : Series(param_smoother_array),
                       'attenuator' : Series(param_attenuator_array),
                       'sconstant' : Series(param_sconstant_array),
                       'init' : Series(initspend_array),
                       'life' : Series(lifespend_array),
                       'ce4' : Series(ce4_array),
                       'worst' : Series(worst_array),
                       'prob10' : Series(prob10_array),
                       })

    if max_feature == 'initspend':
    #truncate last row, which is just the highest spending tested
        return retdf[:-1]
    else:
        return retdf

if __name__ == '__main__':
##################################################################
    # generate frontier 1
##################################################################
    con = db_connect()

    initspend_v_worst_shortfall = efrontier(con, 'initspend', 'worst_shortfall')
    print initspend_v_worst_shortfall
    initspend_v_percentile90 = efrontier(con, 'initspend', 'shortfall_percentile90')
    initspend_v_percentile80 = efrontier(con, 'initspend', 'shortfall_percentile80')
    initspend_v_percentile70 = efrontier(con, 'initspend', 'shortfall_percentile70')
    initspend_v_percentile60 = efrontier(con, 'initspend', 'shortfall_percentile60')
    initspend_v_percentile50 = efrontier(con, 'initspend', 'shortfall_percentile50')

# save to CSV
    initspend_v_worst_shortfall.to_csv('initspend_v_worst_shortfall.csv', index_label='index')
    initspend_v_percentile90.to_csv('initspend_v_percentile90.csv', index_label='index')
    initspend_v_percentile80.to_csv('initspend_v_percentile80.csv', index_label='index')
    initspend_v_percentile70.to_csv('initspend_v_percentile70.csv', index_label='index')
    initspend_v_percentile60.to_csv('initspend_v_percentile60.csv', index_label='index')
    initspend_v_percentile50.to_csv('initspend_v_percentile50.csv', index_label='index')

##################################################################
# generate frontier 2
##################################################################

    lifespend_v_worst_shortfall = efrontier(con, 'lifespend', 'worst_shortfall')
    print lifespend_v_worst_shortfall
    lifespend_v_percentile90 = efrontier(con, 'lifespend', 'shortfall_percentile90')
    lifespend_v_percentile80 = efrontier(con, 'lifespend', 'shortfall_percentile80')
    lifespend_v_percentile70 = efrontier(con, 'lifespend', 'shortfall_percentile70')
    lifespend_v_percentile60 = efrontier(con, 'lifespend', 'shortfall_percentile60')
    lifespend_v_percentile50 = efrontier(con, 'lifespend', 'shortfall_percentile50')

# save to CSV
    lifespend_v_worst_shortfall.to_csv('lifespend_v_worst_shortfall.csv', index_label='index')
    lifespend_v_percentile90.to_csv('lifespend_v_percentile90.csv', index_label='index')
    lifespend_v_percentile80.to_csv('lifespend_v_percentile80.csv', index_label='index')
    lifespend_v_percentile70.to_csv('lifespend_v_percentile70.csv', index_label='index')
    lifespend_v_percentile60.to_csv('lifespend_v_percentile60.csv', index_label='index')
    lifespend_v_percentile50.to_csv('lifespend_v_percentile50.csv', index_label='index')


##################################################################
# generate frontier 3
##################################################################

    initspend_v_probmaxdecline0 = efrontier(con, 'initspend', 'probmaxdecline0')
    print initspend_v_probmaxdecline0
    initspend_v_probmaxdecline10 = efrontier(con, 'initspend', 'probmaxdecline10')
    initspend_v_probmaxdecline20 = efrontier(con, 'initspend', 'probmaxdecline20')
    initspend_v_probmaxdecline30 = efrontier(con, 'initspend', 'probmaxdecline30')
    initspend_v_probmaxdecline40 = efrontier(con, 'initspend', 'probmaxdecline40')
    initspend_v_probmaxdecline50 = efrontier(con, 'initspend', 'probmaxdecline50')
# save to CSV
    initspend_v_probmaxdecline0.to_csv('initspend_v_probmaxdecline0.csv', index_label='index')
    initspend_v_probmaxdecline10.to_csv('initspend_v_probmaxdecline10.csv', index_label='index')
    initspend_v_probmaxdecline20.to_csv('initspend_v_probmaxdecline20.csv', index_label='index')
    initspend_v_probmaxdecline30.to_csv('initspend_v_probmaxdecline30.csv', index_label='index')
    initspend_v_probmaxdecline40.to_csv('initspend_v_probmaxdecline40.csv', index_label='index')
    initspend_v_probmaxdecline50.to_csv('initspend_v_probmaxdecline50.csv', index_label='index')

##################################################################
# generate frontier 4
##################################################################

    lifespend_v_probmaxdecline0 = efrontier(con, 'lifespend', 'probmaxdecline0')
    print initspend_v_probmaxdecline0
    lifespend_v_probmaxdecline10 = efrontier(con, 'lifespend', 'probmaxdecline10')
    lifespend_v_probmaxdecline20 = efrontier(con, 'lifespend', 'probmaxdecline20')
    lifespend_v_probmaxdecline30 = efrontier(con, 'lifespend', 'probmaxdecline30')
    lifespend_v_probmaxdecline40 = efrontier(con, 'lifespend', 'probmaxdecline40')
    lifespend_v_probmaxdecline50 = efrontier(con, 'lifespend', 'probmaxdecline50')
# save to CSV
    lifespend_v_probmaxdecline0.to_csv('lifespend_v_probmaxdecline0.csv', index_label='index')
    lifespend_v_probmaxdecline10.to_csv('lifespend_v_probmaxdecline10.csv', index_label='index')
    lifespend_v_probmaxdecline20.to_csv('lifespend_v_probmaxdecline20.csv', index_label='index')
    lifespend_v_probmaxdecline30.to_csv('lifespend_v_probmaxdecline30.csv', index_label='index')
    lifespend_v_probmaxdecline40.to_csv('lifespend_v_probmaxdecline40.csv', index_label='index')
    lifespend_v_probmaxdecline50.to_csv('lifespend_v_probmaxdecline50.csv', index_label='index')


##################################################################
# print r file to load CSV and generate graph
##################################################################

    strdict = {
        "filename" : "initspend_v_worst_shortfall",
        "dirname" : "c:/Users/druce/ec2/safewithdrawal",
        "ydata" : "initspend",
        "xdata" : "worst_shortfall",
        "ylab"  : "Initial spending rate",
        "xlab"  : "Worst shortfall",
        "legendlabel" : "Initial spending v. worst shortfall",
        }

    rfilestr = """
library("reshape")
library("ggplot2")

setwd("%(dirname)s")

efrontier <- read.csv("%(filename)s.csv")

keep = c("%(ydata)s","%(xdata)s")
plotframe = efrontier[, keep]
plotframe$frontier = "frontier"

ggplot(data=plotframe, aes(x=%(xdata)s, y=%(ydata)s, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("%(ylab)s") +
    xlab("%(xlab)s") +
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
                        labels = c("%(legendlabel)s")) +
    scale_shape_manual("", breaks = c("frontier"),
                       values = c(1),
                       labels = c("%(legendlabel)s")) +
    expand_limits(x = 0, y = 0)

""" % strdict

#nwith open('%s.r'%strdict['filename'], 'w') as f:
#    f.write(rfilestr)


##################################################################
# print r file to load CSV and generate graph
##################################################################

    strdict = {
        "filename" : "lifespend_v_worst_shortfall",
        "dirname" : "c:/Users/druce/ec2/safewithdrawal",
        "ydata" : "lifespend",
        "xdata" : "worst_shortfall",
        "ylab"  : "Lifetime spending",
        "xlab"  : "Worst shortfall",
        "legendlabel" : "Lifetime spending v. worst shortfall",
        }

    rfilestr = """
library("reshape")
library("ggplot2")

setwd("%(dirname)s")

efrontier <- read.csv("%(filename)s.csv")

keep = c("%(ydata)s","%(xdata)s")
plotframe = efrontier[, keep]
plotframe$frontier = "frontier"

ggplot(data=plotframe, aes(x=%(xdata)s, y=%(ydata)s, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("%(ylab)s") +
    xlab("%(xlab)s") +
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
                        labels = c("%(legendlabel)s")) +
    scale_shape_manual("", breaks = c("frontier"),
                       values = c(1),
                       labels = c("%(legendlabel)s")) +
    expand_limits(x = 0, y = 0)

""" % strdict

#$with open('%s.r'%strdict['filename'], 'w') as f:
#    f.write(rfilestr)


##################################################################
# print r file to load CSV and generate graph
##################################################################

    strdict = {
        "filename" : "initspend_v_probmaxdecline0",
        "dirname" : "c:/Users/druce/ec2/safewithdrawal",
        "ydata" : "initspend",
        "xdata" : "probmaxdecline0",
        "ylab"  : "Initial spending",
        "xlab"  : "Probability of shortfall",
        "legendlabel" : "Initial spending v. probability of shortfall",
        }

    rfilestr = """
library("reshape")
library("ggplot2")

setwd("%(dirname)s")

efrontier <- read.csv("%(filename)s.csv")

keep = c("%(ydata)s","%(xdata)s")
plotframe = efrontier[, keep]
plotframe$frontier = "frontier"

ggplot(data=plotframe, aes(x=%(xdata)s, y=%(ydata)s, colour=frontier, shape=frontier)) +
    scale_x_continuous() +
    ylab("%(ylab)s") +
    xlab("%(xlab)s") +
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
                        labels = c("%(legendlabel)s")) +
    scale_shape_manual("", breaks = c("frontier"),
                       values = c(1),
                       labels = c("%(legendlabel)s")) +
    expand_limits(x = 0, y = 0)

""" % strdict

#with open('%s.r'%strdict['filename'], 'w') as f:
#    f.write(rfilestr)





