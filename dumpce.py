import numpy as np
import pandas as pd
from pandas import *
import datetime
import pdb
import MySQLdb as mdb
import argparse

from dump import *

con = db_connect()

##################################################################
# generate frontier, gamma= 1
##################################################################

ce1_v_worst_shortfall = efrontier(con, 'ce1', 'worst_shortfall')
print ce1_v_worst_shortfall
# ce1_v_percentile90 = efrontier(con, 'ce1', 'shortfall_percentile90')
# ce1_v_percentile80 = efrontier(con, 'ce1', 'shortfall_percentile80')
# ce1_v_percentile70 = efrontier(con, 'ce1', 'shortfall_percentile70')
# ce1_v_percentile60 = efrontier(con, 'ce1', 'shortfall_percentile60')
# ce1_v_percentile50 = efrontier(con, 'ce1', 'shortfall_percentile50')

# save to CSV
ce1_v_worst_shortfall.to_csv('ce1_v_worst_shortfall.csv', index_label='index')
# ce1_v_percentile90.to_csv('ce1_v_percentile90.csv', index_label='index')
# ce1_v_percentile80.to_csv('ce1_v_percentile80.csv', index_label='index')
# ce1_v_percentile70.to_csv('ce1_v_percentile70.csv', index_label='index')
# ce1_v_percentile60.to_csv('ce1_v_percentile60.csv', index_label='index')
# ce1_v_percentile50.to_csv('ce1_v_percentile50.csv', index_label='index')

##################################################################
# generate frontier, gamma= 2
##################################################################

ce2_v_worst_shortfall = efrontier(con, 'ce2', 'worst_shortfall')
print ce2_v_worst_shortfall
# ce2_v_percentile90 = efrontier(con, 'ce2', 'shortfall_percentile90')
# ce2_v_percentile80 = efrontier(con, 'ce2', 'shortfall_percentile80')
# ce2_v_percentile70 = efrontier(con, 'ce2', 'shortfall_percentile70')
# ce2_v_percentile60 = efrontier(con, 'ce2', 'shortfall_percentile60')
# ce2_v_percentile50 = efrontier(con, 'ce2', 'shortfall_percentile50')

# save to CSV
ce2_v_worst_shortfall.to_csv('ce2_v_worst_shortfall.csv', index_label='index')
# ce2_v_percentile90.to_csv('ce2_v_percentile90.csv', index_label='index')
# ce2_v_percentile80.to_csv('ce2_v_percentile80.csv', index_label='index')
# ce2_v_percentile70.to_csv('ce2_v_percentile70.csv', index_label='index')
# ce2_v_percentile60.to_csv('ce2_v_percentile60.csv', index_label='index')
# ce2_v_percentile50.to_csv('ce2_v_percentile50.csv', index_label='index')

##################################################################
# generate frontier, gamma= 4
##################################################################

ce4_v_worst_shortfall = efrontier(con, 'ce4', 'worst_shortfall')
print ce4_v_worst_shortfall
ce4_v_percentile90 = efrontier(con, 'ce4', 'shortfall_percentile90')
ce4_v_percentile80 = efrontier(con, 'ce4', 'shortfall_percentile80')
ce4_v_percentile70 = efrontier(con, 'ce4', 'shortfall_percentile70')
ce4_v_percentile60 = efrontier(con, 'ce4', 'shortfall_percentile60')
ce4_v_percentile50 = efrontier(con, 'ce4', 'shortfall_percentile50')

# save to CSV
ce4_v_worst_shortfall.to_csv('ce4_v_worst_shortfall.csv', index_label='index')
ce4_v_percentile90.to_csv('ce4_v_percentile90.csv', index_label='index')
ce4_v_percentile80.to_csv('ce4_v_percentile80.csv', index_label='index')
ce4_v_percentile70.to_csv('ce4_v_percentile70.csv', index_label='index')
ce4_v_percentile60.to_csv('ce4_v_percentile60.csv', index_label='index')
ce4_v_percentile50.to_csv('ce4_v_percentile50.csv', index_label='index')

##################################################################
# generate frontier, gamma= 8
##################################################################

ce8_v_worst_shortfall = efrontier(con, 'ce8', 'worst_shortfall')
print ce8_v_worst_shortfall
ce8_v_percentile90 = efrontier(con, 'ce8', 'shortfall_percentile90')
ce8_v_percentile80 = efrontier(con, 'ce8', 'shortfall_percentile80')
ce8_v_percentile70 = efrontier(con, 'ce8', 'shortfall_percentile70')
ce8_v_percentile60 = efrontier(con, 'ce8', 'shortfall_percentile60')
ce8_v_percentile50 = efrontier(con, 'ce8', 'shortfall_percentile50')


# save to CSV
ce8_v_worst_shortfall.to_csv('ce8_v_worst_shortfall.csv', index_label='index')
ce8_v_percentile90.to_csv('ce8_v_percentile90.csv', index_label='index')
ce8_v_percentile80.to_csv('ce8_v_percentile80.csv', index_label='index')
ce8_v_percentile70.to_csv('ce8_v_percentile70.csv', index_label='index')
ce8_v_percentile60.to_csv('ce8_v_percentile60.csv', index_label='index')
ce8_v_percentile50.to_csv('ce8_v_percentile50.csv', index_label='index')


##################################################################
# generate frontier , gamma=1
##################################################################

ce1_v_probmaxdecline0 = efrontier(con, 'ce1', 'probmaxdecline0')
print ce1_v_probmaxdecline0
# ce1_v_probmaxdecline10 = efrontier(con, 'ce1', 'probmaxdecline10')
# ce1_v_probmaxdecline20 = efrontier(con, 'ce1', 'probmaxdecline20')
# ce1_v_probmaxdecline30 = efrontier(con, 'ce1', 'probmaxdecline30')
# ce1_v_probmaxdecline40 = efrontier(con, 'ce1', 'probmaxdecline40')
# ce1_v_probmaxdecline50 = efrontier(con, 'ce1', 'probmaxdecline50')
# save to CSV
ce1_v_probmaxdecline0.to_csv('ce1_v_probmaxdecline0.csv', index_label='index')
# ce1_v_probmaxdecline10.to_csv('ce1_v_probmaxdecline10.csv', index_label='index')
# ce1_v_probmaxdecline20.to_csv('ce1_v_probmaxdecline20.csv', index_label='index')
# ce1_v_probmaxdecline30.to_csv('ce1_v_probmaxdecline30.csv', index_label='index')
# ce1_v_probmaxdecline40.to_csv('ce1_v_probmaxdecline40.csv', index_label='index')
# ce1_v_probmaxdecline50.to_csv('ce1_v_probmaxdecline50.csv', index_label='index')

##################################################################
# generate frontier , gamma=2
##################################################################

ce2_v_probmaxdecline0 = efrontier(con, 'ce2', 'probmaxdecline0')
print ce2_v_probmaxdecline0
# ce2_v_probmaxdecline10 = efrontier(con, 'ce2', 'probmaxdecline10')
# ce2_v_probmaxdecline20 = efrontier(con, 'ce2', 'probmaxdecline20')
# ce2_v_probmaxdecline30 = efrontier(con, 'ce2', 'probmaxdecline30')
# ce2_v_probmaxdecline40 = efrontier(con, 'ce2', 'probmaxdecline40')
# ce2_v_probmaxdecline50 = efrontier(con, 'ce2', 'probmaxdecline50')
# save to CSV
ce2_v_probmaxdecline0.to_csv('ce2_v_probmaxdecline0.csv', index_label='index')
# ce2_v_probmaxdecline10.to_csv('ce2_v_probmaxdecline10.csv', index_label='index')
# ce2_v_probmaxdecline20.to_csv('ce2_v_probmaxdecline20.csv', index_label='index')
# ce2_v_probmaxdecline30.to_csv('ce2_v_probmaxdecline30.csv', index_label='index')
# ce2_v_probmaxdecline40.to_csv('ce2_v_probmaxdecline40.csv', index_label='index')
# ce2_v_probmaxdecline50.to_csv('ce2_v_probmaxdecline50.csv', index_label='index')

##################################################################
# generate frontier , gamma=4
##################################################################

ce4_v_probmaxdecline0 = efrontier(con, 'ce4', 'probmaxdecline0')
print ce4_v_probmaxdecline0
ce4_v_probmaxdecline10 = efrontier(con, 'ce4', 'probmaxdecline10')
ce4_v_probmaxdecline20 = efrontier(con, 'ce4', 'probmaxdecline20')
ce4_v_probmaxdecline30 = efrontier(con, 'ce4', 'probmaxdecline30')
ce4_v_probmaxdecline40 = efrontier(con, 'ce4', 'probmaxdecline40')
ce4_v_probmaxdecline50 = efrontier(con, 'ce4', 'probmaxdecline50')
# save to CSV
ce4_v_probmaxdecline0.to_csv('ce4_v_probmaxdecline0.csv', index_label='index')
ce4_v_probmaxdecline10.to_csv('ce4_v_probmaxdecline10.csv', index_label='index')
ce4_v_probmaxdecline20.to_csv('ce4_v_probmaxdecline20.csv', index_label='index')
ce4_v_probmaxdecline30.to_csv('ce4_v_probmaxdecline30.csv', index_label='index')
ce4_v_probmaxdecline40.to_csv('ce4_v_probmaxdecline40.csv', index_label='index')
ce4_v_probmaxdecline50.to_csv('ce4_v_probmaxdecline50.csv', index_label='index')


##################################################################
# generate frontier , gamma=8
##################################################################

ce8_v_probmaxdecline0 = efrontier(con, 'ce8', 'probmaxdecline0')
print ce8_v_probmaxdecline0
ce8_v_probmaxdecline10 = efrontier(con, 'ce8', 'probmaxdecline10')
ce8_v_probmaxdecline20 = efrontier(con, 'ce8', 'probmaxdecline20')
ce8_v_probmaxdecline30 = efrontier(con, 'ce8', 'probmaxdecline30')
ce8_v_probmaxdecline40 = efrontier(con, 'ce8', 'probmaxdecline40')
ce8_v_probmaxdecline50 = efrontier(con, 'ce8', 'probmaxdecline50')
# save to CSV
ce8_v_probmaxdecline0.to_csv('ce8_v_probmaxdecline0.csv', index_label='index')
ce8_v_probmaxdecline10.to_csv('ce8_v_probmaxdecline10.csv', index_label='index')
ce8_v_probmaxdecline20.to_csv('ce8_v_probmaxdecline20.csv', index_label='index')
ce8_v_probmaxdecline30.to_csv('ce8_v_probmaxdecline30.csv', index_label='index')
ce8_v_probmaxdecline40.to_csv('ce8_v_probmaxdecline40.csv', index_label='index')
ce8_v_probmaxdecline50.to_csv('ce8_v_probmaxdecline50.csv', index_label='index')
