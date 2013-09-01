import numpy as np
import pandas as pd
from pandas import *
import datetime
import pdb
import MySQLdb as mdb
import argparse
import subprocess

dbserver = 'localhost'
dbuser =  'safe'
dbpass =  'safe'
dbname = 'safewithdrawal'

############################################################
# returns data
############################################################

equitiesArray = [0.1162,0.3749,0.4361,-0.0842,-0.2490,-0.4334,-0.0819,0.5399,-0.0144,0.4767,0.3392,-0.3503,0.3112,-0.0041,-0.0978,-0.1159,0.2034,0.2590,0.1975,0.3644,-0.0807,0.0571,0.0550,0.1879,0.3171,0.2402,0.1837,-0.0099,0.5262,0.3156,0.0656,-0.1078,0.4336,0.1196,0.0047,0.2689,-0.0873,0.2280,0.1648,0.1245,-0.1006,0.2398,0.1106,-0.0850,0.0386,0.1430,0.1899,-0.1469,-0.2647,0.3723,0.2393,-0.0716,0.0657,0.1861,0.3250,-0.0492,0.2155,0.2256,0.0627,0.3173,0.1867,0.0525,0.1661,0.3169,-0.0310,0.3047,0.0762,0.1008,0.0132,0.3758,0.2296,0.3336,0.2858,0.2104,-0.0910,-0.1189,-0.2210,0.2868,0.1088,0.0491,0.1579,0.0549,-0.3700,0.2646,0.1506,0.0211,0.1582]

bondsArray = [0.0538,0.0452,0.0092,0.0601,0.0672,-0.0232,0.0881,0.0183,0.0900,0.0701,0.0306,0.0156,0.0623,0.0452,0.0296,0.0050,0.0194,0.0281,0.0180,0.0222,0.0100,0.0091,0.0185,0.0232,0.0070,0.0036,0.0163,0.0323,0.0268,-0.0065,-0.0042,0.0784,-0.0129,-0.0039,0.1176,0.0185,0.0556,0.0164,0.0404,0.0102,0.0469,0.0101,0.0454,-0.0074,0.1686,0.0872,0.0516,0.0461,0.0569,0.0783,0.1287,0.0141,0.0349,0.0409,0.0391,0.0945,0.2910,0.0741,0.1402,0.2033,0.1514,0.0290,0.0610,0.1329,0.0973,0.1546,0.0719,0.1124,-0.0514,0.1680,0.0210,0.0838,0.1021,-0.0177,0.1259,0.0762,0.1293,0.0240,0.0225,0.0136,0.0314,0.1005,0.1311,-0.0240,0.0712,0.0946,0.0267]

inflationArray = [-0.0149,-0.0208,-0.0097,0.0020,-0.0603,-0.0952,-0.1030,0.0051,0.0203,0.0299,0.0121,0.0310,-0.0278,-0.0048,0.0096,0.0972,0.0929,0.0316,0.0211,0.0225,0.1816,0.0901,0.0271,-0.0180,0.0579,0.0587,0.0088,0.0062,-0.0050,0.0037,0.0286,0.0302,0.0176,0.0150,0.0148,0.0067,0.0122,0.0165,0.0119,0.0192,0.0335,0.0304,0.0472,0.0611,0.0549,0.0336,0.0341,0.0880,0.1220,0.0701,0.0481,0.0677,0.0903,0.1331,0.1240,0.0894,0.0387,0.0380,0.0395,0.0377,0.0113,0.0441,0.0442,0.0465,0.0611,0.0306,0.0290,0.0275,0.0267,0.0254,0.0332,0.0170,0.0161,0.0268,0.0339,0.0155,0.0238,0.0188,0.0326,0.0342,0.0254,0.0408,0.0009,0.0272,0.0150,0.0296,0.0170]

def returnSeries(returns, startYear, endYear):

    try:
        assert endYear-startYear+1 == len(returns)
    except AssertionError:
        print "Invalid args: %d returns; %d to %d is %d years" % (len(returns), startYear, endYear, endYear-startYear+1)

    years = range(startYear, endYear+1)
    s = pd.Series(returns, index = years)
    return s

equitiesSeries = returnSeries(equitiesArray,1926,2012)
bondsSeries = returnSeries(bondsArray,1926,2012)
inflationSeries = returnSeries(inflationArray, 1926,2012)

realEq = equitiesSeries - inflationSeries
realBd = bondsSeries - inflationSeries

############################################################
# Male life table
############################################################

# survivors from 100000 births
MlivesArray = [100000,99262,99213,99182,99158,99138,99120,99104,99089,99075,99065,99056,99047,99032,99007,98968,98912,98841,98754,98654,98541,98414,98275,98128,97980,97834,97693,97555,97419,97284,97147,97009,96868,96724,96576,96423,96264,96096,95919,95729,95525,95303,95062,94800,94517,94209,93875,93513,93120,92691,92224,91716,91165,90572,89940,89270,88558,87800,86995,86138,85227,84254,83217,82111,80935,79684,78351,76929,75411,73792,72066,70223,68254,66161,63947,61612,59147,56545,53811,50951,47974,44882,41683,38401,35063,31699,28341,25024,21785,18670,15722,12986,10501,8297,6393,4794,3496,2479,1711,1149,754,481,298,179,104,58,31,16,8,4,2,1,0,0,0,0,0,0,0,0]
MlivesSeries = pd.Series(MlivesArray)

# life expectancy
MLEarray = [75.38,74.94,73.98,73.00,72.02,71.03,70.04,69.05,68.06,67.07,66.08,65.09,64.09,63.10,62.12,61.14,60.18,59.22,58.27,57.33,56.40,55.47,54.54,53.63,52.71,51.78,50.86,49.93,49.00,48.07,47.13,46.20,45.27,44.33,43.40,42.47,41.54,40.61,39.68,38.76,37.84,36.93,36.02,35.12,34.22,33.33,32.45,31.57,30.71,29.84,28.99,28.15,27.32,26.49,25.68,24.87,24.06,23.26,22.48,21.69,20.92,20.16,19.40,18.66,17.92,17.19,16.48,15.77,15.08,14.40,13.73,13.08,12.44,11.82,11.21,10.62,10.04,9.48,8.94,8.41,7.90,7.41,6.94,6.49,6.06,5.65,5.26,4.89,4.55,4.22,3.92,3.64,3.38,3.15,2.93,2.75,2.58,2.44,2.30,2.19,2.07,1.96,1.85,1.75,1.66,1.56,1.47,1.39,1.30,1.22,1.15,1.07,1.00,0.94,0.87,0.81,0.75,0.70,0.64,0.59]
MLEseries = pd.Series(MLEarray)

# death probability
MdeathrateArray = [0.007379,0.000494,0.000317,0.000241,0.000200,0.000179,0.000166,0.000152,0.000133,0.000108,0.000089,0.000094,0.000145,0.000252,0.000401,0.000563,0.000719,0.000873,0.001017,0.001148,0.001285,0.001412,0.001493,0.001513,0.001487,0.001446,0.001412,0.001389,0.001388,0.001405,0.001428,0.001453,0.001487,0.001529,0.001584,0.001651,0.001737,0.001845,0.001979,0.002140,0.002323,0.002526,0.002750,0.002993,0.003257,0.003543,0.003856,0.004208,0.004603,0.005037,0.005512,0.006008,0.006500,0.006977,0.007456,0.007975,0.008551,0.009174,0.009848,0.010584,0.011407,0.012315,0.013289,0.014326,0.015453,0.016723,0.018154,0.019732,0.021468,0.023387,0.025579,0.028032,0.030665,0.033467,0.036519,0.040010,0.043987,0.048359,0.053140,0.058434,0.064457,0.071259,0.078741,0.086923,0.095935,0.105937,0.117063,0.129407,0.143015,0.157889,0.174013,0.191354,0.209867,0.229502,0.250198,0.270750,0.290814,0.310029,0.328021,0.344422,0.361644,0.379726,0.398712,0.418648,0.439580,0.461559,0.484637,0.508869,0.534312,0.561028,0.589079,0.618533,0.649460,0.681933,0.716029,0.751831,0.789422,0.828894,0.870338,0.913855]
MdeathrateSeries = pd.Series(MdeathrateArray)

############################################################
# Female life table
############################################################

FlivesArray = [100000,99390,99347,99322,99303,99288,99275,99262,99250,99238,99228,99218,99208,99196,99180,99160,99133,99101,99064,99025,98983,98939,98894,98846,98796,98746,98694,98640,98584,98527,98466,98403,98336,98266,98190,98108,98020,97925,97822,97709,97586,97452,97305,97144,96968,96776,96566,96336,96087,95819,95530,95219,94885,94526,94143,93737,93304,92841,92342,91804,91220,90585,89895,89147,88340,87473,86537,85524,84427,83236,81944,80537,79008,77355,75580,73679,71638,69441,67090,64587,61930,59109,56112,52942,49608,46123,42504,38776,34973,31141,27333,23610,20038,16680,13593,10824,8415,6384,4726,3415,2411,1659,1111,722,454,275,160,90,48,24,11,5,2,1,0,0,0,0,0,0]
FlivesSeries = pd.Series(FlivesArray)

FLEarray = [80.43,79.92,78.95,77.97,76.99,76.00,75.01,74.02,73.03,72.04,71.04,70.05,69.06,68.07,67.08,66.09,65.11,64.13,63.15,62.18,61.20,60.23,59.26,58.29,57.32,56.35,55.38,54.40,53.44,52.47,51.50,50.53,49.56,48.60,47.64,46.68,45.72,44.76,43.81,42.86,41.91,40.97,40.03,39.10,38.17,37.24,36.32,35.41,34.50,33.59,32.69,31.80,30.91,30.02,29.14,28.27,27.40,26.53,25.67,24.82,23.97,23.14,22.31,21.49,20.69,19.89,19.10,18.32,17.55,16.79,16.05,15.32,14.61,13.91,13.22,12.55,11.90,11.26,10.63,10.03,9.43,8.86,8.31,7.77,7.26,6.77,6.31,5.87,5.45,5.06,4.69,4.36,4.04,3.76,3.50,3.26,3.05,2.87,2.70,2.54,2.39,2.25,2.11,1.98,1.86,1.74,1.62,1.52,1.41,1.31,1.22,1.13,1.05,0.97,0.89,0.82,0.75,0.70,0.64,0.59]
FLEseries = pd.Series(FLEarray)

FdeathrateArray =[0.006096,0.000434,0.000256,0.000192,0.000148,0.000136,0.000128,0.000122,0.000115,0.000106,0.000100,0.000102,0.000120,0.000157,0.000209,0.000267,0.000323,0.000369,0.000401,0.000422,0.000441,0.000463,0.000483,0.000499,0.000513,0.000528,0.000544,0.000563,0.000585,0.000612,0.000642,0.000678,0.000721,0.000771,0.000830,0.000896,0.000971,0.001056,0.001153,0.001260,0.001377,0.001506,0.001650,0.001810,0.001985,0.002174,0.002375,0.002582,0.002794,0.003012,0.003255,0.003517,0.003782,0.004045,0.004318,0.004619,0.004965,0.005366,0.005830,0.006358,0.006961,0.007624,0.008322,0.009046,0.009822,0.010698,0.011702,0.012832,0.014103,0.015526,0.017163,0.018987,0.020922,0.022951,0.025147,0.027709,0.030659,0.033861,0.037311,0.041132,0.045561,0.050698,0.056486,0.062971,0.070259,0.078471,0.087713,0.098064,0.109578,0.122283,0.136190,0.151300,0.167602,0.185078,0.203700,0.222541,0.241317,0.259716,0.277409,0.294054,0.311697,0.330399,0.350223,0.371236,0.393510,0.417121,0.442148,0.468677,0.496798,0.526605,0.558202,0.591694,0.627196,0.664827,0.704717,0.747000,0.789422,0.828894,0.870338,0.913855]
FdeathrateSeries = pd.Series(FdeathrateArray)

# create frame with LE, lives
def genLifetable(livesSeries, leSeries, deathrateSeries, ret_age, ret_length):
    end_age = ret_age+ret_length

    survival = livesSeries[ret_age:end_age]
    survival = survival/float(survival[ret_age])

    deathrate = survival-survival.shift(-1)
    deathrate.ix[end_age-1] = 1-np.sum(deathrate)

    lifetable = DataFrame(survival, columns=['survival'])
    LE = leSeries[ret_age:end_age]
    lifetable['LE'] = LE
    lifetable['deathrate']=deathrate
    return(lifetable)

def genReturns(realEq, realBd, equitypct, ret_year, ret_length):
    end_year = ret_year + ret_length-1
    eq = realEq.ix[ret_year:end_year]
    bd = realBd.ix[ret_year:end_year]
    returns = equitypct * eq + (1-equitypct) * bd
    return returns

#age LE survivePct year return

def genCohortCashFlows(scenarioFrame, sfactor, smoother, attenuator, sconstant):

    # scenarioframe has surviving % of cohort, life expectancy, port total return for each retirement year
    # populate values spending, other values using withdrawal strategy specified by params

    retframe = scenarioFrame.copy()
    retframe['spend']=0.0
    retframe['startportval']=0.0
    retframe['portval_ma']=0.0
    retframe['endportval']=0.0

    portval=100.0
    portval_MA=100.0
    last_MA=100.0

    for i, row in scenarioFrame.iterrows():
        survival, life_expectancy, deathrate, totret = row

        # compute portfolio moving average
        last_MA = portval_MA
        portval_MA = last_MA + (portval - last_MA)/smoother
        retframe.portval_ma[i] = portval_MA
        retframe.startportval[i] = portval

        # compute spending
        spendrate = sfactor / (life_expectancy + attenuator)
        spend = spendrate * portval_MA + sconstant
        spend = min(spend, portval) # don't spend below 0 (possible if portval_MA oversmoothed, high sconstant)
        spend = max(spend, 0.0) # don't spend below 0 (possible if negative sconstant)
        retframe.spend[i] = spend

        # compute updated portfolio val
        portval = portval - spend
        portval = portval * (1 + totret)
        retframe.endportval[i] = portval

    return retframe

def calcCashFlowMetrics(cashflows):

    #pdb.set_trace()
    initspend = cashflows.spend.irow(0)
    if initspend == 0:
        return (0.0, 100.0, 0.0, 0.0, 0.0, 0.0, 1.0)

    worst_shortfall = min(cashflows.spend/initspend) - 1
    worst_shortfall = worst_shortfall * -100
    lifespend = sum(cashflows.spend * cashflows.survival)

    ce1 = lifetime_ce(cashflows, 1.0)     #ce, gamma =1
    ce2 = lifetime_ce(cashflows, 2.0)     #ce, gamma =2
    ce4 = lifetime_ce(cashflows, 4.0)     #ce, gamma =4

    spend_std = lifetime_std(cashflows)

    return (initspend, worst_shortfall, lifespend, ce1, ce2, ce4, spend_std)

def lifetime_ce(cashflows, gamma):
    cashflows['temp'] = 0.0
    ret_year=cashflows.index[0]
    for i in range(len(cashflows)):
        cashflows.temp[ret_year+i]= crra_certainty_equivalent(cashflows.spend[0:i+1], gamma)
    cashflows.temp = cashflows.temp * cashflows.deathrate
    return sum(cashflows.temp)


def lifetime_std(cashflows):
    cashflows['temp'] = 0.0
    ret_year=cashflows.index[0]
    for i in range(len(cashflows)):
        cashflows.temp[ret_year+i]= np.std(cashflows.spend[0:i+1])
    cashflows.temp = cashflows.temp * cashflows.deathrate
    return sum(cashflows.temp)

def crra_certainty_equivalent(cashflowSeries, gamma):

    if gamma > 1 and 0 in cashflowSeries.values:
        return 0

    if gamma == 1.0:
        # general formula undefined for gamma = 1 but limit as gamma->1 = log
        u = np.mean(np.log(cashflowSeries))
        ce = np.exp(u)
    elif gamma == 2.0: # simple optimization
        u2 =  np.mean(1 - 1.0/(cashflowSeries))
        ce = 1.0 / (1.0 - u2)
    elif gamma == 4.0:
        gamma4temp = 1.0/3.0
        u4 = np.mean(gamma4temp - 1.0/(3.0 * cashflowSeries**3.0))
        ce = 1.0/(1.0 - 3.0 * u4) ** gamma4temp
    else: #general formula
        gammavar=1-gamma
        u = np.mean((cashflowSeries ** (gammavar) - 1.0)/(gammavar))
        ce = ((gammavar) * u +1.0) ** (1.0/(gammavar))

    return ce * len(cashflowSeries)

lt = genLifetable(MlivesSeries, MLEseries, MdeathrateSeries, 65, 30)
df = DataFrame(lt)

rt = genReturns(realEq, realBd, 0.4, 1926, 30)
rtdf = DataFrame(rt, columns=['totret'])
rtdf['age']=range(65,95)
rtdf = rtdf.set_index('age')
scenario = concat([df,rtdf], axis=1)

cashflows = genCohortCashFlows(scenario, 1, 1, 0, 0)

ret_age=65
ret_length=len(df)
deathrate = df['survival']
deathrate = deathrate-deathrate.shift(-1)
deathrate[ret_age+ret_length-1]=1-sum(deathrate[0:ret_length-1])

#print calcCashFlowMetrics(cashflows)

def analyzeCohort(lifetable, realEq, realBd,
                  equityPct, sfactor, smoother, attenuator, sconstant, debug=False):

    ret_age = lifetable.index[0]
    ret_length = len(lifetable)

    nCohorts = len(realEq) - ret_length + 1
    startYear = realEq.index[0]

    initspend_series = pd.Series(np.zeros(nCohorts))
    worst_shortfall_series = pd.Series(np.zeros(nCohorts))
    lifespend_series = pd.Series(np.zeros(nCohorts))
    ce1_series = pd.Series(np.zeros(nCohorts))
    ce2_series = pd.Series(np.zeros(nCohorts))
    ce4_series = pd.Series(np.zeros(nCohorts))
    spend_std_series = pd.Series(np.zeros(nCohorts))

    for i in range(nCohorts):
        eqSeries = realEq[i:ret_length+i]
        bdSeries = realBd[i:ret_length+i]
        retSeries = eqSeries * equityPct + bdSeries * (1-equityPct)
        retFrame = DataFrame(retSeries, columns=['totret'])
        retFrame['age']=range(ret_age,ret_age+ret_length)
        retFrame = retFrame.set_index('age')
        scenario = concat([lifetable,retFrame], axis=1)
        cashflows = genCohortCashFlows(scenario, sfactor, smoother, attenuator, sconstant)
        if debug:
            # save data for cohort
            cashflows["year"] = eqSeries.index
            cashflows["equities"] = eqSeries.values
            cashflows["bonds"] = bdSeries.values
            filename = "debug_cohort_%.0f_%.0f_%.0f_%.0f_%.0f_%.0d.csv" % (equityPct*100, sfactor*10, smoother*10, attenuator*10, sconstant*10, startYear + i)
            cashflows.to_csv(filename, index_label='index')
        initspend_series[i], worst_shortfall_series[i], lifespend_series[i], ce1_series[i], ce2_series[i], ce4_series[i], spend_std_series[i] = calcCashFlowMetrics(cashflows)


    if debug:
        # save frame
        yearSeries = Series(range(startYear, startYear+nCohorts))
        debugFrame = DataFrame({
                'year' : yearSeries,
                'initspend' : initspend_series,
                'worst_shortfall' : worst_shortfall_series,
                'lifespend' : lifespend_series,
                'ce1' : ce1_series,
                'ce2' : ce2_series,
                'ce4' : ce4_series,
                'spend_std' : spend_std_series,
                })
        filename = "debug_%.0f_%.0f_%.0f_%.0f_%.0f.csv" % (equityPct*100, sfactor*10, smoother*10, attenuator*10, sconstant*10)
        debugFrame.to_csv(filename, index_label='index')

    initspend = initspend_series[0]
    lifespend = np.mean(lifespend_series)

    worst_shortfall = np.max(worst_shortfall_series)
    shortfall_percentile90 = np.percentile(worst_shortfall_series.values, 90)
    shortfall_percentile80 = np.percentile(worst_shortfall_series.values, 80)
    shortfall_percentile70 = np.percentile(worst_shortfall_series.values, 70)
    shortfall_percentile60 = np.percentile(worst_shortfall_series.values, 60)
    shortfall_percentile50 = np.percentile(worst_shortfall_series.values, 50)

    # ce_adjust across the distribution of ce_adjusted timeseries
    ce1 = np.mean(crra_certainty_equivalent(ce1_series, 1.0)/len(ce1_series))
    ce2 = np.mean(crra_certainty_equivalent(ce1_series, 2.0)/len(ce1_series))
    ce4 = np.mean(crra_certainty_equivalent(ce1_series, 4.0)/len(ce1_series))

    spend_std = np.mean(spend_std_series)

    probmaxdecline0 = float(sum(worst_shortfall_series > 0)) / nCohorts
    probmaxdecline10 = float(sum(worst_shortfall_series >= 10)) / nCohorts
    probmaxdecline20 = float(sum(worst_shortfall_series >= 20)) / nCohorts
    probmaxdecline30 = float(sum(worst_shortfall_series >= 30)) / nCohorts
    probmaxdecline40 = float(sum(worst_shortfall_series >= 40)) / nCohorts
    probmaxdecline50 = float(sum(worst_shortfall_series >= 50)) / nCohorts

    return(initspend, worst_shortfall, lifespend, ce1, ce2, ce4,
           probmaxdecline0, probmaxdecline10, probmaxdecline20, probmaxdecline30, probmaxdecline40, probmaxdecline50, spend_std,
           shortfall_percentile90, shortfall_percentile80, shortfall_percentile70, shortfall_percentile60, shortfall_percentile50)


def populate_param_vectors():

    equity_lowerbound, equity_step, equity_iters = 0.0, 0.05, 21      # 0 to 1 step .05 -> 21
    range_equity = [equity_lowerbound + equity_i * equity_step for equity_i in range(equity_iters)]

    sfactor_lowerbound, sfactor_step, sfactor_iters = 0.1, 0.1, 2  # 0.1 to 2 step 0.1 -> 20
    range_sfactor = [sfactor_lowerbound + sfactor_i * sfactor_step for sfactor_i in range(sfactor_iters)]

    smoother_lowerbound, smoother_step, smoother_iters = 1.0, 1, 24 # 1 to 5 step 0.5, 6 to 12 step 1, 15 to 30 step 5 -> 20
    range_smoother = [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 15.0, 20.0, 25.0, 30.0]

    attenuator_lowerbound, attenuator_step, attenuator_iters = 0.0, 1.0, 21 # 0 to 10 step 1 , 12 to 20 step 2, 25 to 40 step 5 -> 25
    range_attenuator = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 25.0, 30.0, 35.0,40.0]

    sconstant_lowerbound, sconstant_step, sconstant_iters = 0, 0.25, 21 # 0 to 5 step 0.25 -> 21
    range_sconstant = [sconstant_lowerbound + sconstant_i * sconstant_step for sconstant_i in range(sconstant_iters)]

    print "range_equity", range_equity
    print "range_sfactor", range_sfactor
    print "range_smoother", range_smoother
    print "range_attenuator", range_attenuator
    print "range_sconstant", range_sconstant

    print len(range_equity) * len(range_sfactor) * len(range_smoother) * len(range_attenuator) * len(range_sconstant)

    id=0
    status=0
    ret_age = lt.index[0]
    ret_length = len(lt)
    ret_sex = 'M'

    con = db_connect()

    starttime = datetime.datetime.now()

    for sconstant in range_sconstant:
        for attenuator in range_attenuator:
            for smoother in range_smoother:
                for sfactor in range_sfactor:
                    # if sfactor_i==0, only test attenuator = 0, smoother =1
                    # those parameters have no effect so don't test all combos
                    if (sfactor==0.0 and (attenuator<>0.0 or smoother <>1.0)):
                        next
                    else:
                        for equityPct in range_equity:
                            id+=1
                            print "%.2f %.2f %.2f %.2f %.2f" % (equityPct, sfactor, smoother, attenuator, sconstant)

                            with con:
                                cur = con.cursor()
                                # should insert multiple rows at once, or use mysqlimport from text file
                                cur.execute("insert into param_metrics (id, status, ret_age, ret_length, ret_sex, param_equitypct, param_sfactor, param_smoother, param_attenuator, param_sconstant) values (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", (id, status, ret_age, ret_length, ret_sex, equityPct, sfactor, smoother, attenuator, sconstant))

def db_connect():
    try:
        con = mdb.connect(dbserver, dbuser, dbpass, dbname)
    except mdb.Error, e:
        print "Database connection error %d: %s" % (e.args[0], e.args[1])
        if con:
            con.close()
            return False
    return con

def db_get_rows(con, pid):

    print pid
    with con:
        try:
            cur = con.cursor()
            if pid:
                whereclause = "where status = -%s" %pid
            else:
                whereclause = "where status <=0"

            sqlstr ="""
select id, status, ret_age, ret_length, ret_sex,
  param_equitypct, param_sfactor, param_smoother, param_attenuator, param_sconstant
  from param_metrics
  %s
""" % whereclause
            print sqlstr
            cur.execute(sqlstr)
            rows = cur.fetchall()
        except mdb.Error, e:
            print "Database query error %d: %s" % (e.args[0], e.args[1])
            if con:
                con.close()
                return False

    return rows

def update_row(con, id, initspend, worst_shortfall, lifespend, ce1, ce2, ce4, prob0, prob10, prob20, prob30, prob40, prob50, spendstd, shortfall_percentile90, shortfall_percentile80, shortfall_percentile70, shortfall_percentile60, shortfall_percentile50):

    sqlstr ="""
update param_metrics
 set status = 1,
 initspend=%s,
 worst_shortfall=%s,
 lifespend=%s,
 ce1=%s,
 ce2=%s,
 ce4=%s,
 probmaxdecline0=%s,
 probmaxdecline10=%s,
 probmaxdecline20=%s,
 probmaxdecline30=%s,
 probmaxdecline40=%s,
 probmaxdecline50=%s,
 spend_std = %s,
 shortfall_percentile90 = %s,
 shortfall_percentile80 = %s,
 shortfall_percentile70 = %s,
 shortfall_percentile60 = %s,
 shortfall_percentile50 = %s
 where id = %s
"""

    with con:
        try:
            cur = con.cursor()
            cur.execute(sqlstr, (initspend, worst_shortfall, lifespend, ce1, ce2, ce4, prob0, prob10, prob20, prob30, prob40, prob50, spendstd, shortfall_percentile90, shortfall_percentile80, shortfall_percentile70, shortfall_percentile60, shortfall_percentile50, id))
        except mdb.Error, e:
            print "Database update error %d: %s" % (e.args[0], e.args[1])
            if con:
                con.close()
                return False
    return True

def analyze_param_vectors(pid, lt, realEq, realBd, name="main"):

    con = db_connect()
    if not con:
        print "%s: db_connect failed" % name
        return False

    rows = db_get_rows(con, pid)
    if not rows:
        print "%s: no rows returned"
        return 0

    c=0
    for row in rows:
        id, status, ret_age, ret_length, ret_sex, equitypct, sfactor, smoother, attenuator, sconstant = row
        print "%s %.2f %.2f %.2f %.2f %.2f" % (name, equitypct, sfactor, smoother, attenuator, sconstant)
        initspend, worst_shortfall, lifespend, ce1, ce2, ce4, prob0, prob10, prob20, prob30, prob40, prob50, spend_std, shortfall_percentile90, shortfall_percentile80, shortfall_percentile70, shortfall_percentile60, shortfall_percentile50 = analyzeCohort(lt, realEq, realBd, equitypct, sfactor, smoother, attenuator, sconstant)
        print "%s: %.2f %.3f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f" % (name, initspend, worst_shortfall, lifespend, ce1, ce2, ce4, prob0, prob10, prob20, prob30, prob40, prob50)
        update_row(con,id,initspend, worst_shortfall, lifespend, ce1, ce2, ce4, prob0, prob10, prob20, prob30, prob40, prob50, spend_std, shortfall_percentile90, shortfall_percentile80, shortfall_percentile70, shortfall_percentile60, shortfall_percentile50)
        c+=1

    con.close()
    return c


def assign_param_vectors(N):

    #pdb.set_trace()
    con = db_connect()
    if not con:
        print "%s: db_connect failed" % name
        return False

    with con:
        try:
            sqlstr = """
drop table if exists assign_temp;
"""
            cur = con.cursor()
            cur.execute(sqlstr)
            sqlstr = """
create table assign_temp as SELECT @i:=@i+1 AS c, id
FROM param_metrics ,(SELECT @i:=0) foo where param_metrics.status is null or param_metrics.status <> 1;
"""
            cur = con.cursor()
            cur.execute(sqlstr)

            sqlstr = "select count(*) from assign_temp;"
            cur = con.cursor()
            cur.execute(sqlstr)
            row = cur.fetchone()
            count = row[0]
            print count
        except mdb.Error, e:
            print "Database query error %d: %s" % (e.args[0], e.args[1])
            if con:
                con.close()
                return False

    breakpoint = count // N

    for pid in range(N):
        start = pid * breakpoint
        if pid == N-1:
            end = count
        else:
            end = ( pid +1) * breakpoint -1

        with con:
            try:
                # join to assign_temp
                sqlstr = "update assign_temp join param_metrics on assign_temp.id = param_metrics.id set status = -%d where c >=%d and c <= %d" % (pid+1, start, end)
                #print sqlstr
                cur.execute(sqlstr)

            except mdb.Error, e:
                print "Database update error %d: %s" % (e.args[0], e.args[1])
                if con:
                    con.close()
                    return False

    sqlstr = "drop table assign_temp"
    cur.execute(sqlstr)

    con.close()
    return count

if __name__ == '__main__':

############################################################
# parse command-line args
############################################################

    parser = argparse.ArgumentParser(description='analyze safe withdrawal scenarios')

    parser.add_argument('-p', '--populate', action='store_true', help='empty previous scenarios and populate new ones to be run',)
    parser.add_argument('-s', '--setup',    action='store_true', help='assign scenarios to pids',)
    parser.add_argument('-a', '--analyze',  action='store_true', help='analyze scenarios',)
    parser.add_argument('-m', '--multi',    action='store_true', help='spawn multiple processes to analyze scenarios',)
    parser.add_argument('-n', '--name',     action='store', help='tag this process with a name (for internal use) ',)
    parser.add_argument('-x', '--xprocesses', action='store', help='processes to run v database (default 4)',)
    parser.add_argument('-b', '--base',     action='store', help='base pid - analyze from this pid up (when running on multiple machines)',)

    args = parser.parse_args()
    if args.name == None:
        args.name = ""

    if args.xprocesses==None:
        args.xprocesses = 4

    did_something = False
    if args.populate:
        populate_param_vectors(int(args.populate))
        did_something = True

    if args.setup:
        assign_param_vectors(int(args.xprocesses))

    if args.multi:
        if args.base:
            base = int(args.base)
        else:
            base = 0

        for i in range(int(args.xprocesses)):
            name = "%d" % (base + i + 1)
            subprocess.Popen(["python", "SafeWithdrawal.py", "-a", "-n", name])
        did_something = True

    elif args.analyze: # mutually exclusive, multi takes precedence
        analyze_param_vectors(args.name, lt, realEq, realBd, name=args.name)
        did_something = True

    if not did_something:
        print "No command arguments found, use --help for a list"





