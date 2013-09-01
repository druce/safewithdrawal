import datetime

import pdb
import argparse
import subprocess
#import timeit

import numpy as np
import pandas as pd
from pandas import *

from SWmysql import db_connect, db_get_rows, update_row, exec_sql
from SWreturns import realEq, realBd
from SWlife import MlivesSeries, MLEseries

# step 1 - create mysql database, user, grant permissions, e.g.
# create database swtest;
# create user 'safe'@'myhost' identified by 'safe';
# grant all privileges on swtest.* to 'safe'@'myhost';
# python SWmysql.py # create param_metrics table

# step 2 - create param_metrics table
# populate command
# load from text file

# step 3 - assign pids - enables running on multiple machines pointing to a single db
# step 4 - run analysis

# step 5 - dump data - charts run in R


def genLifetable(livesSeries, leSeries, ret_age, ret_length):
    """Create frame with life expectancies, lives"""

    end_age = ret_age + ret_length

    survival = livesSeries[ret_age:end_age]
    survival = survival / float(survival[ret_age])

    deathrate = survival - survival.shift(-1)
    deathrate.ix[end_age-1] = 1 - np.sum(deathrate)

    lifetable = DataFrame(survival, columns=['survival'])
    LE = leSeries[ret_age:end_age]
    lifetable['LE'] = LE
    lifetable['deathrate'] = deathrate
    return lifetable


def genReturns(realEq, realBd, equitypct, ret_year, ret_length):
    """Create ret_length year long return series for a portfolio with equitypct equities, 1-equitypct bonds
    starting ret_year"""

    end_year = ret_year + ret_length - 1
    eq = realEq.ix[ret_year:end_year]
    bd = realBd.ix[ret_year:end_year]
    returns = equitypct * eq + (1 - equitypct) * bd
    return returns


def genCohortCashFlows(scenarioFrame, sfactor, smoother, attenuator, sconstant):
    """generate cohort cash follows for a scenario frame using sfactor, smoother, attenuator, sconstant"""

    # scenarioframe has surviving % of cohort, life expectancy, port total return for each retirement year
    # populate values spending, other values using withdrawal strategy specified by params

    retframe = scenarioFrame.copy()
    retframe['spend'] = 0.0
    retframe['startportval'] = 0.0
    retframe['portval_ma'] = 0.0
    retframe['endportval'] = 0.0

    portval = 100.0
    portval_MA = 100.0

    for i, row in scenarioFrame.iterrows():
        survival, life_expectancy, deathrate, totret = row

        # compute portfolio moving average
        last_MA = portval_MA
        portval_MA = last_MA + (portval - last_MA) / smoother
        retframe.portval_ma[i] = portval_MA
        retframe.startportval[i] = portval

        # compute spending
        spendrate = sfactor / (life_expectancy + attenuator)
        spend = spendrate * portval_MA + sconstant
        spend = min(spend, portval)  # don't spend below 0 (possible if portval_MA oversmoothed, high sconstant)
        spend = max(spend, 0.0)  # don't spend below 0 (possible if negative sconstant)
        retframe.spend[i] = spend

        # compute updated portfolio val
        portval = portval - spend
        portval *= 1 + totret
        retframe.endportval[i] = portval

    return retframe


def calcCashFlowMetrics(cashflows, percentile_dict):

    #pdb.set_trace()
    initspend = cashflows.spend.irow(0)
    if initspend == 0:
        return 0.0, 100.0, 0.0, 0.0, 0.0, 0.0, 1.0

    worst_shortfall = min(cashflows.spend / initspend) - 1
    worst_shortfall *= -100
    lifespend = sum(cashflows.spend * cashflows.survival)

    ce1 = lifetime_ce(cashflows, 1.0)     # ce, gamma =1
    ce2 = lifetime_ce(cashflows, 2.0)     # ce, gamma =2
    ce4 = lifetime_ce(cashflows, 4.0)     # ce, gamma =4
    ce8 = lifetime_ce(cashflows, 8.0)     # ce, gamma =8
    ce16 = lifetime_ce(cashflows, 16.0)   # ce, gamma =8

    spend_std = lifetime_std(cashflows)  # not adjusted for frequency

    #pdb.set_trace()
    cashflows['temp'] = 0.0
    cond = cashflows.spend < (0.5 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob50 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.6 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob40 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.7 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob30 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.8 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob20 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.9 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob10 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < cashflows.spend.irow(0)
    cashflows.temp[cond] = cashflows.survival[cond]
    prob0 = max(cashflows.temp)

    # for each shortfall, store number experiencing in dict
    for i in range(len(cashflows)):
        shortfall = min(cashflows.spend[0:i + 1] / cashflows.spend.irow(0)) - 1
        if not(percentile_dict.get(shortfall)):
            percentile_dict[shortfall] = 0.0
        percentile_dict[shortfall] += cashflows.deathrate.irow(i)

    return initspend, worst_shortfall, lifespend, ce1, ce2, ce4, ce8, ce16, \
        spend_std, prob0, prob10, prob20, prob30, prob40, prob50, percentile_dict


def calcProbMetrics(cashflows):

    #pdb.set_trace()
    initspend = cashflows.spend.irow(0)
    if initspend == 0:
        return 0.0, 100.0, 0.0, 0.0, 0.0, 0.0, 1.0

    cashflows['temp'] = 0.0
    cond = cashflows.spend < (0.5 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob50 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.6 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob40 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.7 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob30 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.8 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob20 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < (0.9 * cashflows.spend.irow(0))
    cashflows.temp[cond] = cashflows.survival[cond]
    prob10 = max(cashflows.temp)

    #cashflows['temp']=0.0 # seems redundant
    cond = cashflows.spend < cashflows.spend.irow(0)
    cashflows.temp[cond] = cashflows.survival[cond]
    prob0 = max(cashflows.temp)

    return prob0, prob10, prob20, prob30, prob40, prob50


def lifetime_ce(cashflows, gamma):
    cashflows['temp'] = 0.0
    ret_year = cashflows.index[0]
    for i in range(len(cashflows)):
        cashflows.temp[ret_year + i] = crra_certainty_equivalent(cashflows.spend[0:i + 1], gamma)
    cashflows.temp = cashflows.temp * cashflows.deathrate
    return sum(cashflows.temp)


def lifetime_std(cashflows):
    cashflows['temp'] = 0.0
    ret_year = cashflows.index[0]
    for i in range(len(cashflows)):
        cashflows.temp[ret_year + i] = np.std(cashflows.spend[0:i + 1])
    cashflows.temp = cashflows.temp * cashflows.deathrate
    return sum(cashflows.temp)


def crra_certainty_equivalent(cashflowSeries, gamma):

    if gamma > 1 and 0 in cashflowSeries.values:
        return 0

    if gamma == 1.0:
        # general formula undefined for gamma = 1 but limit as gamma->1 = log
        u = np.mean(np.log(cashflowSeries))
        ce = np.exp(u)
    elif gamma == 2.0:  # simple optimization
        u2 = np.mean(1 - 1.0 / cashflowSeries)
        ce = 1.0 / (1.0 - u2)
    elif gamma > 1.0:
        g_minus1 = gamma - 1.0
        gamma4temp = 1.0 / g_minus1
        u4 = np.mean(gamma4temp - 1.0 / (g_minus1 * cashflowSeries ** g_minus1))
        ce = 1.0 / (1.0 - g_minus1 * u4) ** gamma4temp
    else:  # general formula
        g_1minus = 1 - gamma
        u = np.mean((cashflowSeries ** g_1minus - 1.0) / g_1minus)
        ce = (g_1minus * u + 1.0) ** (1.0 / g_1minus)

    return ce * len(cashflowSeries)

lt = genLifetable(MlivesSeries, MLEseries, 65, 30)
df = DataFrame(lt)

rt = genReturns(realEq, realBd, 0.4, 1926, 30)
rtdf = DataFrame(rt, columns=['totret'])
rtdf['age'] = range(65, 95)
rtdf = rtdf.set_index('age')
scenario = concat([df, rtdf], axis=1)

cashflows = genCohortCashFlows(scenario, 1, 1, 0, 0)

ret_age = 65
ret_length = len(df)
deathrate = df['survival']
deathrate = deathrate - deathrate.shift(-1)
deathrate[ret_age + ret_length - 1] = 1 - sum(deathrate[0:ret_length - 1])

#print calcCashFlowMetrics(cashflows)
#print calcProbMetrics(cashflows)
# print analyzeCohort(lt, realEq, realBd, 0.8, 0.3, 4.5, 0, 2)


def analyzeCohort(lifetable, realEq, realBd,
                  equityPct, sfactor, smoother, attenuator, sconstant, debug=False):

    ret_age = lifetable.index[0]
    ret_length = len(lifetable)
    nCohorts = len(realEq) - ret_length + 1
    startYear = realEq.index[0]
    returnSeries = realEq * equityPct + realBd * (1 - equityPct)

    initspend_series = pd.Series(np.zeros(nCohorts))
    worst_shortfall_series = pd.Series(np.zeros(nCohorts))
    lifespend_series = pd.Series(np.zeros(nCohorts))
    ce1_series = pd.Series(np.zeros(nCohorts))
    ce2_series = pd.Series(np.zeros(nCohorts))
    ce4_series = pd.Series(np.zeros(nCohorts))
    ce8_series = pd.Series(np.zeros(nCohorts))
    ce16_series = pd.Series(np.zeros(nCohorts))

    spend_std_series = pd.Series(np.zeros(nCohorts))
    prob0 = pd.Series(np.zeros(nCohorts))
    prob10 = pd.Series(np.zeros(nCohorts))
    prob20 = pd.Series(np.zeros(nCohorts))
    prob30 = pd.Series(np.zeros(nCohorts))
    prob40 = pd.Series(np.zeros(nCohorts))
    prob50 = pd.Series(np.zeros(nCohorts))

    percentile_dict = {}

    for i in range(nCohorts):
        eqSeries = realEq[i:ret_length + i]
        bdSeries = realBd[i:ret_length + i]
        retSeries = returnSeries[i:ret_length + i]
        retFrame = DataFrame(retSeries, columns=['totret'])
        retFrame['age'] = range(ret_age, ret_age + ret_length)
        retFrame = retFrame.set_index('age')
        scenario = concat([lifetable, retFrame], axis=1)
        cashflows = genCohortCashFlows(scenario, sfactor, smoother, attenuator, sconstant)
        if debug:
            # save data for cohort
            cashflows["year"] = eqSeries.index
            cashflows["equities"] = eqSeries.values
            cashflows["bonds"] = bdSeries.values
            filename = "debug_cohort_%.0f_%.0f_%.0f_%.0f_%.0f_%.0d.csv" % (equityPct * 100, sfactor * 10, smoother * 10,
                                                                           attenuator * 10, sconstant * 10,
                                                                           startYear + i)
            cashflows.to_csv(filename, index_label='index')

        initspend_series[i], worst_shortfall_series[i], lifespend_series[i], \
            ce1_series[i], ce2_series[i], ce4_series[i], ce8_series[i], ce16_series[i], \
            spend_std_series[i], prob0[i], prob10[i], prob20[i], prob30[i], prob40[i], prob50[i], \
            percentile_dict = calcCashFlowMetrics(cashflows, percentile_dict)

        pdb.set_trace()

    if debug:
        # save frame
        yearSeries = Series(range(startYear, startYear + nCohorts))
        debugFrame = DataFrame({
            'year': yearSeries,
            'initspend': initspend_series,
            'worst_shortfall': worst_shortfall_series,
            'lifespend': lifespend_series,
            'ce1': ce1_series,
            'ce2': ce2_series,
            'ce4': ce4_series,
            'ce8': ce8_series,
            'ce16': ce16_series,
            'spend_std': spend_std_series,
        })
        filename = "debug_%.0f_%.0f_%.0f_%.0f_%.0f.csv" % (equityPct * 100, sfactor * 10, smoother * 10,
                                                           attenuator * 10, sconstant * 10)
        debugFrame.to_csv(filename, index_label='index')

    initspend = initspend_series[0]
    lifespend = np.mean(lifespend_series)

    worst_shortfall = np.max(worst_shortfall_series)

    #pdb.set_trace()
    ks = []
    vs = []
    for z in sorted(percentile_dict.keys()):
        ks.append(z)
        vs.append(percentile_dict[z])

    sfdf = DataFrame({'shortfall': ks, 'population': vs})
    sfdf.population = sfdf.population / sum(sfdf.population)

    shortfall_percentile90 = safepercentile(sfdf, 0.90) * -100
    shortfall_percentile80 = safepercentile(sfdf, 0.80) * -100
    shortfall_percentile70 = safepercentile(sfdf, 0.70) * -100
    shortfall_percentile60 = safepercentile(sfdf, 0.60) * -100
    shortfall_percentile50 = safepercentile(sfdf, 0.50) * -100

    # ce_adjust across the distribution of ce_adjusted timeseries
    ce1 = np.mean(crra_certainty_equivalent(ce1_series, 1.0) / len(ce1_series))
    ce2 = np.mean(crra_certainty_equivalent(ce1_series, 2.0) / len(ce1_series))
    ce4 = np.mean(crra_certainty_equivalent(ce1_series, 4.0) / len(ce1_series))
    ce8 = np.mean(crra_certainty_equivalent(ce1_series, 8.0) / len(ce1_series))
    ce16 = np.mean(crra_certainty_equivalent(ce1_series, 16.0) / len(ce1_series))

    spend_std = np.mean(spend_std_series)  # not adjusted for frequency

    # average prob0,prob10,...
    probmaxdecline0 = np.mean(prob0)
    probmaxdecline10 = np.mean(prob10)
    probmaxdecline20 = np.mean(prob20)
    probmaxdecline30 = np.mean(prob30)
    probmaxdecline40 = np.mean(prob40)
    probmaxdecline50 = np.mean(prob50)

    return initspend, worst_shortfall, lifespend, ce1, ce2, ce4, ce8, ce16, probmaxdecline0, probmaxdecline10, \
        probmaxdecline20, probmaxdecline30, probmaxdecline40, probmaxdecline50, spend_std, shortfall_percentile90, \
        shortfall_percentile80, shortfall_percentile70, shortfall_percentile60, shortfall_percentile50


def safepercentile(percentile_frame, percentile):

    # should exception if percentile not between 0 and 1

    #pdb.set_trace()
    percentile = 1 - percentile  # going from bottom

    percentile_frame['cumulative'] = percentile_frame.population.cumsum()
    tempframe = percentile_frame[percentile_frame.cumulative > percentile]

    if len(tempframe) > 0:
        return tempframe.shortfall.irow(0)
    else:
        #no rows, return none
        return None


def prob_analyzeCohort(lifetable, realEq, realBd, equityPct, sfactor, smoother, attenuator, sconstant, debug=False):

    ret_age = lifetable.index[0]
    ret_length = len(lifetable)

    nCohorts = len(realEq) - ret_length + 1
    startYear = realEq.index[0]

    prob0 = pd.Series(np.zeros(nCohorts))
    prob10 = pd.Series(np.zeros(nCohorts))
    prob20 = pd.Series(np.zeros(nCohorts))
    prob30 = pd.Series(np.zeros(nCohorts))
    prob40 = pd.Series(np.zeros(nCohorts))
    prob50 = pd.Series(np.zeros(nCohorts))

    for i in range(nCohorts):
        eqSeries = realEq[i:ret_length + i]
        bdSeries = realBd[i:ret_length + i]
        retSeries = eqSeries * equityPct + bdSeries * (1 - equityPct)
        retFrame = DataFrame(retSeries, columns=['totret'])
        retFrame['age'] = range(ret_age, ret_age + ret_length)
        retFrame = retFrame.set_index('age')
        scenario = concat([lifetable, retFrame], axis=1)
        cashflows = genCohortCashFlows(scenario, sfactor, smoother, attenuator, sconstant)
        if debug:
            # save data for cohort
            cashflows["year"] = eqSeries.index
            cashflows["equities"] = eqSeries.values
            cashflows["bonds"] = bdSeries.values
            filename = "debug_cohort_%.0f_%.0f_%.0f_%.0f_%.0f_%.0d.csv" % \
                       (equityPct * 100, sfactor * 10, smoother * 10, attenuator * 10, sconstant * 10, startYear + i)
            cashflows.to_csv(filename, index_label='index')

        prob0[i], prob10[i], prob20[i], prob30[i], prob40[i], prob50[i] = calcProbMetrics(cashflows)

    # average prob0,prob10,...
    probmaxdecline0 = np.mean(prob0)
    probmaxdecline10 = np.mean(prob10)
    probmaxdecline20 = np.mean(prob20)
    probmaxdecline30 = np.mean(prob30)
    probmaxdecline40 = np.mean(prob40)
    probmaxdecline50 = np.mean(prob50)

    return probmaxdecline0, probmaxdecline10, probmaxdecline20, probmaxdecline30, probmaxdecline40, probmaxdecline50


def populate_param_vectors():
    """Generate a file which can be loaded into param_metrics table"""
    equity_lowerbound, equity_step, equity_iters = 0.0, 0.05, 21      # 0 to 1 step .05 -> 21
    range_equity = [equity_lowerbound + equity_i * equity_step for equity_i in range(equity_iters)]

    sfactor_lowerbound, sfactor_step, sfactor_iters = 0.0, 0.1, 21  # 0.1 to 2 step 0.1 -> 20
    range_sfactor = [sfactor_lowerbound + sfactor_i * sfactor_step for sfactor_i in range(sfactor_iters)]

    # 1 to 5 step 0.5, 6 to 12 step 1, 15 to 30 step 5 -> 20
    # smoother_lowerbound, smoother_step, smoother_iters = 1.0, 1, 24
    range_smoother = [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 15.0, 20.0,
                      25.0, 30.0]

    # 0 to 10 step 1 , 12 to 20 step 2, 25 to 40 step 5 -> 25
    # attenuator_lowerbound, attenuator_step, attenuator_iters = 0.0, 1.0, 21
    range_attenuator = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 25.0,
                        30.0, 35.0, 40.0]

    sconstant_lowerbound, sconstant_step, sconstant_iters = 0, 0.25, 21  # 0 to 5 step 0.25 -> 21
    range_sconstant = [sconstant_lowerbound + sconstant_i * sconstant_step for sconstant_i in range(sconstant_iters)]

    print "range_equity", range_equity
    print "range_sfactor", range_sfactor
    print "range_smoother", range_smoother
    print "range_attenuator", range_attenuator
    print "range_sconstant", range_sconstant

    print len(range_equity) * len(range_sfactor) * len(range_smoother) * len(range_attenuator) * len(range_sconstant)

    param_metrics_id = 0
    status = 0
    ret_age = lt.index[0]
    ret_length = len(lt)
    ret_sex = 'M'

    #con = db_connect()

    starttime = datetime.now()

    with open('param_metrics.txt', "w") as f:
        for sconstant in range_sconstant:
            for attenuator in range_attenuator:
                for smoother in range_smoother:
                    for sfactor in range_sfactor:
                        # if sfactor==0, only test attenuator = 0, smoother =1
                        # those parameters have no effect so don't test all combos
                        if sfactor == 0.0 and (attenuator != 0.0 or smoother != 1.0):
                            pass
                        # if sfactor==0, and sconstant==0, spend =0, don't test
                        if sfactor == 0.0 and sconstant == 0.0:
                            pass
                        else:
                            for equityPct in range_equity:
                                param_metrics_id += 1
                                f.write('%s, %s, %s, %s, %s, %s, %s, %s, %s, %s\n' % (param_metrics_id, status, ret_age,
                                                                                      ret_length, ret_sex, equityPct,
                                                                                      sfactor, smoother, attenuator,
                                                                                      sconstant))
                                if (param_metrics_id % 1000) == 0:
                                    nowtime = datetime.now()
                                    print "%s: %d" % (str(nowtime), param_metrics_id)

    #load data local infile '/home/ubuntu/safewithdrawal/param_metrics.txt' into table param_metrics fields terminated by ',' lines terminated by '\n' (id, status, ret_age, ret_length, ret_sex, param_equitypct, param_sfactor, param_smoother, param_attenuator, param_sconstant);


def analyze_param_vectors(pid, lt, realEq, realBd, name="main"):

    con = db_connect()
    if not con:
        print "%s: db_connect failed" % name
        return False

    rows = db_get_rows(con, pid)
    if not rows:
        print "%s: no rows returned"
        return 0

    c = 0
    for row in rows:
        pdb.set_trace()
        param_metrics_id, status, ret_age, ret_length, ret_sex, equitypct, sfactor, smoother, attenuator, \
            sconstant = row
        #print "%s %.2f %.2f %.2f %.2f %.2f" % (name, equitypct, sfactor, smoother, attenuator, sconstant)
        initspend, worst_shortfall, lifespend, ce1, ce2, ce4, ce6, ce8, prob0, prob10, prob20, prob30, prob40, prob50, spend_std, shortfall_percentile90, shortfall_percentile80, shortfall_percentile70, shortfall_percentile60, shortfall_percentile50 = analyzeCohort(lt, realEq, realBd, equitypct, sfactor, smoother, attenuator, sconstant)
        #print "%s: %.2f %.3f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f" % (name, initspend, worst_shortfall, lifespend, ce1, ce2, ce4, prob0, prob10, prob20, prob30, prob40, prob50)
        update_row(con, param_metrics_id, initspend, worst_shortfall, lifespend, ce1, ce2, ce4, ce6, ce8,
                   prob0, prob10, prob20, prob30, prob40, prob50, spend_std,
                   shortfall_percentile90, shortfall_percentile80, shortfall_percentile70,
                   shortfall_percentile60, shortfall_percentile50)
        #prob_update_row(con,id,prob0, prob10, prob20, prob30, prob40, prob50)
        c+=1
        if c % 1000 == 0:
            print "%s: %d" % (str(datetime.datetime.now()), c)

    con.close()
    return c


def assign_param_vectors(N):
    """Take any unprocessed parameter vectors, assign them to be processed by N processes."""
    # eg, set pids they should be processed by
    #pdb.set_trace()
    con = db_connect()
    if not con:
        print "%s: db_connect failed" % name
        return False

    sqlstr = """
drop table if exists assign_temp;
"""
    exec_sql(con, sqlstr)

    sqlstr = """
create table assign_temp as SELECT @i:=@i+1 AS c, id
FROM param_metrics ,(SELECT @i:=0) foo where param_metrics.status is null or param_metrics.status <> 1;
"""
    exec_sql(con, sqlstr)

    sqlstr = "select count(*) from assign_temp;"
    cur = exec_sql(con, sqlstr)
    row = cur.fetchone()
    count = row[0]
    print count

    breakpoint = count // N

    for pid in range(N):
        start = pid * breakpoint
        if pid == N - 1:
            end = count
        else:
            end = (pid + 1) * breakpoint - 1

        # join to assign_temp
        sqlstr = """
        update assign_temp
        join param_metrics on assign_temp.id = param_metrics.id
        set status = -%d where c >=%d and c <= %d" % (pid+1, start, end)
        """
        exec_sql(con, sqlstr)

    sqlstr = "drop table if exists assign_temp"
    exec_sql(con, sqlstr)

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
    if args.name is None:
        args.name = ""

    if args.xprocesses is None:
        args.xprocesses = 4

    did_something = False
    if args.populate:
        # create scenario file
        populate_param_vectors()

        # load file into db (faster than individual SQL statements)
        print "loading with SQL statement:"
        sqlstr = """
        delete from param_metrics;
        load data infile 'param_metrics.txt'
        into table param_metrics
        fields terminated by ','
        lines terminated by '\r\n'
        (id, status, ret_age, ret_length, ret_sex, param_equitypct, param_sfactor, param_smoother, param_attenuator,
        param_sconstant);
        """
        print "(This will take several minutes)"

        print sqlstr
        con = db_connect()
        if not con:
            print "db_connect failed"
        else:
            exec_sql(con, sqlstr)
        did_something = True

    if args.setup:
        #pdb.set_trace()
        assign_param_vectors(int(args.xprocesses))
        did_something = True

    if args.multi:
        if args.base:
            base = int(args.base)
        else:
            base = 0

        for i in range(int(args.xprocesses)):
            name = "%d" % (base + i + 1)
            subprocess.Popen(["python", "SafeWithdrawal.py", "-a", "-n", name])
        did_something = True

    elif args.analyze:  # mutually exclusive, multi takes precedence
        analyze_param_vectors(args.name, lt, realEq, realBd, name=args.name)
        did_something = True

    if not did_something:
        print "No command arguments found, use --help for a list"
