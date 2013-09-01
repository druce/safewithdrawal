import MySQLdb as mdb

############################################################
# MySQL abstraction functions
############################################################

# create database swtest;
# create user 'safe'@'myhost' identified by 'safe';
# grant all privileges on swtest.* to 'safe'@'myhost';
# python SWmysql.py to create table using create_table function

dbserver = 'localhost'
dbuser = 'safe'
dbpass = 'safe'
dbname = 'swtest'


def db_connect():
    """connect to MySQL database, return connection"""
    con = None
    try:
        con = mdb.connect(dbserver, dbuser, dbpass, dbname)
        return con
    except mdb.Error, e:
        print "Database connection error %d: %s" % (e.args[0], e.args[1])
        if con:
            con.close()
        return None


def exec_sql(con, sql_str, vals=()):
    """use connection con to execute sql_str, with parameters vals"""
    try:
        cursor = con.cursor()
        cursor.execute(sql_str, vals)
        return cursor
    except mdb.Error, e:
        print "Database error %d: %s" % (e.args[0], e.args[1])
        if con:
            con.close()
        return None


def create_table(con):
    """create param_metrics table"""
    sqlstr = "drop table if exists param_metrics;"
    exec_sql(con, sqlstr)

    sqlstr = """
CREATE TABLE `param_metrics` (
  `id` int(11) NOT NULL,
  `status` int(11) NOT NULL DEFAULT 0,
  `ret_age` int(11) NOT NULL,
  `ret_length` int(11) NOT NULL,
  `ret_sex` varchar(2) NOT NULL,
  `param_equitypct` double NOT NULL,
  `param_sfactor` double NOT NULL,
  `param_smoother` double NOT NULL,
  `param_attenuator` double NOT NULL,
  `param_sconstant` double NOT NULL,
  `initspend` double DEFAULT NULL,
  `worst_shortfall` double DEFAULT NULL,
  `lifespend` double DEFAULT NULL,
  `ce1` double DEFAULT NULL,
  `ce2` double DEFAULT NULL,
  `ce4` double DEFAULT NULL,
  `ce8` double DEFAULT NULL,
  `ce16` double DEFAULT NULL,
  `probmaxdecline0` double DEFAULT NULL,
  `probmaxdecline10` double DEFAULT NULL,
  `probmaxdecline20` double DEFAULT NULL,
  `probmaxdecline30` double DEFAULT NULL,
  `probmaxdecline40` double DEFAULT NULL,
  `probmaxdecline50` double DEFAULT NULL,
  `spend_std` double DEFAULT NULL,
  `shortfall_percentile90` double DEFAULT NULL,
  `shortfall_percentile80` double DEFAULT NULL,
  `shortfall_percentile70` double DEFAULT NULL,
  `shortfall_percentile60` double DEFAULT NULL,
  `shortfall_percentile50` double DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `ix_unique` (`param_equitypct`,`param_sfactor`,`param_smoother`,`param_attenuator`,`param_sconstant`,`ret_age`,`ret_length`,`ret_sex`),
  KEY `ix_param_equitypct` (`param_equitypct`),
  KEY `ix_param_sfactor` (`param_sfactor`),
  KEY `ix_param_smoother` (`param_smoother`),
  KEY `ix_param_attenuator` (`param_attenuator`),
  KEY `ix_param_sconstant` (`param_sconstant`),
  KEY `ix_initspend` (`initspend`),
  KEY `ix_worstshortfall` (`worst_shortfall`),
  KEY `ix_lifespend` (`lifespend`),
  KEY `ix_ce1` (`ce1`),
  KEY `ix_ce2` (`ce2`),
  KEY `ix_ce4` (`ce4`),
  KEY `ix_ce8` (`ce8`),
  KEY `ix_ce16` (`ce16`),
  KEY `ix_prob0` (`probmaxdecline0`),
  KEY `ix_prob10` (`probmaxdecline10`),
  KEY `ix_prob20` (`probmaxdecline20`),
  KEY `ix_prob30` (`probmaxdecline30`),
  KEY `ix_prob40` (`probmaxdecline40`),
  KEY `ix_prob50` (`probmaxdecline50`),
  KEY `ix_spendstd` (`spend_std`),
  KEY `ix_percentile90` (`shortfall_percentile90`),
  KEY `ix_percentile80` (`shortfall_percentile80`),
  KEY `ix_percentile70` (`shortfall_percentile70`),
  KEY `ix_percentile60` (`shortfall_percentile60`),
  KEY `ix_percentile50` (`shortfall_percentile50`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1
"""
    exec_sql(con, sqlstr)


def db_get_rows(con, pid):
    """Get an array of rows that process identified by pid still needs to calculate"""
    print pid
    rows = []
    if pid:
        whereclause = "where status = -%s" % pid
    else:
        whereclause = "where status <=0"

    sqlstr = """
select id, status, ret_age, ret_length, ret_sex,
  param_equitypct, param_sfactor, param_smoother, param_attenuator, param_sconstant
  from param_metrics
  %s
""" % whereclause

    print sqlstr
    cur = exec_sql(con, sqlstr)
    rows = cur.fetchall()

    return rows


def update_row(con, param_metrics_id, initspend, worst_shortfall, lifespend, ce1, ce2, ce4, ce8, ce16,
               prob0, prob10, prob20, prob30, prob40, prob50, spendstd,
               shortfall_percentile90, shortfall_percentile80, shortfall_percentile70, shortfall_percentile60,
               shortfall_percentile50):
    """update a row with calculated values"""
    sqlstr = """
update param_metrics
 set status = 1,
 initspend=%s,
 worst_shortfall=%s,
 lifespend=%s,
 ce1=%s,
 ce2=%s,
 ce4=%s,
 ce8=%s,
 ce16=%s,
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
    return exec_sql(con, sqlstr, (initspend, worst_shortfall, lifespend, ce1, ce2, ce4, ce8, ce16,
                                  prob0, prob10, prob20, prob30, prob40, prob50, spendstd,
                                  shortfall_percentile90, shortfall_percentile80, shortfall_percentile70,
                                  shortfall_percentile60, shortfall_percentile50, param_metrics_id))


def prob_update_row(con, param_metrics_id, prob0, prob10, prob20, prob30, prob40, prob50):
    """update a row with calculated values for probabilities only"""
    sqlstr = """
update param_metrics
 set status = 1,
 probmaxdecline0=%s,
 probmaxdecline10=%s,
 probmaxdecline20=%s,
 probmaxdecline30=%s,
 probmaxdecline40=%s,
 probmaxdecline50=%s
 where id = %s
"""
    return exec_sql(con, sqlstr)

if __name__ == "__main__":
    con = db_connect()
    create_table(con)
