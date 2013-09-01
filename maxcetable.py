import numpy as np
import pandas as pd
from pandas import *
import datetime
import pdb
import MySQLdb as mdb
import argparse

from dump import *

sqlstr = """
select * from param_metrics where id in (
select id from param_metrics where ce8 = (select max(ce8) from param_metrics)
union
select id from param_metrics where ce4 = (select max(ce4) from param_metrics)
union
select id from param_metrics where ce2 = (select max(ce2) from param_metrics)
union
select id from param_metrics where ce1 = (select max(ce1) from param_metrics)
union
select id from param_metrics where lifespend = (select max(lifespend) from param_metrics)
)
"""

con = db_connect()

rows = do_query(con, sqlstr)

print rows

#create dataframe
#save as csv

# top 12
select ce4, worst_shortfall, param_equitypct, param_sconstant, param_sfactor, param_smoother, param_attenuator, initspend, lifespend, probmaxdecline0 from param_metrics where worst_shortfall < 10 order by ce4 desc limit 12;

