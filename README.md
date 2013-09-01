safewithdrawal
==============

Code for safe withdrawal study - http://blog.streeteye.com/blog/2013/05/cat-food-revisited-final-thoughts-part-4/


requirements
============

Mysql, python with numpy and pandas, R for charts

step 1
======
create mysql database, user, grant permissions, e.g.

    create database swtest;
    create user 'safe'@'myhost' identified by 'safe';
    grant all privileges on swtest.* to 'safe'@'myhost';

step 2
======

populate param_metrics table with scenarios to evaluate

  python SWmysql.py 
  python SafeWithdrawal.py --populate

step 3
======

set up table for processing by setting number of processes, assigning each scenario to a process

  python SafeWithdrawal.py --setup --xprocesses 4

default is 4 for a single quad-core machine.

to run on 4 octo-core machines:

  python SafeWithdrawal.py --setup --xprocesses 32
  
step 4
======

launch processes to analyze scenarios

  python SafeWithdrawal.py --multi --xprocesses 4 --base 0

to run on 4 octo-core machines

          python SafeWithdrawal.py --multi --xprocesses 8 --base 0
          python SafeWithdrawal.py --multi --xprocesses 8 --base 8
          python SafeWithdrawal.py --multi --xprocesses 8 --base 16
          python SafeWithdrawal.py --multi --xprocesses 8 --base 24

for reference, the analysis in the article took < 1 week on 1 on-demand + 3 spot Amazon c1.xlarge instances.

step 5 
======

dump data to csv files

  python dump.py
  python dumpce.py

charts created using .r files reading csv files.




