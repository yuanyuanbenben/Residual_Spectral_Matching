#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


echo "run for tuning r=10"
 
nohup R --slave --vanilla --args 200 100 20 10 10 10 50 10 < tuning.R > test2.log 2>&1 &

nohup R --slave --vanilla --args 300 150 20 10 10 10 50 10 < tuning.R > test3.log 2>&1 &

nohup R --slave --vanilla --args 400 200 20 10 10 10 100 10 < tuning.R > test4.log 2>&1 &

nohup R --slave --vanilla --args 500 250 20 10 10 10 100 10 < tuning.R > test5.log 2>&1 &

nohup R --slave --vanilla --args 600 300 20 10 10 10 200 10 < tuning.R > test6.log 2>&1 &

nohup R --slave --vanilla --args 700 350 20 10 10 10 200 100 < tuning.R > test7.log 2>&1 &

nohup R --slave --vanilla --args 800 400 20 10 10 10 200 100 < tuning.R > test8.log 2>&1 &

wait
echo "run for tuning r=20"
 
nohup R --slave --vanilla --args 200 100 20 10 20 20 50 10 < tuning.R > test2.log 2>&1 &

nohup R --slave --vanilla --args 300 150 20 10 20 20 50 10 < tuning.R > test3.log 2>&1 &

nohup R --slave --vanilla --args 400 200 20 10 20 20 100 10 < tuning.R > test4.log 2>&1 &

nohup R --slave --vanilla --args 500 250 20 10 20 20 100 10 < tuning.R > test5.log 2>&1 &

nohup R --slave --vanilla --args 600 300 20 10 20 20 200 10 < tuning.R > test6.log 2>&1 &

nohup R --slave --vanilla --args 700 350 20 10 20 20 200 100 < tuning.R > test7.log 2>&1 &

nohup R --slave --vanilla --args 800 400 20 10 20 20 200 100 < tuning.R > test8.log 2>&1 &

wait
echo "run for tuning r=5"
 
nohup R --slave --vanilla --args 200 100 20 10 5 5 50 10 < tuning.R > test2.log 2>&1 &

nohup R --slave --vanilla --args 300 150 20 10 5 5 50 10 < tuning.R > test3.log 2>&1 &

nohup R --slave --vanilla --args 400 200 20 10 5 5 100 10 < tuning.R > test4.log 2>&1 &

nohup R --slave --vanilla --args 500 250 20 10 5 5 100 10 < tuning.R > test5.log 2>&1 &

nohup R --slave --vanilla --args 600 300 20 10 5 5 200 10 < tuning.R > test6.log 2>&1 &

nohup R --slave --vanilla --args 700 350 20 10 5 5 200 100 < tuning.R > test7.log 2>&1 &

nohup R --slave --vanilla --args 800 400 20 10 5 5 200 100 < tuning.R > test8.log 2>&1 &
wait
echo "All scripts have been run successfully."

