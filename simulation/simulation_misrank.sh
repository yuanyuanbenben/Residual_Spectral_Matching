#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for convex mathod simulation

echo "s=6"
nohup R --slave --vanilla --args 500 250 20 52 6 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "s=7"
nohup R --slave --vanilla --args 500 250 20 52 7 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "s=8"
nohup R --slave --vanilla --args 500 250 20 52 8 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "s=9"
nohup R --slave --vanilla --args 500 250 20 52 9 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "s=10"
nohup R --slave --vanilla --args 500 250 20 52 10 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "s=11"
nohup R --slave --vanilla --args 500 250 20 52 11 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "All scripts have been run successfully."

