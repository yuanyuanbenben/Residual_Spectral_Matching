#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$
    exit 1
}


trap terminate_scripts SIGINT


# run for convex mathod simulation


echo "s=21"
nohup R --slave --vanilla --args 500 250 20 52 21 20 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=22"
nohup R --slave --vanilla --args 500 250 20 52 22 20 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=23"
nohup R --slave --vanilla --args 500 250 20 52 23 20 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=24"
nohup R --slave --vanilla --args 500 250 20 52 24 20 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=25"
nohup R --slave --vanilla --args 500 250 20 52 25 20 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=26"
nohup R --slave --vanilla --args 500 250 20 52 26 20 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait



echo "All scripts have been run successfully."

