#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$
    exit 1
}


trap terminate_scripts SIGINT


# run for convex mathod simulation


echo "s=11"
nohup R --slave --vanilla --args 500 250 20 52 11 10 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=12"
nohup R --slave --vanilla --args 500 250 20 52 12 10 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=13"
nohup R --slave --vanilla --args 500 250 20 52 13 10 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=14"
nohup R --slave --vanilla --args 500 250 20 52 14 10 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=15"
nohup R --slave --vanilla --args 500 250 20 52 15 10 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait


echo "s=16"
nohup R --slave --vanilla --args 500 250 20 52 16 10 100 10 < compare_to_baseline.R > test8.log 2>&1 &
echo "waiting..."
wait



echo "All scripts have been run successfully."

