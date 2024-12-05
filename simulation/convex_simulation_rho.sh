#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for convex mathod simulation

# m = 400 n = 200

echo "rho=0.1"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 10 26 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait


echo "rho=0.3"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 30 26 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait

echo "rho=0.4"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 40 26 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait

echo "rho=0.5"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 50 26 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait

echo "rho=0.6"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 60 52 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait

echo "rho=0.7"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 70 52 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait

echo "rho=0.8"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 80 26 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait

echo "rho=0.9"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 90 26 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait

echo "rho=1.0"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 100 26 < compare_to_baseline_convex.R > test4.log 2>&1 &
echo "waiting..."
wait



wait

echo "All scripts have been run successfully."

