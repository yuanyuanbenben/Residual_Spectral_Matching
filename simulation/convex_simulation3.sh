#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for convex mathod simulation

# m = 200 n = 100
echo "m=200 n=100"
nohup R --slave --vanilla --args 200 100 20 52 20 70 70 < compare_to_baseline_convex.R > test.log 2>&1 &
echo "waiting..."
wait

# m = 300 n = 150
echo "m=300 n=150"
nohup R --slave --vanilla --args 300 150 20 52 20 70 70 < compare_to_baseline_convex.R > test.log 2>&1 &
  echo "waiting..."
wait

echo "m=400 n=200"
# m = 400 n = 200
nohup R --slave --vanilla --args 400 200 20 52 20 70 70 < compare_to_baseline_convex.R > test.log 2>&1 &
echo "waiting..."
wait

echo "m=500 n=250"
# m = 400 n = 200
nohup R --slave --vanilla --args 500 250 20 52 20 70 70 < compare_to_baseline_convex.R > test.log 2>&1 &
  echo "waiting..."
wait

echo "m=600 n=300"
# m = 600 n = 300
nohup R --slave --vanilla --args 600 300 20 52 20 70 70 < compare_to_baseline_convex.R > test.log 2>&1 &
echo "waiting..."
wait

echo "m=700 n=350"
# m = 600 n = 300
nohup R --slave --vanilla --args 700 350 20 52 20 70 70 < compare_to_baseline_convex.R > test.log 2>&1 &
  echo "waiting..."
wait

echo "m=800 n=400"
# m = 800 n = 400
nohup R --slave --vanilla --args 800 400 20 52 20 70 70 < compare_to_baseline_convex.R > test.log 2>&1 &
echo "waiting..."
wait


echo "All scripts have been run successfully."

