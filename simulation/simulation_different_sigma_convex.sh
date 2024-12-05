#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for convex mathod simulation
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 0.01 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 0.02 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 0.05 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 0.1 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 0.2 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 0.5 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 1.0 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 1.2 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait
# 
# echo "m=500 n=250"
# nohup R --slave --vanilla --args 1.5 < compare_different_noise_convex.R > test2.log 2>&1 &
#   echo "waiting..."
# wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 2 < compare_different_noise_convex.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "All scripts have been run successfully."

