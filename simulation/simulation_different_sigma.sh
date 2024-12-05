#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for convex mathod simulation
# 
echo "m=500 n=250"
nohup R --slave --vanilla --args 0.01 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 0.02 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 0.05 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 0.1 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 0.2 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 0.5 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 1.0 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 1.2 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 1.5 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 2 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 3 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 4 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 5 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 3 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 4 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 5 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 3 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 4 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "m=500 n=250"
nohup R --slave --vanilla --args 5 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "All scripts have been run successfully."

