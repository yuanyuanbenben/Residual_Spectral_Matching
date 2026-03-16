#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT

# Get the directory where the script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIMULATION_DIR="$(dirname "$SCRIPT_DIR")"

# Change to simulation directory
cd "$SIMULATION_DIR"

# run for convex mathod simulation
echo "sigma=0.0 r=5"
nohup R --slave --vanilla --args 0.0 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.01 r=5"
nohup R --slave --vanilla --args 0.01 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.02 r=5"
nohup R --slave --vanilla --args 0.02 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.05 r=5"
nohup R --slave --vanilla --args 0.05 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.1 r=5"
nohup R --slave --vanilla --args 0.1 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.2 r=5"
nohup R --slave --vanilla --args 0.2 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.5 r=5"
nohup R --slave --vanilla --args 0.5 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=1.0 r=5"
nohup R --slave --vanilla --args 1.0 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=2.0 r=5"
nohup R --slave --vanilla --args 2 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=5.0 r=5"
nohup R --slave --vanilla --args 5 5 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.0 r=10"
nohup R --slave --vanilla --args 0.0 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.01 r=10"
nohup R --slave --vanilla --args 0.01 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.02 r=10"
nohup R --slave --vanilla --args 0.02 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.05 r=10"
nohup R --slave --vanilla --args 0.05 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.1 r=10"
nohup R --slave --vanilla --args 0.1 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.2 r=10"
nohup R --slave --vanilla --args 0.2 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.5 r=10"
nohup R --slave --vanilla --args 0.5 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=1.0 r=10"
nohup R --slave --vanilla --args 1.0 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=2.0 r=10"
nohup R --slave --vanilla --args 2 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=5.0 r=10"
nohup R --slave --vanilla --args 5 10 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait


echo "sigma=0.0 r=20"
nohup R --slave --vanilla --args 0.0 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.01 r=20"
nohup R --slave --vanilla --args 0.01 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.02 r=20"
nohup R --slave --vanilla --args 0.02 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.05 r=20"
nohup R --slave --vanilla --args 0.05 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.1 r=20"
nohup R --slave --vanilla --args 0.1 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.2 r=20"
nohup R --slave --vanilla --args 0.2 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.5 r=20"
nohup R --slave --vanilla --args 0.5 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=1.0 r=20"
nohup R --slave --vanilla --args 1.0 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=2.0 r=20"
nohup R --slave --vanilla --args 2 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=5.0 r=20"
nohup R --slave --vanilla --args 5 20 < compare_different_noise.R > test2.log 2>&1 &
  echo "waiting..."
wait


echo "All scripts have been run successfully."

