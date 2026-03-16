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
echo "sigma=0.0 r=5 mode=1"
nohup R --slave --vanilla --args 0.0 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.01 r=5 mode=1"
nohup R --slave --vanilla --args 0.01 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.02 r=5 mode=1"
nohup R --slave --vanilla --args 0.02 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.05 r=5 mode=1"
nohup R --slave --vanilla --args 0.05 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.1 r=5 mode=1"
nohup R --slave --vanilla --args 0.1 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.2 r=5 mode=1"
nohup R --slave --vanilla --args 0.2 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.5 r=5 mode=1"
nohup R --slave --vanilla --args 0.5 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=1.0 r=5 mode=1"
nohup R --slave --vanilla --args 1.0 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=2.0 r=5 mode=1"
nohup R --slave --vanilla --args 2 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=5.0 r=5 mode=1"
nohup R --slave --vanilla --args 5 5 1 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait


echo "sigma=0.0 r=5 mode=2"
nohup R --slave --vanilla --args 0.0 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.01 r=5 mode=2"
nohup R --slave --vanilla --args 0.01 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.02 r=5 mode=2"
nohup R --slave --vanilla --args 0.02 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.05 r=5 mode=2"
nohup R --slave --vanilla --args 0.05 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.1 r=5 mode=2"
nohup R --slave --vanilla --args 0.1 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.2 r=5 mode=2"
nohup R --slave --vanilla --args 0.2 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.5 r=5 mode=2"
nohup R --slave --vanilla --args 0.5 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=1.0 r=5 mode=2"
nohup R --slave --vanilla --args 1.0 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=2.0 r=5 mode=2"
nohup R --slave --vanilla --args 2 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=5.0 r=5 mode=2"
nohup R --slave --vanilla --args 5 5 2 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.0 r=5 mode=3"
nohup R --slave --vanilla --args 0.0 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.01 r=5 mode=3"
nohup R --slave --vanilla --args 0.01 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.02 r=5 mode=3"
nohup R --slave --vanilla --args 0.02 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.05 r=5 mode=3"
nohup R --slave --vanilla --args 0.05 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.1 r=5 mode=3"
nohup R --slave --vanilla --args 0.1 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.2 r=5 mode=3"
nohup R --slave --vanilla --args 0.2 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.5 r=5 mode=3"
nohup R --slave --vanilla --args 0.5 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=1.0 r=5 mode=3"
nohup R --slave --vanilla --args 1.0 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=2.0 r=5 mode=3"
nohup R --slave --vanilla --args 2 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=5.0 r=5 mode=3"
nohup R --slave --vanilla --args 5 5 3 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.0 r=5 mode=4"
nohup R --slave --vanilla --args 0.0 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.01 r=5 mode=4"
nohup R --slave --vanilla --args 0.01 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.02 r=5 mode=4"
nohup R --slave --vanilla --args 0.02 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.05 r=5 mode=4"
nohup R --slave --vanilla --args 0.05 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.1 r=5 mode=4"
nohup R --slave --vanilla --args 0.1 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.2 r=5 mode=4"
nohup R --slave --vanilla --args 0.2 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=0.5 r=5 mode=4"
nohup R --slave --vanilla --args 0.5 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=1.0 r=5 mode=4"
nohup R --slave --vanilla --args 1.0 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=2.0 r=5 mode=4"
nohup R --slave --vanilla --args 2 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "sigma=5.0 r=5 mode=4"
nohup R --slave --vanilla --args 5 5 4 < hetero_compare_different_noise_convex.R > test4.log 2>&1 &
  echo "waiting..."
wait

echo "All scripts have been run successfully."

