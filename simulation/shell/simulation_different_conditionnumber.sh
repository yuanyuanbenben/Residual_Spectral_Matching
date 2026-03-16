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
echo "kappa=2 r=5"
nohup R --slave --vanilla --args 2 5 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=5 r=5"
nohup R --slave --vanilla --args 5 5 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=10 r=5"
nohup R --slave --vanilla --args 10 5 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=100 r=5"
nohup R --slave --vanilla --args 100 5 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=1000 r=5"
nohup R --slave --vanilla --args 1000 5 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=10000 r=5"
nohup R --slave --vanilla --args 10000 5 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=2 r=10"
nohup R --slave --vanilla --args 2 10 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=5 r=10"
nohup R --slave --vanilla --args 5 10 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=10 r=10"
nohup R --slave --vanilla --args 10 10 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=100 r=10"
nohup R --slave --vanilla --args 100 10 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=1000 r=10"
nohup R --slave --vanilla --args 1000 10 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=10000 r=10"
nohup R --slave --vanilla --args 10000 10 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=2 r=20"
nohup R --slave --vanilla --args 2 20 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=5 r=20"
nohup R --slave --vanilla --args 5 20 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=10 r=20"
nohup R --slave --vanilla --args 10 20 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=100 r=20"
nohup R --slave --vanilla --args 100 20 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=1000 r=20"
nohup R --slave --vanilla --args 1000 20 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "kappa=10000 r=20"
nohup R --slave --vanilla --args 10000 20 < compare_different_conditionnumber.R > test3.log 2>&1 &
  echo "waiting..."
wait

echo "All scripts have been run successfully."

