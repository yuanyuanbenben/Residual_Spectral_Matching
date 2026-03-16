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
echo "poly decay ratio=0.2 r=5"
nohup R --slave --vanilla --args 1 0.2 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.4 r=5"
nohup R --slave --vanilla --args 1 0.4 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.6 r=5"
nohup R --slave --vanilla --args 1 0.6 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.8 r=5"
nohup R --slave --vanilla --args 1 0.8 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.2 r=10"
nohup R --slave --vanilla --args 1 0.2 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.4 r=10"
nohup R --slave --vanilla --args 1 0.4 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.6 r=10"
nohup R --slave --vanilla --args 1 0.6 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.8 r=10"
nohup R --slave --vanilla --args 1 0.8 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.2 r=20"
nohup R --slave --vanilla --args 1 0.2 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.4 r=20"
nohup R --slave --vanilla --args 1 0.4 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.6 r=20"
nohup R --slave --vanilla --args 1 0.6 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "poly decay ratio=0.8 r=20"
nohup R --slave --vanilla --args 1 0.8 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait


echo "M depen M=2 r=5"
nohup R --slave --vanilla --args 2 2 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=3 r=5"
nohup R --slave --vanilla --args 2 3 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=5 r=5"
nohup R --slave --vanilla --args 2 5 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=10 r=5"
nohup R --slave --vanilla --args 2 10 5 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=2 r=10"
nohup R --slave --vanilla --args 2 2 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=3 r=10"
nohup R --slave --vanilla --args 2 3 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=5 r=10"
nohup R --slave --vanilla --args 2 5 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=10 r=10"
nohup R --slave --vanilla --args 2 10 10 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=2 r=20"
nohup R --slave --vanilla --args 2 2 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=3 r=20"
nohup R --slave --vanilla --args 2 3 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=5 r=20"
nohup R --slave --vanilla --args 2 5 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait

echo "M depen M=10 r=20"
nohup R --slave --vanilla --args 2 10 20 < compare_different_weakdependent.R > test2.log 2>&1 &
  echo "waiting..."
wait
echo "All scripts have been run successfully."

