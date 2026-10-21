# Shared settings for the real-data driver.  Sourced by run_real.sh.

# The paper's real-data scenarios 1-3.
NETFLIX_CASES="1 2 3"
AMAZON_CASES="1 2 3"

# Netflix rank grid by case.
netflix_s_grid() {
  case "$1" in
    1)     seq 1 40 ;;
    2 | 3) seq 1 30 ;;
    *)     return 1 ;;
  esac
}

amazon_s_grid() { seq 1 10; }

# Penalty grids for the nuclear-norm baseline.
NETFLIX_LAM="100 130 160 200 230 260 300 330 360 400"
AMAZON_LAM="100 130 160 200 230 260 300 330 360 400 430 460 500 530 560 600 630 660 700 730 760 800 830 860 900 930 960 1000 1100 1200 1300 1400 1500 1700 2000 2300 2500 2800 3000 5000"
RSMNUC_LAM="1e-7 3.16e-7 1e-6 3.16e-6 1e-5 3.16e-5 1e-4 3.16e-4 1e-3 3.16e-3 1e-2"

STEP_MODE=${STEP_MODE:-auto}

HET_PILOT_ITERS=${HET_PILOT_ITERS:-300}
HET_LAM=${HET_LAM:-"1e-7 1e-6 1e-5 1e-4 1e-3 1e-2"}

NETFLIX_CAP=${NETFLIX_CAP:-1500}
AMAZON_CAP=${AMAZON_CAP:-800}
