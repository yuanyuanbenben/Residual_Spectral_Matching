#!/bin/bash
# ============================================================================
# Driver for the real-data experiments.
#
#   ./run_real.sh [npar] [stage]   stage = all | ref | fac | select | nuc | het | report
#
# Defaults: npar = 100, stage = all.  Environment:
#   REAL_DATA_ROOT  directory holding real_data_example/ and real_data_example2/.
#                   Optional: if unset, the driver looks for the data next to
#                   the release, in matrix_test/ one or two levels above it, or
#                   in the directory it was called from.
#   REAL_CAP        SSA iteration cap for the RSM fits (default 1500)
#   REAL_PATIENCE   held-out patience (default 10)
#   REAL_TREF       reference-spectrum ensembles (default 100)
#   SKIP_EXISTING=1 (default) resume by skipping finished CSVs
#
# Stages, in order:
#   ref | fac | select | nuc | het | report
# ============================================================================
set -u
CALLER_PWD=$PWD
cd "$(dirname "$0")"
RELEASE=$(cd .. && pwd)
. ./lib_real.sh

resolve_root() {
  if [ -n "${REAL_DATA_ROOT:-}" ]; then printf '%s\n' "$REAL_DATA_ROOT"; return 0; fi
  for d in "$CALLER_PWD/matrix_test" "$CALLER_PWD" \
           "$RELEASE/matrix_test" "$RELEASE/../matrix_test" \
           "$RELEASE/../../matrix_test" "$RELEASE/data" "$RELEASE"; do
    if [ -d "$d/real_data_example" ] || [ -d "$d/real_data_example2" ]; then
      (cd "$d" && pwd); return 0
    fi
  done
  return 1
}
if ROOT=$(resolve_root); then
  REAL_DATA_ROOT=$ROOT
else
  echo "error: no real_data_example/ or real_data_example2/ found." >&2
  echo "Put the preprocessed data in matrix_test/ next to the release, or set REAL_DATA_ROOT." >&2
  exit 1
fi
export REAL_DATA_ROOT
echo "data root: $REAL_DATA_ROOT"

NPAR=${1:-100}
STAGE=${2:-all}
SKIP_EXISTING=${SKIP_EXISTING:-1}
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
export OPENBLAS_NUM_THREADS=${OPENBLAS_NUM_THREADS:-1}
export NETFLIX_CAP AMAZON_CAP HET_PILOT_ITERS
unset OPENBLAS_MAIN_FREE 2>/dev/null || true

mkdir -p output_real logs_real cache

run_queue() {
  local q="$1"
  xargs -P "$NPAR" -a "$q" -L 1 bash -c '
    set -u
    ds=$0; c=$1; meth=$2; par=$3; st=${4:-}
    key=$(printf "%s" "$par" | sed "s/[^0-9A-Za-z.-]/_/g")
    if [ -n "$st" ]; then
      if [ "$st" = auto ]; then key="${key}_auto"; else key="${key}_st${st}"; fi
    fi
    f="output_real/$ds/${meth}_case${c}_${key}.csv"
    if [ "${SKIP_EXISTING:-1}" = 1 ] && [ -s "$f" ] && grep -q '"OK"' "$f"; then
      echo "SKIP $f"; exit 0
    fi
    if [ -z "${REAL_CAP:-}" ]; then
      if [ "$ds" = amazon ]; then REAL_CAP=${AMAZON_CAP:-800}; else REAL_CAP=${NETFLIX_CAP:-1500}; fi
    fi
    log="logs_real/${ds}_${meth}_case${c}_${key}.log"
    if REAL_CAP="$REAL_CAP" REAL_SSA_STEP="$st" \
         Rscript real_job.R "$ds" "$c" "$meth" "$par" "output_real/$ds" > "$log" 2>&1; then
      echo "OK   $f"
    else
      echo "FAIL $f  (see $log)"
    fi
  ' < "$q"
  rm -f "$q"
}

gen_ref() {
  for ds in netflix amazon; do
    if [ "$ds" = netflix ]; then cases=$NETFLIX_CASES; else cases=$AMAZON_CASES; fi
    for c in $cases; do echo "$ds $c ref 0"; done
  done
}

gen_fac() {
  for c in $NETFLIX_CASES; do
    for s in $(netflix_s_grid "$c"); do
      echo "netflix $c base_fac $s"
      echo "netflix $c rsm_fac $s $STEP_MODE"
    done
  done
  for c in $AMAZON_CASES; do
    for s in $(amazon_s_grid); do
      echo "amazon $c base_fac $s"
      echo "amazon $c rsm_fac $s $STEP_MODE"
    done
  done
}

gen_nuc() {
  for c in $NETFLIX_CASES; do
    for l in $NETFLIX_LAM; do echo "netflix $c base_nuc $l"; done
  done
  for c in $AMAZON_CASES; do
    for l in $AMAZON_LAM; do echo "amazon $c base_nuc $l"; done
  done
  sel=cache/selected_s.csv
  if [ ! -s "$sel" ]; then
    echo "!! $sel missing: run the 'select' stage first" >&2
    return 1
  fi
  tail -n +2 "$sel" | while IFS=, read -r ds c s _rest; do
    for l in $RSMNUC_LAM; do echo "$ds $c rsm_nuc ${s}:${l} $STEP_MODE"; done
  done
}

gen_het_ref() {
  sel=cache/selected_s.csv
  if [ ! -s "$sel" ]; then
    echo "!! $sel missing: run the 'select' stage first" >&2
    return 1
  fi
  tail -n +2 "$sel" | while IFS=, read -r ds c s _rest; do
    echo "$ds $c het_ref $s $STEP_MODE"
  done
}

gen_het() {
  sel=cache/selected_s.csv
  if [ ! -s "$sel" ]; then
    echo "!! $sel missing: run the 'select' stage first" >&2
    return 1
  fi
  tail -n +2 "$sel" | while IFS=, read -r ds c s _rest; do
    echo "$ds $c hetW_fac $s $STEP_MODE"
    for l in $HET_LAM; do echo "$ds $c hetW_nuc ${s}:${l} $STEP_MODE"; done
  done
}

do_stage() {
  local s="$1"
  case "$s" in
    ref)    gen_ref > cache/q_ref.txt;  echo "== ref ==";  run_queue cache/q_ref.txt ;;
    fac)    gen_fac > cache/q_fac.txt;  echo "== fac ==";  run_queue cache/q_fac.txt ;;
    select) Rscript aggregate_real.R --select ;;
    nuc)    gen_nuc > cache/q_nuc.txt || exit 1; echo "== nuc =="; run_queue cache/q_nuc.txt ;;
    het)
      gen_het_ref > cache/q_het_ref.txt || exit 1
      echo "== het_ref =="; run_queue cache/q_het_ref.txt
      gen_het > cache/q_het.txt || exit 1
      echo "== het =="; run_queue cache/q_het.txt
      ;;
    report) Rscript aggregate_real.R output_real_report.txt ;;
    all)
      do_stage ref
      do_stage fac
      do_stage select
      do_stage nuc
      do_stage het
      do_stage report
      ;;
    *) echo "unknown stage: $s" >&2; exit 1 ;;
  esac
}

do_stage "$STAGE"
