#!/bin/sh
# ============================================================================
# One command -> every simulation table of the paper.
#
# Usage:  sh run_paper.sh [npar] [nrep]        (defaults 200, 100)
#   npar  parallel R processes.  Set it to the number of cores you want to use;
#         each R process is pinned to one thread (OMP_NUM_THREADS=1).
#   nrep  repetitions per configuration.  The paper uses 100; a quick smoke test
#         is `sh run_paper.sh 8 1`.
#
#   Resuming is the default: a job whose CSV is already complete is left alone;
#   a truncated one is recomputed.  SKIP_EXISTING=0 forces a full recomputation.
#
# Stages:
#   1. heteroscedastic experiment                -> output_hetero/
#   2. the six homoscedastic families            -> output_homo_<fam>/
#   3. misspecified-rank family                  -> output_misrank/
#   4. report generators, in this order:
#        aggregate_paper.R  aggregate_hetero_paper.R  aggregate_misrank.R
#      (the first truncates output_paper_report.txt, the other two append)
#
# Protocol: RSM_STOP=val RSM_INIT=oracle RSM_CAP=1500 HOMO_TOR=1e-4.
# ============================================================================
set -e
cd "$(dirname "$0")"
. ./lib_grids.sh

NPAR=${1:-200}
NREP=${2:-100}

export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 GOTO_NUM_THREADS=1
export RSM_STOP=val RSM_INIT=oracle RSM_CAP=1500 HOMO_TOR=1e-4
SKIP_EXISTING=${SKIP_EXISTING:-1}; export SKIP_EXISTING

# ---- 1. the heteroscedastic experiment -------------------------------------
sh run_hetero.sh "$NPAR" output_hetero "$NREP"

# ---- 2. the six homoscedastic families -------------------------------------
gen() {
  for fam in $HOMO_FAMS; do
    homo_job_specs "$fam" "$NREP" | awk -v f="$fam" '{print "H", f, $1, $2, $3, $4}'
  done
}

Q=$(mktemp "${TMPDIR:-/tmp}/paper_jobs.XXXXXX")
trap 'rm -f "$Q"' EXIT
gen > "$Q"
NJOB=$(wc -l < "$Q")
echo "queue: $NJOB jobs at -P $NPAR"

cat "$Q" | xargs -P "$NPAR" -n 6 sh -c '
  case "$0" in
    H)  o="output_homo_$1"; mkdir -p "$o"
        Rscript homo_job.R "$1" "$5" "$o" "$2" "$3" "$4" \
          > "$o/log_${2}_${3}_${4}_${5}.txt" 2>&1 ;;
  esac' || echo "!! xargs reported a non-zero exit (see the per-job logs)"

echo "log scan (ARM ERROR = an arm was silently swallowed to NA):"
grep -rl "ARM ERROR\|Execution halted" output_homo_*/log_*.txt output_hetero/log_*.txt 2>/dev/null | head -20 || true
find output_homo_* output_hetero -name 'log_*.txt' -size 0 2>/dev/null | head -20

# ---- 3. the misspecified-rank family ---------------------------------------
sh run_misrank.sh "$NPAR" "$NREP" output_misrank

# ---- 4. the reports --------------------------------------------------------
Rscript aggregate_paper.R output_paper_report.txt
Rscript aggregate_hetero_paper.R output_paper_report.txt output_hetero
Rscript aggregate_misrank.R output_paper_report.txt

echo "report: output_paper_report.txt ($(wc -l < output_paper_report.txt) lines)"
