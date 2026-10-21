# ============================================================================
# Single source of truth for the paper's simulation grids.
# Sourced by run_homo.sh, run_hetero.sh, run_misrank.sh and run_paper.sh.
#
#   homo_job_specs   <family> <nrep>   -> one line per job: "<p1> <p2> <p3> <rep>"
#   hetero_job_specs <nrep>            -> one line per job: "<mode> <sigma> <rep>"
#
# Each line is one job; the four (resp. three) fields are the positional
# parameters that follow the shared arguments of homo_job.R (resp.
# hetero_job.R).  HOMO_FAMS lists the six homoscedastic families and SIGMAS
# its 10-value sigma grid.
# ============================================================================
HOMO_FAMS="size cond noise supp robust dep"
SIGMAS="0 0.01 0.02 0.05 0.1 0.2 0.5 1 2 5"

homo_job_specs() {
  fam=$1; nrep=$2
  case "$fam" in
    size)
      for m in 200 300 400 500 600 700 800 900 1000; do
        for r in 5 10 20; do
          for rep in $(seq 1 "$nrep"); do echo "$m $r 0 $rep"; done
        done
      done ;;
    cond)
      for k in 2 5 10 100 1000 10000; do
        for r in 5 10 20; do
          for rep in $(seq 1 "$nrep"); do echo "$k $r 0 $rep"; done
        done
      done ;;
    noise)
      for r in 5 10 20; do
        for sg in 0 0.01 0.02 0.05 0.1 0.2 0.5 1 2 5; do
          for rep in $(seq 1 "$nrep"); do echo "$r $sg 0 $rep"; done
        done
      done ;;
    supp)
      for sg in 0 0.001 0.002 0.005 0.01 0.02 0.05 0.1 0.2 0.5 1 2; do
        for rep in $(seq 1 "$nrep"); do echo "$sg 0 0 $rep"; done
      done ;;
    robust)
      for m in 200 300 400 500 600 700 800 900 1000; do
        for h in 0.1 0.2 0.3; do
          for rep in $(seq 1 "$nrep"); do echo "$m 5 $h $rep"; done
        done
      done
      for h in 0.1 0.2; do
        for rep in $(seq 1 "$nrep"); do echo "200 10 $h $rep"; done
      done ;;
    dep)
      for r in 5 10 20; do
        for rt in 0.2 0.4 0.6 0.8; do
          for rep in $(seq 1 "$nrep"); do echo "$r 1 $rt $rep"; done
        done
        for rt in 2 3 5 10; do
          for rep in $(seq 1 "$nrep"); do echo "$r 2 $rt $rep"; done
        done
      done ;;
    misrank)
      # the misspecified-rank family: true rank r, working rank s = r..r+6.
      for r in 5 10 20; do
        for k in 0 1 2 3 4 5 6; do
          for rep in $(seq 1 "$nrep"); do echo "$r $((r + k)) 0 $rep"; done
        done
      done ;;
    *) echo "unknown family: $fam" >&2; return 1 ;;
  esac
}

hetero_job_specs() {
  nrep=$1
  for sg in $SIGMAS; do
    for md in 1 2 3 4; do
      for rep in $(seq 1 "$nrep"); do echo "$md $sg $rep"; done
    done
  done
}
