# Residual Spectral Matching for Matrix Completion — Simulation and Real-Data Code

This is the code for the paper *Residual Spectral Matching for Matrix
Completion*. It contains the estimators and the scripts that produce every
table of the paper's numerical study: the simulation experiments (homoscedastic
noise, heteroscedastic noise, weak dependence, ill-conditioned and factor-model
ground truths, and a misspecified working rank) and the Netflix / Amazon
real-data experiments.

**Paper:** [Residual Spectral Matching for Matrix Completion](https://arxiv.org/pdf/2412.10005)

**Scope.** This release reproduces the **simulation** tables (the main-text
matrix-size and different-noise tables, and the supplement's numerical-results
section) and contains the complete driver for the **Netflix / Amazon
real-data** experiments, including the scripts that rebuild the preprocessed
rating CSVs from the raw downloads. The data themselves are not redistributed:
put the preprocessed data in `matrix_test/` next to the release (or set
`REAL_DATA_ROOT`), or run the preprocessing scripts described in Section 2.5.
The matrix-denoising table is **not** part of this release.

---

## Table of Contents

1. [Project Architecture](#1-project-architecture)
   - [Core Estimators (`R/`)](#11-core-estimators-r)
   - [Simulation Drivers (`simulation/`)](#12-simulation-drivers-simulation)
   - [Real-Data Drivers (`real_data/`)](#13-real-data-drivers-real_data)
   - [Table to Script Map](#14-table-to-script-map)
2. [Working Flow](#2-working-flow)
   - [Full Reproduction](#21-full-reproduction)
   - [Simulation Families](#22-simulation-families)
   - [Output File Format](#23-output-file-format)
   - [Dependencies](#24-dependencies)
   - [Real Data: Netflix and Amazon](#25-real-data-netflix-and-amazon)
3. [License](#3-license)

---

## 1. Project Architecture

```
Residual_Spectral_Matching/
├── README.md
├── LICENSE                               # MIT
├── .gitignore
│
├── R/                                    # Estimator implementations
│   ├── rsm.R                             # the residual spectral matching estimators
│   └── baseline_methods.R                # the two baselines
│
├── cpp/                                  # Optional compiled core (Rcpp/Armadillo)
│   ├── README.md                         # speedup, build and kernel reference
│   └── rsm_core.cpp                      # inner loops of the estimators
│
├── simulation/                           # The paper's experiments
│   ├── lib_grids.sh                      # the experiment grids (single source of truth)
│   ├── homo_job.R                        # one homoscedastic job  (one CSV)
│   ├── hetero_job.R                      # one heteroscedastic job (one CSV)
│   ├── run_paper.sh                      # everything: jobs + reports
│   ├── run_homo.sh                       # one homoscedastic family
│   ├── run_hetero.sh                     # the heteroscedastic experiment
│   ├── run_misrank.sh                    # the misspecified-rank family
│   ├── aggregate_paper.R                 # homoscedastic tables -> report
│   ├── aggregate_hetero_paper.R          # heteroscedastic tables -> report
│   └── aggregate_misrank.R               # misspecified-rank tables -> report
│
└── real_data/                            # Netflix / Amazon experiments
    ├── preprocess/
    │   ├── netflix_preprocess.R           # raw Netflix Prize -> case CSVs
    │   ├── amazon_preprocess.R            # raw Amazon Reviews'23 -> case CSVs
    │   └── run_preprocess.sh              # wrapper for the two scripts
    ├── lib_real.sh                       # rank and penalty grids
    ├── real_job.R                        # one fit (one CSV)
    ├── run_real.sh                       # all fits + selection + report
    └── aggregate_real.R                  # the real-data tables
```

The scripts expect `R/` and `simulation/` to keep their relative positions:
the jobs `source('../R/*.R')`, and the drivers are all run with the working
directory set to `simulation/` (each driver does this itself).

`cpp/` is optional. It holds an Rcpp/Armadillo implementation of the inner
loops of `R/rsm.R`: about **3.2–3.3x faster end-to-end** for the factorization
arm and **2.7–2.9x** for the nuclear-norm arm, with the same stopping point
`k*`. Every driver in this release calls the base-R estimators, so nothing in
`cpp/` is needed to reproduce the tables; see [`cpp/README.md`](cpp/README.md)
for the build instructions and the kernel reference.

### 1.1 Core Estimators (`R/`)

| File | Function | Penalty | Noise | Paper section |
|------|----------|---------|-------|---------------|
| `rsm.R` | `Matrix_factor_rsm` | nonconvex factorization, residual spectral matching | homoscedastic | Section 4.3 + Algorithm 1 |
| `rsm.R` | `Matrix_factor_rsm_val` | as above, with held-out-validation stopping | homoscedastic | Section 5 |
| `rsm.R` | `Nuclear_opt_rsm` | nuclear norm, residual spectral matching | homoscedastic | Section 4.3 + Algorithm 2 |
| `rsm.R` | `Nuclear_opt_rsm_val` | as above, with held-out-validation stopping | homoscedastic | Section 5 |
| `rsm.R` | `rsm_estimate_tau` | entrywise noise-scale estimate (whitening) | heteroscedastic | variance-profile estimation |
| `baseline_methods.R` | `Matrix_factor_func` | nonconvex factorization, frobenius | homoscedastic | baselines |
| `baseline_methods.R` | `Nuclear_opt_func` | nuclear norm, proximal gradient | homoscedastic | baselines |

Every estimator in `rsm.R` accepts an optional entrywise weight matrix `W`
(`W_ij = 1/tau_ij`); `W = NULL` is the homoscedastic case. Supplying `W` turns
the same routine into the whitened estimator used for the heteroscedastic
experiments, which is how the whitened arms of `hetero_job.R` are built — there
is no separate whitened implementation.

### 1.2 Simulation Drivers (`simulation/`)

| Script | Role |
|--------|------|
| `lib_grids.sh` | single source of truth for the grids; sourced by every driver |
| `homo_job.R` | one job = one (family, configuration, repetition) = one CSV |
| `hetero_job.R` | one job = one (mode, sigma, repetition) = one CSV |
| `run_homo.sh` | runs one homoscedastic family in parallel |
| `run_hetero.sh` | runs the heteroscedastic experiment in parallel |
| `run_misrank.sh` | runs the misspecified-rank family in parallel |
| `run_paper.sh` | **one command for everything**: all jobs, then all three reports |
| `aggregate_paper.R` | homoscedastic results → `output_paper_report.txt` (truncates it) |
| `aggregate_hetero_paper.R` | heteroscedastic results → appends to the same report |
| `aggregate_misrank.R` | misspecified-rank results → appends to the same report |

Every job writes one CSV and compares the residual spectral matching estimators
(`rsm_fac` / `rsm_nuc`) with the baselines (`base_fac` / `base_nuc`); the
heteroscedastic job adds the whitened arms `hetW_*` and `hetWtrue_*`. There is
one output directory per experiment:

| Experiment | Output directory |
|------------|------------------|
| homoscedastic family `fam` | `output_homo_<fam>/` |
| heteroscedastic | `output_hetero/` |
| misspecified rank | `output_misrank/` |

### 1.3 Real-Data Drivers (`real_data/`)

| File | Role |
|------|------|
| `preprocess/netflix_preprocess.R` | rebuilds the Netflix case CSVs from `netflix_data.csv` |
| `preprocess/amazon_preprocess.R` | rebuilds the Amazon case CSVs from the raw JSONL files |
| `preprocess/run_preprocess.sh` | wrapper: `./run_preprocess.sh netflix <csv>` or `amazon <dir>` |
| `lib_real.sh` | rank and penalty grids, iteration caps, pilot settings |
| `real_job.R` | one fit = one (dataset, case, method, parameter) = one CSV |
| `run_real.sh` | runs the stages `ref` -> `fac` -> `select` -> `nuc` -> `het` -> `report` |
| `aggregate_real.R` | selects the rank (`--select`) and writes the real-data tables |

### 1.4 Table to Script Map

| Paper table | Family / driver | Report section |
|-------------|-----------------|----------------|
| main `tab:change_matrix_size` | `run_homo.sh size` | `aggregate_paper.R` |
| supp `tab:change_matrix_size_sp`, `tab:change_matrix_size_max` | `run_homo.sh size` | `aggregate_paper.R` |
| main `tab:different_noise`, scenario 1 | `run_homo.sh noise` | `aggregate_paper.R` |
| main `tab:different_noise`, scenarios 2–5 | `run_hetero.sh` | `aggregate_hetero_paper.R` |
| supp `tab:supp:different_noise_specnorm`, `tab:supp:different_noise_maxnorm` | `run_homo.sh noise` + `run_hetero.sh` | both |
| supp `tab:supp:different_noise_*_addition` | `run_homo.sh noise` (the `r = 10, 20` rows) | `aggregate_paper.R` |
| supp `tab:supp:ill_fnorm`, `_specnorm`, `_maxnorm` | `run_homo.sh cond` | `aggregate_paper.R` |
| supp `tab:supp:factor_fnorm`, `_specnorm`, `_maxnorm` | `run_homo.sh supp` | `aggregate_paper.R` |
| supp `tab:supp:hete_noise_fnorm`, `_specnorm`, `_maxnorm` | `run_homo.sh robust` | `aggregate_paper.R` |
| supp `tab:supp:dependent_fnorm`, `_specnorm`, `_maxnorm` | `run_homo.sh dep` | `aggregate_paper.R` |
| supp `tab:supp:misrank_fnorm`, `_specnorm`, `_maxnorm` | `run_misrank.sh` | `aggregate_misrank.R` |
| main real-data tables, Netflix | `run_real.sh` (netflix) | `aggregate_real.R` |
| main real-data tables, Amazon | `run_real.sh` (amazon) | `aggregate_real.R` |

The three simulation report generators all write into the single file
`output_paper_report.txt`; `aggregate_paper.R` starts it, the other two append.
Run them in that order. The real-data module is separate: it writes
`real_data/output_real_report.txt` and does not touch the simulation report.

---

## 2. Working Flow

### 2.1 Full Reproduction

```bash
cd simulation
sh run_paper.sh 200 100        # 200 parallel processes, 100 repetitions
```

This runs every family and writes the report. Resuming is the default: a job
whose CSV is already complete is left alone, and a truncated one is recomputed.
To force a full recomputation (for example after changing the code), turn the
check off:

```bash
SKIP_EXISTING=0 sh run_paper.sh 200 100
```

A quick smoke test of the whole pipeline is

```bash
sh run_paper.sh 8 1            # 8 processes, 1 repetition per cell
```

The individual drivers can also be run on their own, which is usually what you
want when iterating on one family; their arguments are listed in 2.2.

### 2.2 Simulation Families

Each family is run by its own driver and writes to its own output directory:

| Family | Command | Output |
|--------|---------|--------|
| `size` | `sh run_homo.sh size 200 100` | `output_homo_size/` |
| `noise` (homoscedastic) | `sh run_homo.sh noise 200 100` | `output_homo_noise/` |
| `noise` (heteroscedastic) | `sh run_hetero.sh 200 output_hetero 100` | `output_hetero/` |
| `cond` | `sh run_homo.sh cond 200 100` | `output_homo_cond/` |
| `supp` | `sh run_homo.sh supp 200 100` | `output_homo_supp/` |
| `robust` | `sh run_homo.sh robust 200 100` | `output_homo_robust/` |
| `dep` | `sh run_homo.sh dep 200 100` | `output_homo_dep/` |
| `misrank` | `sh run_misrank.sh 200 100` | `output_misrank/` |

`run_homo.sh` and `run_misrank.sh` take `[npar] [nrep] [outdir]`;
`run_hetero.sh` takes `[npar] [outdir] [nrep]`. The grids and simulation
settings live in `lib_grids.sh`; the paper and supplement describe the
experiment design.

### 2.3 Output File Format

One job writes one CSV into its experiment's output directory:

```
output_homo_<family>/homo_<family>_<m>_<n>_<rho>_<r>_<s>_<key>_rep<nnn>.csv
output_hetero/hetero_m<mode>_s<sigma>_rep<nnn>.csv
output_misrank/homo_misrank_<m>_<n>_<rho>_<r>_<s>_<sigma>_rep<nnn>.csv
```

Each CSV holds a single row. The columns are:

- **settings** — `family`, `rep`, `m`, `n`, `rho`, `r`, `s`, `sigma`, `kappa`,
  `hetero_size`, `dep_mode`, `dep_ratio` (or `mode`, `sigma`, `rep` for the
  heteroscedastic jobs).
- **error metrics** — for each estimator `<arm>`, three columns `<arm>`,
  `<arm>_sp`, `<arm>_inf`, and a wall-time column `t_<arm>`.
- **stopping point** — `<arm>_k`, present for the arms that use the held-out
  stopping rule: the number of SSA steps that rule selected.
- **penalty** — `rsm_nuc_lam` / `hetW_nuc_lam` / `hetWtrue_nuc_lam`, the swept
  nuclear-norm penalty the arm selected.

The three metrics are the relative errors between the estimate `M_hat` and the
ground truth `M_0`:

| Column suffix | Metric | Definition |
|---------------|--------|------------|
| *(none)*, `E_F` | relative Frobenius | `sqrt(mean((M_0 - M_hat)^2) / mean(M_0^2))` |
| `_sp`, `E_sp` | relative spectral | `d_1(M_0 - M_hat) / d_1(M_0)` |
| `_inf`, `E_inf` | relative max | `max|M_0 - M_hat| / max|M_0|` |

The estimator arms are:

| Arm | Estimator |
|-----|-----------|
| `rsm_fac`, `rsm_nuc` | residual spectral matching, with the held-out stopping rule |
| `hetW_fac`, `hetW_nuc` | whitened residual spectral matching, estimated noise profile |
| `hetWtrue_fac`, `hetWtrue_nuc` | whitened residual spectral matching, true noise profile |
| `base_fac`, `base_nuc` | the two baselines |

The report generators average every arm over the repetitions of a cell and
print, per family, one table per metric plus a summary of the relative `E_F`
improvement of each RSM arm over its baseline. Percentages are
`100 * (baseline - RSM) / baseline`, so **positive means the RSM arm
has the lower error**. A trailing `*` marks a cell whose reference is already
at machine level, where the percentage is a ratio of two ~zero errors and
carries no information; read the absolute columns there instead. The report
also prints which cells are limited by the iteration cap rather than by the
stopping rule, and which cells have failed repetitions.

Each job also writes `log_*.txt` next to its CSV. A successful job ends its log
with `OK <path>` (or `SKIP <path>` when it was resumed); an empty log means the
R process died. A failed arm writes `NA` and the job still prints `OK`, so run
`grep -l "ARM ERROR" output_*/log_*.txt` as the failure check.

### 2.4 Dependencies

- **R >= 3.6** and base R only for the estimators and drivers — no contributed
  packages are used. The one exception is Amazon preprocessing, which needs
  `jsonlite` for chunked JSONL reading; nothing else imports it.
- An R build with a multithreaded BLAS is supported; every R process sets
  `OMP_NUM_THREADS=1` (and the OpenBLAS / MKL / GotoBLAS equivalents) itself, so
  parallelism comes from `npar` and not from the linear algebra library. Set
  `npar` to the number of cores you want to occupy.
- `sh` and `xargs` for the drivers.
- The optional `cpp/` core needs `Rcpp`, `RcppArmadillo` and a C++ toolchain.
  Nothing else in the release does.

### 2.5 Real Data: Netflix and Amazon

The driver reads preprocessed rating CSVs. They are not redistributed. It
expects a directory containing the two data trees:

```
matrix_test/
├── real_data_example/data/case{1,2,3}_{train,validation,test}data.csv
└── real_data_example2/data/case{1,2,3}_{train,validation,test}data.csv
```

The experiments use the paper's real-data scenarios 1, 2 and 3 for both
datasets: Netflix with user thresholds of 700, 200 and 75 ratings, and Amazon
with the Movies_and_TV, Books and Electronics categories.

No environment variable is needed when `matrix_test/` sits next to the
release. If `REAL_DATA_ROOT` is unset, the driver uses the first directory it
finds that contains `real_data_example/` or `real_data_example2/`, checking
`matrix_test/` in the current directory, the current directory itself,
`matrix_test/` next to the release (and one or two levels above it), and
`data/` or the release root. Set `REAL_DATA_ROOT` only when the data is
somewhere else.

**CSV schema.** Every split file has the same layout:

```
"","X1_train","X2_train","Y_train"
"1",277,836,5
```

Column 1 is the CSV row name. Columns 2, 3 and 4 are the row index (`1..m`),
the column index (`1..n`) and the rating. `real_job.R` reads columns 2–4 and
ignores the header names. `m` and `n` are taken from the training split;
the same index space is used by the validation and test files.

**Sources.**

- *Netflix*: the Netflix Prize data,
  <https://www.kaggle.com/datasets/netflix-inc/netflix-prize-data>
  (a Kaggle account is required; the direct mirror is
  <https://archive.org/download/nf_prize_dataset.tar>). The raw
  `combined_data_1.txt` … `combined_data_4.txt` store one movie per block: a
  header line `movie_id:` followed by lines `user_id,rating,date`. Flatten
  them into the single file `netflix_data.csv` with the header
  `movies,id,score,time`:

  ```sh
  { echo 'movies,id,score,time'
    awk -F, '/^[0-9]+:$/ {m=$1; sub(/:$/, "", m); next}
             {print m "," $0}' combined_data_*.txt
  } > netflix_data.csv
  ```

- *Amazon*: Amazon Reviews'23, <https://amazon-reviews-2023.github.io/>
  (Hugging Face mirror: `McAuley-Lab/Amazon-Reviews-2023`). Download and
  decompress the three review categories used here:

  ```
  https://mcauleylab.ucsd.edu/public_datasets/data/amazon_2023/raw/review_categories/Movies_and_TV.jsonl.gz
  https://mcauleylab.ucsd.edu/public_datasets/data/amazon_2023/raw/review_categories/Books.jsonl.gz
  https://mcauleylab.ucsd.edu/public_datasets/data/amazon_2023/raw/review_categories/Electronics.jsonl.gz
  ```

  Decompress each `.jsonl.gz` with `gunzip`. Each line is a JSON review; the
  fields used here are `parent_asin` (item), `asin`, `user_id` and `rating`.
  Reading these files requires `jsonlite`
  (`stream_in`); this is only a preprocessing dependency — the estimators and
  drivers in this release use base R only.

**Preprocessing.** Both datasets use the same procedure: keep popular items,
keep sufficiently active users, reindex the surviving users and items to
`1..m` and `1..n`, permute the observed entries, and split them 4/6 train,
1/6 validation, 1/6 test. The user sample is drawn with seed `20240504`.
A user's activity is counted within the selected items only. The exact filters
(all that the preprocessing does) are:

| Data | Case | Items | Users |
|------|------|-------|-------|
| Netflix | 1 | movies with > 25,000 ratings (n = 1034) | all users with > 700 ratings (m = 3036, no sampling) |
| Netflix | 2 | same | users with > 200 ratings, random 3000 (m = 3000) |
| Netflix | 3 | same | users with > 75 ratings, random 3000 (m = 3000) |
| Amazon | 1 | Movies_and_TV: top 1000 `parent_asin` by review count | users with > 14 ratings, random 3000 |
| Amazon | 2 | Books: top 1000 `parent_asin` | users with > 12 ratings, random 3000 |
| Amazon | 3 | Electronics: top 1000 `parent_asin` | users with > 12 ratings, random 3000 |

**Rebuild.** `real_data/preprocess/` contains the scripts that produce these
files from the raw downloads:

```sh
cd real_data/preprocess
./run_preprocess.sh netflix /path/to/netflix_data.csv
./run_preprocess.sh amazon  /path/to/raw_jsonl   # holds the three *.jsonl files
```

The output root is `REAL_DATA_ROOT`, an optional last argument
(`./run_preprocess.sh netflix <csv> [out_root]`), or by default
`matrix_test/` next to the release — reused if it already holds one of the
two data trees, created otherwise. The two R scripts follow the same rule and
can also be called directly (`Rscript netflix_preprocess.R <csv> [out_root]`);
they create the `data/` directories themselves and print the resulting `m`,
`n` and rating count per case. The Netflix script reads
`netflix_data.csv` whole. The Amazon script makes two streaming passes per
category with `jsonlite::stream_in` — one to count reviews per `parent_asin`,
one to keep the selected rows — so the multi-gigabyte JSONL files are never
loaded whole.

The resulting training matrices are:

| Data | Case | m | n | train density `rho` |
|------|------|---|---|---------------------|
| Netflix | 1 | 3036 | 1034 | 0.497 |
| Netflix | 2 | 3000 | 1034 | 0.239 |
| Netflix | 3 | 3000 | 1034 | 0.154 |
| Amazon | 1 | 3000 | 1000 | 0.0128 |
| Amazon | 2 | 3000 | 1000 | 0.0110 |
| Amazon | 3 | 3000 | 1000 | 0.0128 |

**Estimators.** `rsm_fac` / `rsm_nuc` are the RSM factorization and
nuclear-norm estimators in `R/rsm.R`; `hetW_fac` / `hetW_nuc` are the same
estimators whitened with an estimated noise profile; `base_fac` / `base_nuc`
are the two baselines. The report has twelve columns — the six methods
unclipped and clipped to `[1, 5]` — and a second table with absolute test RMSE.

**Run.**

```sh
cd real_data
./run_real.sh 100          # 100 parallel processes, all stages, resumable
./run_real.sh 100 fac      # one stage only
./run_real.sh 100 report   # regenerate the report from existing fits
```

The stages are `ref` (cache the reference spectra), `fac` (RSM and baseline
nonconvex arms), `select` (pick the RSM rank by validation RMSE), `nuc` (RSM
and baseline nuclear arms), `het` (whitened pilot, then the whitened factor
and nuclear arms) and `report`. `SKIP_EXISTING=1` (default) makes the sweep
resumable.

**Output.** One fit writes one CSV to
`output_real/<dataset>/<method>_case<case>_<key>.csv`, where `key` is the rank,
the penalty, or `s:lambda` (plus `_auto` when the step was selected). Each row
records `status`, `step`, `s`, `lambda`, `k_best`, the raw and clipped
validation/test RMSE, and the relative test RMSE. The relative test RMSE is
`RMSE(test) / sqrt(mean(Y_test^2))`. `aggregate_real.R` reads these CSVs and
writes `output_real_report.txt`.

---

## 3. License

MIT. See [LICENSE](LICENSE). The optional `cpp/` core is covered by the same
license.
