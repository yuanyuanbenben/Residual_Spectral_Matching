# Residual Spectral Matching for Matrix Completion

This project implements matrix completion methods based on residual spectral matching, including both convex optimization (nuclear norm) and non-convex optimization (matrix factorization) approaches. The code supports multiple noise settings (homoscedastic, heteroscedastic, weakly dependent) and various experimental conditions.

**Paper:** [Residual Spectral Matching for Matrix Completion](https://arxiv.org/pdf/2412.10005)

---

## Table of Contents

1. [Project Architecture](#1-project-architecture)
2. [Working Flow](#2-working-flow)
   - [Simulation Experiments](#21-simulation-experiments)
   - [Real Data Experiments](#22-real-data-experiments)
   - [Output File Format](#23-output-file-format)
   - [Dependencies](#24-dependencies)
   - [Quick Start](#25-quick-start)

---

## 1. Project Architecture

```
Residual_Spectral_Matching/
├── R/                                          # Core Algorithm Module
│   ├── baseline_methods.R                      # Baseline method implementations
│   ├── proposed_method.R                       # Proposed method implementations
│   └── hetero_proposed_method.R                # Heteroscedastic noise extensions
│
├── simulation/                                 # Simulation Experiments Module
│   ├── shell/                                  # Shell scripts for batch execution
│   ├── output/                                 # Simulation results (CSV files)
│   └── plot/                                   # Result visualizations
│
├── real_data_example/                          # Real Data Example 1 （Netflix）
│   ├── data/                                   # Raw data
│   ├── output/                                 # Output results
│   └── plot/                                   # Result visualizations
│
└── real_data_example2/                         # Real Data Example 2 (Amazon)
    ├── data/
    ├── output/
    └── plot/
```

### 1.1 Core Algorithm Module (`R/`)

| File | Description |
|------|-------------|
| `baseline_methods.R` | Baseline methods: Matrix factorization gradient descent, Nuclear norm proximal gradient descent |
| `proposed_method.R` | Proposed method: Residual spectral matching optimized matrix completion |
| `hetero_proposed_method.R` | Extended method for heteroscedastic noise |

### 1.2 Simulation Experiments Module (`simulation/`)

Contains 15 R scripts corresponding to different experimental settings. Each experiment has both convex and non-convex versions:

| R Script | Corresponding Shell Scripts | Experimental Purpose |
|----------|----------------------------|---------------------|
| `compare_to_baseline*.R` | `simulation*.sh`, `convex_simulation*.sh` | Performance comparison under different matrix dimensions, sampling rates, and ranks |
| `compare_different_noise*.R` | `simulation_different_sigma*.sh` | Performance comparison under different noise levels |
| `compare_different_conditionnumber*.R` | `simulation_different_conditionnumber*.sh` | Performance comparison under different condition numbers |
| `compare_different_weakdependent*.R` | `simulation_different_weakdependent*.sh` | Performance comparison under weakly dependent noise structures |
| `robust_compare_to_baseline*.R` | `robust_simulation*.sh` | Robustness comparison under heteroscedastic noise |
| `hetero_compare_different_noise*.R` | `hetero_simulation_different_sigma*.sh` | Performance comparison under heteroscedastic noise patterns |
| `compare_different_noise_supp*.R` | `supp_simulation_different_sigma*.sh` | Supplementary experiments (factor model ground truth) |
| `matrix_denoising.R` | `denoising.sh` | Matrix denoising method comparison (3 methods) |
| `residual_plot.R` | - | Residual spectrum distribution visualization |

### 1.3 Real Data Examples (`real_data_example/`, `real_data_example2/`)

| Folder | Content |
|--------|---------|
| `real_data_example/` | Testing and residual analysis for Netflix datasets |
| `real_data_example2/` | Testing and residual analysis for Amazon datasets |

---

## 2. Working Flow

### 2.1 Simulation Experiments

#### (1) Baseline Comparison Experiments (`compare_to_baseline.R` / `compare_to_baseline_convex.R`)

**Purpose**: Compare the proposed method against baseline methods under various matrix dimensions, observation fractions, and rank settings.

**Command-line Arguments**:
- `args[1]`: m (number of rows)
- `args[2]`: n (number of columns)
- `args[3]`: rho*100 (observation percentage)
- `args[4]`: number of parallel workers
- `args[5]`: s (factorization rank)
- `args[6]`: r (true rank)
- `args[7]`: stepsize1 (learning rate for proposed method)
- `args[8]`: stepsize2 (learning rate for baseline method)

**How to Run**:
```bash
cd simulation/shell
./simulation.sh          # Non-convex optimization
./convex_simulation.sh   # Convex optimization
```

---

#### (2) Different Noise Level Experiments (`compare_different_noise.R` / `compare_different_noise_convex.R`)

**Purpose**: Evaluate method performance under different noise standard deviations.

**Command-line Arguments**:
- `args[1]`: sigma (noise level, from 0.0 to 0.5)
- `args[2]`: r (matrix rank)

**Fixed Settings**: m=500, n=250, rho=0.2

**How to Run**:
```bash
./simulation_different_sigma.sh          # Non-convex
./convex_simulation_different_sigma.sh   # Convex
```

---

#### (3) Different Condition Number Experiments (`compare_different_conditionnumber.R` / `compare_different_conditionnumber_convex.R`)

**Purpose**: Test method robustness under different condition numbers (singular value decay rates).

**Command-line Arguments**:
- `args[1]`: kappa (condition number)
- `args[2]`: r (matrix rank)

**Fixed Settings**: m=500, n=250, rho=0.05

**How to Run**:
```bash
./simulation_different_conditionnumber.sh          # Non-convex
./convex_simulation_different_conditionnumber.sh   # Convex
```

---

#### (4) Weakly Dependent Noise Experiments (`compare_different_weakdependent.R` / `compare_different_weakdependent_convex.R`)

**Purpose**: Evaluate method performance when noise has weak temporal/spatial correlations.

**Noise Structures**:
- **Mode 1**: Exponential decay covariance `Sigma[i,j] = ratio^|i-j|`
- **Mode 2**: Banded structure (1 within bandwidth, 0 otherwise)

**Command-line Arguments**:
- `args[1]`: dependent_mode (1 or 2)
- `args[2]`: dependent_ratio (decay base or bandwidth)
- `args[3]`: r (matrix rank)

**How to Run**:
```bash
./simulation_different_weakdependent.sh          # Non-convex
./convex_simulation_different_weakdependent.sh   # Convex
```

---

#### (5) Robustness Comparison Experiments (`robust_compare_to_baseline.R` / `robust_compare_to_baseline_convex.R`)

**Purpose**: Test method robustness under heteroscedastic noise.

**Noise Generation**: Each element multiplied by `Uniform(1-hetero_size, 1+hetero_size)`

**Command-line Arguments**:
- `args[1]`: m (rows)
- `args[2]`: n (columns)
- `args[3]`: rho*100 (observation percentage)
- `args[4]`: number of parallel workers
- `args[5]`: r (true rank)
- `args[6]`: lambda_tuning1*10 (tuning parameter for proposed method)
- `args[7]`: lambda_tuning2*10 (tuning parameter for baseline method)
- `args[8]`: hetero_size (degree of heteroscedasticity)

**How to Run**:
```bash
./robust_simulation.sh          # Non-convex
./robust_simulation_convex.sh   # Convex
```

---

#### (6) Heteroscedastic Noise Experiments (`hetero_compare_different_noise.R` / `hetero_compare_different_noise_convex.R`)

**Purpose**: Compare three methods (baseline, proposed, heteroscedastic extension) under four heteroscedastic noise patterns.

**Noise Patterns**:
- **Mode 1**: Row-wise Bernoulli scaling `H[i,j] = H[i,j] * Bernoulli(0.2)`
- **Mode 2**: Row-wise Gaussian scaling `H[i,j] = H[i,j] * N(0,1)`
- **Mode 3**: Row-column separable Bernoulli scaling
- **Mode 4**: Row-column separable Gaussian scaling

**Command-line Arguments**:
- `args[1]`: sigma (noise level)
- `args[2]`: r (matrix rank)
- `args[3]`: mode (noise type, 1-4)

**How to Run**:
```bash
./hetero_simulation_different_sigma.sh          # Non-convex (3 methods comparison)
./hetero_simulation_different_sigma_convex.sh   # Convex
```

---

#### (7) Supplementary Experiments (`compare_different_noise_supp.R`)

**Purpose**: Generate ground truth matrix using factor model `M_0 = L * R^T` (instead of SVD approach).

**Command-line Arguments**:
- `args[1]`: sigma (noise level)
- `args[2]`: r (matrix rank)

**How to Run**:
```bash
./supp_simulation_different_sigma.sh          # Non-convex
./supp_simulation_different_sigma_convex.sh   # Convex
```

---

#### (8) Matrix Denoising Experiments (`matrix_denoising.R`)

**Purpose**: Compare three matrix denoising methods:
1. Matrix factorization (hard-thresholding of singular values)
2. Nuclear norm soft-thresholding
3. Random Matrix Theory (RMT) optimal shrinkage

**Settings**: Automatically scans 100 noise levels (sigma = 0.005, 0.010, ..., 0.5)

**How to Run**:
```bash
./denoising.sh
```

---

#### (9) Residual Spectrum Visualization (`residual_plot.R`)

**Purpose**: Generate visualizations of residual matrix singular value distributions, comparing with theoretical Marchenko-Pastur distributions.

**Output**: 4 PNG figures (non-convex proposed/baseline, convex proposed/baseline)

**How to Run**:
```bash
cd simulation
R --slave --vanilla < residual_plot.R
```

---

### 2.2 Real Data Experiments

#### `real_data_example/` - 4 Real Datasets

**File Structure**:
```
real_data_example/
├── data/                           # Raw data
├── realdata[1-4]_test.R            # Testing scripts (non-convex)
├── realdata[1-4]_test_convex.R     # Testing scripts (convex)
├── realdata[1-4]_residual.R        # Residual analysis (non-convex)
├── realdata[1-4]_residual_convex.R # Residual analysis (convex)
├── realdata_preprocess.R           # Data preprocessing
├── realdata_plot.R                 # Result visualization
├── case[1-4].sh                    # Execution scripts
└── case[1-4]_convex.sh
```

**How to Run**:
```bash
cd real_data_example
./case1.sh      # Run dataset 1 (non-convex)
./case1_convex.sh  # Run dataset 1 (convex)
```

---

#### `real_data_example2/` - Amazon Datasets

**File Structure**:
```
real_data_example2/
├── data/                           # Amazon data
├── amazon_data_test[1-4].R         # Testing scripts (non-convex)
├── amazon_data_test[1-4]_convex.R  # Testing scripts (convex)
├── amazon[1-4]_residual.R          # Residual analysis (non-convex)
├── amazon[1-4]_residual_convex.R   # Residual analysis (convex)
├── amazon_data_preprocess.R        # Data preprocessing
├── realdata_plot.R                 # Result visualization
├── case[1-4].sh                    # Execution scripts
└── case[1-4]_convex.sh
```

**How to Run**:
```bash
cd real_data_example2
./case1.sh      # Run Amazon dataset 1 (non-convex)
./case1_convex.sh  # Run Amazon dataset 1 (convex)
```

---

### 2.3 Output File Format

All simulation results are saved to `simulation/output/` directory. CSV file naming convention:

```
{type}_{method}_{parameters}.csv
```

- **type**: Experiment type prefix (e.g., `convex_loss`, `nonconvex_loss`, `hetero_loss`, `robust_loss`)
- **method**: `our1` (proposed method) or `baseline1` (baseline method)
- **parameters**: Experiment parameters (m_n_rho_r_sigma, etc.)

Each row contains 4 loss metrics:
1. Frobenius norm (relative)
2. Spectral norm (relative)
3. Max norm (relative)
4. Subspace distance

---

### 2.4 Dependencies

**R Packages**:
```r
library(foreach)
library(doParallel)
library(ggplot2)
library(ggbreak)      # For visualization only
library(reshape2)     # For matrix_denoising only
library(svd)          # For matrix_denoising only
```

**System Requirements**:
- R >= 3.6.0
- bash (for shell scripts)
- Multi-core CPU (for parallel computing)

---

### 2.5 Quick Start

```bash
# 1. Run a simple demo (fixed parameters: m=500, n=250, r=10, rho=20%, sigma=1)
#    This runs a single simulation and prints results to console
cd Residual_Spectral_Matching/simulation
Rscript demo.R

# 2. Run matrix denoising experiment
cd shell
./denoising.sh

# 3. Generate residual spectrum visualization
cd ../
R --slave --vanilla < residual_plot.R

# 4. Run real data example
cd ../real_data_example
./case1.sh
```

---

## License

This project is for academic research purposes.
