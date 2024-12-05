# Materials for "Matrix_Completion_via_Residual_Spectral_Matching"

## Overview

- Directory ***simulation*** contains all codes, outputs and plots in the simulation.
	- The folder ***output*** contains simulation results in "*Section 2.2: Motivated example:matrix denoising*" and "*Section 5.1: Simulation study*".
	- The folder ***plot*** contains plots in "*Section 5.1: Simulation study*".
 	- "***proposed_method.R***" and "***baseline_methods.R***": functions to implement the algorithms presented in the paper and the classical baseline algorithms.
  	- "***compare_to_baseline.R***" and "***compare_to_baseline_convex.R***": functions to present simulations including the performance compared to baselines under different matrix sizes and selected ranks.
  	- "***compare_different_noise.R***" and "***compare_different_noise_convex.R***": functions to present simulations including the performance compared to baselines under different noise level.
  	- "***matrix_denoising.R***": functions to present the perforamce gap for the random matrix estimator and baselines in the matrix denoising problem.
  	- "***results_out.R***" and "***residual_plot.R***": functions to generate plots.
  	- "***xxx.sh***" : shell files to run the R scripts.
  	- Here also contains some auxiliary R scripts.

- Directory ***real_data_example*** contains all codes, data, outputs and plots in the netflix data experiment.
	- The folder ***data*** contains preprocessed data in "*Section 5.2: Netflix rating data*".
 	- The folder ***output*** contains numerical results in "*Section 5.2: Netflix rating data*".
  	- The folder ***plot*** contains plots in "*Section 5.2: Netflix rating data*".
  	- "***proposed_method.R***" and "***baseline_methods.R***": functions to implement the algorithms presented in the paper and the classical baseline algorithms.
  	- "***realdata_preprocess.R***": functions for the preparation and preprocess of the real dataset.
  	- "***realdata1_test.R***" to "***realdata4_test.R***": functions to compare the performance of our rank constraint method and baselines for scenario 1 to 4.
  	-  "***realdata1_test_convex.R***" to "***realdata4_test_convex.R***": functions to compare the performance of our nuclear norm method and baselines for scenario 1 to 4.
  	-  "***realdata_plot.R***": functions to generate plots.
  	-  "***xxx.sh***" : shell files to run the R scripts.
  	-  Here also contains some auxiliary R scripts.
- Directory ***real_data_example2*** contains all codes, data, outputs and plots in the amazon reviews data experiment.
	- The folder ***data*** contains preprocessed data in "*Section 5.3: Amazon reviews data*".
	- The folder ***output*** contains numerical results in "*Section 5.3: Amazon reviews data*".
  	- The folder ***plot*** contains plots in "*Section 5.3: Amazon reviews data*".
  	- "***proposed_method.R***" and "***baseline_methods.R***": functions to implement the algorithms presented in the paper and the classical baseline algorithms.
  	- "***amazon_data_preprocess.R***": functions for the preparation and preprocess of the real dataset.
  	- "***amazon_data_test1.R***" to "***amazon_data_test4.R***": functions to compare the performance of our rank constraint method and baselines for scenario 1 to 4.
  	- "***amazon_data_test1_convex.R***" to "***amazon_data_test4_convex.R***": functions to compare the performance of our rank constraint method and baselines for scenario 1 to 4.
  	- "***realdata_plot.R***": functions to generate plots.
  	- "***xxx.sh***" : shell files to run the R scripts.
  	- Here also contains some auxiliary R scripts.

 
