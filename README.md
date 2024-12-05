# Materials for "Matrix_Completion_via_Residual_Spectral_Matching"

## Overview

- ### Directory ***simulation*** contains all codes, outputs and plots in the simulation.
	- The folder ***output*** contains simulation results in "*Section 2.2: Motivated example:matrix denoising*" and "*Section 5.1: Simulation study*".
	- The folder ***plot*** contains plots in "*Section 5.1: Simulation study*".
 	- "***proposed_method.R***" and "***baseline_methods.R***": functions to implement the algorithms presented in the paper and the classical baseline algorithms.
  	- "***compare_to_baseline.R***" and "***compare_to_baseline_convex.R***": functions to present simulations including the performance compared to baselines under different matrix size and selected rank.
  	- "***compare_different_noise.R***" and "***compare_different_noise_convex.R***": functions to present simulations including the performance compared to baselines under different noise level.
  	- "***matrix_denoising.R***": functions to present the perforamce gap for the random matrix estimator and baselines in the matrix denoising problem.
  	- "***results_out.R***" and "***residual_plot.R***": functions to generate plots.
  	- "***xxx.sh***" : shell files to run the R scripts.
  	- Here also contains some auxiliary R scripts.

- ### Directory ***real_data_example*** contains all codes, data, outputs and plots in the netflix data experiment.
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
- ### Directory ***real_data_example2*** contains all codes, data, outputs and plots in the amazon reviews data experiment.
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
## Workflows

 Note: Change the path and set your own relative path in the line "***setwd("your path of the folder/simulation")***" of each R script if this line exists in simulation and "***setwd("your path of the folder/real_data_example")***", "***setwd("your path of the folder/real_data_example2")***" in the Netflix data example and the Amazon reviews data example . 

- ### simulation for matrix denoising example.
	- run the shell file "***denoising.sh***" directly. 
- ### Simulation under different matrix size and different rank.
	- for the three case:*r = 5*, *r = 10* and *r = 20*, run the shell files "***simulation.sh***", "***simulation2.sh***", "***simulation3.sh***" for the results under different matrix size respectively of the rank constraint estimator and baseline.
 	- for the three case:*r = 5*, *r = 10* and *r = 20*, run the shell files "***convex_simulation.sh***", "***convex_simulation2.sh***", "***convex_simulation3.sh***" for the results under different matrix size respectively of the nuclear norm estimator and baseline.
	- for the three case:*r = 5*, *r = 10* and *r = 20*, run the shell files "***simulation_misrank.sh***", "***simulation_misrank2.sh***", "***simulation_misrank3.sh***" for the results under different selected rank respectively.
	- run the R script "***results_out.R***" to get the numerical results and performance plots.
 	- run the R script "***residual_plot.R***" to get the plots of residual matrix singular values' distributions.  
- ### Simulation under different noise level.
	- run the shell file "***simulation_different_sigma.sh***" for the results under different noise level of the rank constraint estimator and baseline.
	- run the shell file "***simulation_different_sigma_convex.sh***" for the results under different noise level of the nuclear norm estimator and baseline.
- ### Real data example: Netflix data
	- If using the original data, download the "***netflix_data.zip***" at [here](https://www.kaggle.com/datasets/netflix-inc/netflix-prize-data)) and unzip it as "***your path of the folder/real_data_example/netflix_data.csv***". Then run the R script "***realdata_preprocess.R***" to preprocess the original data. If not, omit this step. 
	- run the shell files "***case1.sh***", "***case2.sh***", "***case3.sh***", "***case4.sh***" for the results of the rank constraint estimator, baseline and their modifications in scenario 1 to 4.
 	- run the shell files "***case1_convex.sh***", "***case2_convex.sh***", "***case3_convex.sh***", "***case4_convex.sh***" for the results of the nuclear norm estimator, baseline and their modifications in scenario 1 to 4.
  	- run the shell files "***residual.sh***" for the spectral results of residual matrices in all scenarios and estimators.
  	- run the R script "***realdata_plot.R***" to get the plots of singular values' distributions. 
- ### Real data example: Amazon reviews data
	- If using the original data, download the "***Movies_and_TV.jsonl.gz***", "***Books.jsonl.gz***", "***Electronics.jsonl.gz***", "***Automotive.jsonl.gz***" at [here](https://amazon-reviews-2023.github.io) and unzip them as "***your path of the folder/real_data_example2/Movies_and_TV.jsonl/Movies_and_TV.jsonl***", "***your path of the folder/real_data_example2/Books.jsonl/Books.jsonl***", "***your path of the folder/real_data_example2/Electronics.jsonl/Electronics.jsonl***", "***your path of the folder/real_data_example2/Automotive.jsonl.jsonl/Automotive.jsonl.jsonl***". Then run the R script "***amazon_data_preprocess.R***" to preprocess the original data. If not, omit this step. 
	- run the shell files "***case1.sh***", "***case2.sh***", "***case3.sh***", "***case4.sh***" for the results of the rank constraint estimator, baseline and their modifications in scenario 1 to 4.
 	- run the shell files "***case1_convex.sh***", "***case2_convex.sh***", "***case3_convex.sh***", "***case4_convex.sh***" for the results of the nuclear norm estimator, baseline and their modifications in scenario 1 to 4.
  	- run the shell files "***residual.sh***" for the spectral results of residual matrices in all scenarios and estimators.
  	- run the R script "***realdata_plot.R***" to get the plots of singular values' distributions. 
 
