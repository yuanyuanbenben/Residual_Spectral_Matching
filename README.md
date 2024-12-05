# Materials for "Matrix_Completion_via_Residual_Spectral_Matching"

## Overview
  Directory <strong><em>simulation</em></strong> contains all codes, outputs and plots in the simulation.
    <ul>
        <li>"<strong><em>proposed_method.R</em></strong>" and "<strong><em>baseline_methods.R</em></strong>": functions to implement the algorithms presented in the paper and the classical baseline algorithms.</li>
        <li> "<strong><em>compare_to_baseline.R</em></strong>" and "<strong><em>compare_to_baseline_convex.R</em></strong>": functions to present simulations including the performance compared to baselines under different matrix sizes and selected ranks.</li>
        <li>"<strong><em>compare_different_noise.R</em></strong>" and "<strong><em>compare_different_noise_convex.R</em></strong>": functions to present simulations including the performance compared to baselines under different noise level.</li>
        <li>"<strong><em>matrix_denoising.R</em></strong>": functions to present perforamce gap for random matrix estimator and baselines in the matrix denoising problem.</li>
        <li>"<strong><em>results_out.R</em></strong>" and "<strong><em>residual_plot.R</em></strong>": functions to generate plots.</li>
        <li>"<strong><em>xxx.sh</em></strong>" : shell files to run the R scripts.</li>
        <li>Some auxiliary R scripts.</li>
        <li>The folder <strong><em>output</em></strong> contains simulation results in "<em>Section 2.2: Motivated example:matrix denoising</em>" and "<em>Section 5.1: Simulation study</em>"</li>
        <li>The folder <strong><em>plot</em></strong> contains plots in "<em>Section 5.1: Simulation study</em>" and supplementary.</li>
        
        <li> The folder <strong><em>real_data</em></strong> contains codes used for "<em>Section 5: Real Data Examples</em>"
        <ul>
            <li>The folder <strong><em>netflix_data</em></strong> contains flies about the Netflix data experiment.
	        <ul>
		        <li>"<strong><em>realdata1_preprocess.R</em></strong>" and "<strong><em>realdata1_test.R</em></strong>": functions for the preparation and test processes of the real dataset.</li>
		        <li>"<strong><em>netflix_DFISTA.R</em></strong>", "<strong><em>netflix_DFISTA_link.R</em></strong>" and "<strong><em>netflix_baseline_FISTA.R</em></strong>": functions to implement the optimal algorithm used for methods presented in article for Netflix dataset.</li>
	        </ul>
            </li>
            <li>The folder <strong><em>video_data</em></strong> contains flies about the lions video in Davis 2017 data experiment.
	        <ul>
		       <li>"<strong><em>realdata2_preprocess.R</em></strong>" and "<strong><em>realdata2_test.R</em></strong>": functions for the preparation and test processes of the real dataset.</li>
		        <li>"<strong><em>cs_DFISTA.R</em></strong>" and "<strong><em>cs_baseline_FISTA.R</em></strong>": functions to implement the optimal algorithm used for methods presented in article under the dynamic compressed sensing setting.</li>
		        <li>"<strong><em>robust_pca.R</em></strong>": functions used for the preprocess of the vedio example which seperates a matrix into a low rank and a sparse part.</li>
	        </ul>
            </li>
            <li>The folder <strong><em>cifar10</em></strong> contains flies about the Cifar10 data experiment.
	        <ul>
		        <li>"<strong><em>compress.py</em></strong>": the preparetion and compressed prcesses for real data.</li>
		        <li>"<strong><em>lenet.py</em></strong>" and "<strong><em>resnet.py</em></strong>": network model used.</li>
		        <li>"<strong><em>utils.py</em></strong>":some auxiliary functions.</li>
		        <li>"<strong><em>main.py</em></strong>": main function.</li>
	        </ul>
            </li>
            <li> "<strong><em>help_functions.R</em></strong>": some auxiliary functions for above implement.</li>   
        </ul>
        </li>
        <li>"<strong><em>plot.R</em></strong>": functions to collect results in simulations and real data examples and draw the pictures used in the article.</li>
    </ul>
            
