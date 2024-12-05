# Materials for "Matrix_Completion_via_Residual_Spectral_Matching"

## Overview
Directory <strong><em>simulation</em></strong> contains all codes, outputs and plots in the simulation.
    <ul>
        <li>The folder <strong><em>output</em></strong> contains simulation results in "<em>Section 2.2: Motivated example:matrix denoising</em>" and "<em>Section 5.1: Simulation study</em>".</li>
        <li>The folder <strong><em>plot</em></strong> contains plots in "<em>Section 5.1: Simulation study</em>".
        <li>"<strong><em>proposed_method.R</em></strong>" and "<strong><em>baseline_methods.R</em></strong>": functions to implement the algorithms presented in the paper and the classical baseline algorithms.</li>
        <li> "<strong><em>compare_to_baseline.R</em></strong>" and "<strong><em>compare_to_baseline_convex.R</em></strong>": functions to present simulations including the performance compared to baselines under different matrix sizes and selected ranks.</li>
        <li>"<strong><em>compare_different_noise.R</em></strong>" and "<strong><em>compare_different_noise_convex.R</em></strong>": functions to present simulations including the performance compared to baselines under different noise level.</li>
        <li>"<strong><em>matrix_denoising.R</em></strong>": functions to present the perforamce gap for the random matrix estimator and baselines in the matrix denoising problem.</li>
        <li>"<strong><em>results_out.R</em></strong>" and "<strong><em>residual_plot.R</em></strong>": functions to generate plots.</li>
        <li>"<strong><em>xxx.sh</em></strong>" : shell files to run the R scripts.</li>
        <li>Here also contains some auxiliary R scripts.</li>
        </li>
    </ui>
Directory <strong><em>real_data_example</em></strong> contains all codes, outputs and plots in the netflix data experiment.
    <ui>
        <li>The folder <strong><em>data</em></strong> contains preprocessed data in "<em>Section 5.2: Netflix rating data</em>".</li>
        <li>The folder <strong><em>output</em></strong> contains numerical results in "<em>Section 5.2: Netflix rating data</em>".</li>
        <li>The folder <strong><em>plot</em></strong> contains plots in "<em>Section 5.2: Netflix rating data</em>".</li>
        <li>"<strong><em>proposed_method.R</em></strong>" and "<strong><em>baseline_methods.R</em></strong>": functions to implement the algorithms presented in the paper and the classical baseline algorithms.</li>
	<li>"<strong><em>realdata_preprocess.R</em></strong>": functions for the preparation and preprocess of the real dataset.</li>
	<li>"<strong><em>realdata1_test.R</em></strong>" to "<strong><em>realdata4_test.R</em></strong>": functions to compare the performance of our rank constraint method and baselines for scenario 1 to 4.</li>
    <li>"<strong><em>realdata1_test_convex.R</em></strong>" to "<strong><em>realdata4_test_convex.R</em></strong>": functions to compare the performance of our nuclear norm method and baselines for scenario 1 to 4.</li>
    <li>"<strong><em>realdata_plot.R</em></strong>": functions to generate plots.</li>
    <li>"<strong><em>xxx.sh</em></strong>" : shell files to run the R scripts.</li>
    <li>Here also contains some auxiliary R scripts.</li>
 	</ui>
