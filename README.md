# rl_twitter
Code for 'A computational model of reward learning and habits on social media'

This repository contains three main folders.

**01-clean_data** contains scripts to clean and pre-process the datasets, and save the processed versions for use in modelling.

**02-model_fitting_and_simulation** contains the model function script (01-model_functions.R), and then scripts that use this to fit the models (02-fit_models.R), and simulate synthetic datasets from each of the models (03-simulation_generative.R). It also contains the shell script used to run the model fitting script on the cluster (04-submit_fit_models.sh).

**03-stats_and_visualisation** contains script to run all statistical analyses and produce all figures in the paper. All figures are saved in a folder within the script called 'Figures'.

For reproducibility, .renv() files in the project allow re-installation of all packages in the versions used for the paper.
The paper was made using R version 4.2.0 (2022-04-22 ucrt).
