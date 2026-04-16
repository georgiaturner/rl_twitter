# rl\_twitter

Code for 'A computational model of reward learning and habits on social media'



This repository contains three main folders.



**01-clean\_data** contains scripts to clean and pre-process the datasets, and save the processed versions for use in modelling.



**02-model\_fitting\_and\_simulation** contains the model function script (01-model\_functions.R), and then scripts that use this to fit the models (02-fit\_models.R), and simulate synthetic datasets from each of the models (03-simulation\_generative.R). It also contains the shell script used to run the model fitting script on the cluster (04-submit\_fit\_models.sh).



**03-stats\_and\_visualisation** contains script to run all statistical analyses and produce all figures in the paper. All figures are saved in a subfolder called 'figures'.

For reproducibility, .renv() files in the project allow re-installation of all packages in the versions used for the paper.


All analyses in the paper used R version 4.2.0 (2022-04-22 ucrt).

For any questions or queries, please email Georgia Turner at georgianjt@gmail.com.

